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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Abolitionist Reading of State Killing Legitimacy
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of state killing
 *   legitimacy, asserting that state killing categorically violates human
 *   dignity regardless of desert or utility. It is one reading of the
 *   'state_killing_legitimacy' kernel, alongside retributive and deterrence
 *   readings. This reading frames the condemned person as a rights-bearer
 *   (beneficiary) and the state's power to kill as inherently illegitimate
 *   (victim). The high extractiveness reflects the categorical prohibition
 *   this reading imposes on state power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.98).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Abolitionist Reading of State Killing Legitimacy").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '95f89d4f-f3cc-4343-88d3-6cee990fd5b9').
narrative_ontology:cs_kernel_codification('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', formalized).
narrative_ontology:cs_authority_grounding('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', lineage).
narrative_ontology:cs_interpretation_layer_present('95f89d4f-f3cc-4343-88d3-6cee990fd5b9').
narrative_ontology:cs_reading_relation('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', foundational, human_dignity_is_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', human_dignity_is_inalienable, deontological).
narrative_ontology:cs_axiom('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', secondary, state_power_is_limited_by_dignity).
narrative_ontology:cs_axiom_status(state_power_is_limited_by_dignity, holdable).
narrative_ontology:cs_axiom_grounding('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', state_power_is_limited_by_dignity, deontological).
narrative_ontology:cs_reference_frame('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', post_enlightenment_human_rights_framework).
narrative_ontology:cs_drift_state('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', contemporary_global_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('95f89d4f-f3cc-4343-88d3-6cee990fd5b9', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_person).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_killing_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% From the abolitionist perspective, the condemned person is the primary rights-bearer whose inherent dignity is violated by state killing, regardless of their actions. The constraint (abolition) benefits them by affirming their right to life.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_person, beneficiary,
    powerless, immediate, trapped, local).

% The state's power to execute is seen as illegitimate and inherently extractive of human dignity. The abolitionist constraint seeks to remove this power, making the state's capacity for lethal force the 'victim' of the constraint.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_killing_power, payer,
    institutional, generational, constrained, national).

% Organizations and individuals actively campaigning for the global abolition of capital punishment. They frame state killing as a fundamental human rights violation and work to shift legal and moral norms.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_advocates, agenda_setter,
    organized, generational, mobile, global).

% States that maintain capital punishment, often citing retributive justice or deterrence as justifications. They are structurally excluded from the abolitionist discourse, which views their position as morally indefensible.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retentionist_states, excluded,
    institutional, generational, constrained, national).

% International bodies and NGOs that monitor human rights compliance and advocate for the universal abolition of the death penalty, providing legal and moral arguments against state killing.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, human_rights_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a universal moral standard that prohibits state-sanctioned killing, aiming to align legal systems with a principle of inherent human dignity.
% TRANSFER_FUNCTION: Transfers the ultimate power over life and death from the state to the individual, affirming an inalienable right to life that the state cannot abrogate.
% ABSENT_VOICES: Retentionist states and proponents of retributive or deterrence-based justice are absent from the abolitionist framing, as their arguments are deemed morally invalid by the core premise of human dignity.
% DISAPPEARANCE_RATIONALE: If the abolitionist constraint vanished, the moral and legal landscape regarding capital punishment would revert to a state where state killing is more readily justified by desert or utility, potentially leading to increased executions and a diminished global human rights framework.
% FOUNDING_PROBLEM: The historical problem of states exercising arbitrary or disproportionate power over the lives of their citizens, leading to irreversible injustices and violations of fundamental human rights.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal bodies, and philosophical ethicists universally corroborate the ongoing problem of state power to kill, citing cases of wrongful execution and the inherent moral hazard of such power. This corroboration comes from outside the direct beneficiaries (condemned persons) and payers (state killing power).
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is very high (0.95) because this reading seeks to extract the ultimate power of life and death from the state, a fundamental shift in sovereign authority. Suppression is also very high (0.98) as this reading actively suppresses any justification for state killing, whether based on retribution or deterrence. Resistance is high (0.9) because this reading directly challenges deeply entrenched legal and moral traditions in many states. Theater ratio is low (0.1) as there is little performative maintenance; the constraint is a direct, active challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist perspective, the constraint is a moral imperative, not an extractive mechanism. However, from the perspective of the state's traditional power, it is a severe extraction of sovereign authority. The engine's classification as 'snare' reflects the high extractiveness and suppression inherent in this categorical prohibition, even if the abolitionist intent is morally driven.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned person is the full beneficiary (d=0.0) as their right to life is affirmed. The 'state_killing_power' is the full target (d=1.0) as this reading seeks to entirely remove its capacity for lethal force. Abolitionist advocates are agenda-setters, working to impose this constraint. Retentionist states are excluded, as their justifications are rejected by this reading's core premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_prohibition_vs_consequentialism,
    'Is the categorical prohibition on state killing (deontological) fundamentally incommensurable with consequentialist justifications (deterrence, utility)?',
    'Philosophical analysis of moral frameworks and their capacity for reconciliation or hierarchical ordering.',
    'If incommensurable, the abolitionist reading will always ''foreclose'' other readings within its own framework, leading to persistent conceptual conflict. If commensurable, a more integrated legal theory might emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_prohibition_vs_consequentialism, conceptual, 'The fundamental philosophical divide between deontological and consequentialist ethics in justifying state action.').

omega_variable(
    universal_dignity_empirical_basis,
    'Does the concept of ''universal human dignity'' have a sufficiently robust and universally accepted empirical or philosophical grounding to serve as a categorical prohibition?',
    'Cross-cultural philosophical consensus building, or empirical studies on the psychological and social effects of affirming/denying inherent dignity.',
    'A stronger grounding would increase the perceived legitimacy and ''mountain-like'' quality of the abolitionist constraint. A weaker grounding would expose it to more ''preference''-based challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_dignity_empirical_basis, empirical, 'The grounding of universal human dignity as a categorical moral principle.').


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
narrative_ontology:measurement(stat_be_t1948, state_killing_legitimacy__abolition_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(stat_be_t1970, state_killing_legitimacy__abolition_reading, base_extractiveness, 1970, 0.88).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__abolition_reading, base_extractiveness, 1990, 0.92).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__abolition_reading, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__abolition_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1948, state_killing_legitimacy__abolition_reading, suppression_requirement, 1948, 0.9).
narrative_ontology:measurement(stat_su_t1970, state_killing_legitimacy__abolition_reading, suppression_requirement, 1970, 0.92).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__abolition_reading, suppression_requirement, 1990, 0.95).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__abolition_reading, suppression_requirement, 2010, 0.97).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__abolition_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, identity_coordination).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, human_rights_law_enforcement).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, international_criminal_justice).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
