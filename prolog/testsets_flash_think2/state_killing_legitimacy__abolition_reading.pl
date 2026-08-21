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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: State Killing as Categorical Violation of Human Dignity (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'abolition_reading' of the
 *   'state_killing_legitimacy' kernel. From this perspective, state killing
 *   is understood as a categorical violation of human dignity, regardless of
 *   any claims of desert or utility. The constraint itself is the inherent
 *   extractive nature of this violation. The high extractiveness (0.9) and
 *   suppression (0.95) reflect the ultimate nature of taking a life, which is
 *   seen as an irreducible extraction from the condemned person. The claimed
 *   type is 'snare' because any coordination story (e.g., justice,
 *   deterrence) is viewed as a cover for this fundamental extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.9).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "State Killing as Categorical Violation of Human Dignity (Abolitionist Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, 'e9865355-3b78-45e2-81d8-05ef4237beaf').
narrative_ontology:cs_kernel_codification('e9865355-3b78-45e2-81d8-05ef4237beaf', formalized).
narrative_ontology:cs_authority_grounding('e9865355-3b78-45e2-81d8-05ef4237beaf', lineage).
narrative_ontology:cs_interpretation_layer_present('e9865355-3b78-45e2-81d8-05ef4237beaf').
narrative_ontology:cs_reading_relation('e9865355-3b78-45e2-81d8-05ef4237beaf', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('e9865355-3b78-45e2-81d8-05ef4237beaf', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('e9865355-3b78-45e2-81d8-05ef4237beaf', foundational, human_dignity_absolute).
narrative_ontology:cs_axiom_status(human_dignity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e9865355-3b78-45e2-81d8-05ef4237beaf', human_dignity_absolute, deontological).
narrative_ontology:cs_axiom('e9865355-3b78-45e2-81d8-05ef4237beaf', secondary, state_power_subordinate_to_rights).
narrative_ontology:cs_axiom_status(state_power_subordinate_to_rights, holdable).
narrative_ontology:cs_axiom_grounding('e9865355-3b78-45e2-81d8-05ef4237beaf', state_power_subordinate_to_rights, deontological).
narrative_ontology:cs_reference_frame('e9865355-3b78-45e2-81d8-05ef4237beaf', universal_human_rights_framework).
narrative_ontology:cs_drift_state('e9865355-3b78-45e2-81d8-05ef4237beaf', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e9865355-3b78-45e2-81d8-05ef4237beaf', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, state_judicial_system).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_person).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, pro_death_penalty_advocates).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, abolitionist_advocates).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the ultimate cost of the state's power to kill, experiencing the categorical violation of their human dignity with no possibility of exit or appeal to a higher authority within the system.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_person, payer,
    powerless, immediate, trapped, local).

% Administers and enforces the power of state killing, asserting its authority and perceived right to inflict ultimate punishment. From this reading, it benefits from exercising a power that categorically violates human dignity.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Actively resists state killing on moral and philosophical grounds, bearing costs in advocacy, legal challenges, and moral struggle. They seek to dismantle the system that perpetrates this categorical violation.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_advocates, payer,
    organized, biographical, mobile, global).

% Benefits from the perceived justice, retribution, or deterrence offered by state killing, supporting its continuation. They uphold the state's right to inflict this punishment.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, pro_death_penalty_advocates, beneficiary,
    organized, biographical, mobile, national).

% Bears the moral and societal cost of state killing, often feeling constrained by perceived needs for justice or safety, which can lead to complicity in the violation of human dignity.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, general_public, payer,
    moderate, biographical, constrained, national).

% Monitors and critiques state killing from a universal human rights perspective, advocating for its abolition and highlighting its categorical violation of human dignity. They provide an external analytical lens.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this abolitionist reading, the constraint (the categorical violation of human dignity by state killing) has no legitimate coordination function. Any claimed coordination (e.g., justice, deterrence) is seen as a cover for the inherent extraction.
% TRANSFER_FUNCTION: Transfers life and inherent human dignity from the condemned person to the state's assertion of ultimate power and its perceived right to inflict punishment.
% ABSENT_VOICES: The executed individuals, whose voices are permanently silenced. Also, future generations who might inherit a legal and moral system that has normalized such a categorical violation, thereby implicitly devaluing human dignity.
% DISAPPEARANCE_RATIONALE: If the categorical violation of human dignity by state killing were universally acknowledged and ceased overnight, legal systems, moral frameworks, and the very understanding of state power would fundamentally shift. Societies would need to re-evaluate punishment, justice, and human rights, leading to a profound reordering of justice systems and moral principles globally.
% FOUNDING_PROBLEM: The problem of how societies respond to heinous crimes, assert state authority, and maintain order, often framed as achieving justice or ensuring public safety.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, philosophical ethics, and abolitionist movements corroborate the view that state killing is a categorical violation. Conversely, proponents of capital punishment attest to its necessity for justice, retribution, or deterrence, framing it as a legitimate response to the founding problem.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness reflects the ultimate and irreversible nature of state killing, which from this reading, is an absolute taking of life and dignity. Suppression is maximal because the condemned person is rendered utterly powerless. Theater ratio is low as the act is direct and its consequences are undeniable. Accessibility collapse is high for the condemned, as there is no alternative to death. Resistance is high due to ongoing global abolitionist movements. The measurements are held constant over the interval (representing 1948-2023) to reflect the unchanging nature of the categorical violation itself, even as societal views and legal frameworks evolve.
 *
 * PERSPECTIVAL GAP:
 *   The state judicial system and pro-death penalty advocates perceive state killing as a legitimate exercise of justice or deterrence, while abolitionist advocates and international human rights bodies perceive it as a fundamental violation. The engine's classification will highlight this divergence, showing the state's exercise of power as a snare from the abolitionist seat, despite claims of justice or utility from other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned person is the primary target (payer) of this constraint, bearing the full weight of the extraction. The state judicial system is the primary beneficiary (agenda_setter), as it exercises and benefits from this ultimate power. Abolitionist advocates are payers, expending resources to resist the constraint. Pro-death penalty advocates are beneficiaries, supporting the system. The general public is a payer of moral costs, while international human rights bodies serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_contingent_dignity,
    'Is human dignity an absolute, inalienable quality that cannot be forfeited, or is it contingent on an individual''s actions or societal recognition?',
    'Philosophical consensus on the foundations of human rights, or a shift in international legal norms that explicitly rejects conditional dignity.',
    'If dignity is universally accepted as absolute, the abolitionist reading''s claim of categorical violation is strengthened, making the constraint''s extractiveness undeniable. If dignity is contingent, the justification for state killing based on desert or utility gains conceptual ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_contingent_dignity, conceptual, 'Ambiguity regarding the absolute or contingent nature of human dignity.').

omega_variable(
    state_power_limits_scope,
    'What are the ultimate, non-negotiable limits of state power, particularly concerning the right to life, and are these limits derived from inherent rights or social contract?',
    'Global legal precedent establishing a jus cogens norm against state killing, or a widespread philosophical agreement on the inherent limits of state authority over individual life.',
    'If state power is universally acknowledged to be categorically limited by the right to life, the abolitionist reading becomes a foundational principle. If state power is seen as having legitimate exceptions to this limit, the constraint''s classification as a snare becomes more contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_power_limits_scope, conceptual, 'Ambiguity regarding the inherent limits of state power over individual life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t15, state_killing_legitimacy__abolition_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__abolition_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(stat_tr_t45, state_killing_legitimacy__abolition_reading, theater_ratio, 45, 0.1).
narrative_ontology:measurement(stat_tr_t60, state_killing_legitimacy__abolition_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(stat_tr_t75, state_killing_legitimacy__abolition_reading, theater_ratio, 75, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(stat_be_t15, state_killing_legitimacy__abolition_reading, base_extractiveness, 15, 0.9).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__abolition_reading, base_extractiveness, 30, 0.9).
narrative_ontology:measurement(stat_be_t45, state_killing_legitimacy__abolition_reading, base_extractiveness, 45, 0.9).
narrative_ontology:measurement(stat_be_t60, state_killing_legitimacy__abolition_reading, base_extractiveness, 60, 0.9).
narrative_ontology:measurement(stat_be_t75, state_killing_legitimacy__abolition_reading, base_extractiveness, 75, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(stat_su_t15, state_killing_legitimacy__abolition_reading, suppression_requirement, 15, 0.95).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__abolition_reading, suppression_requirement, 30, 0.95).
narrative_ontology:measurement(stat_su_t45, state_killing_legitimacy__abolition_reading, suppression_requirement, 45, 0.95).
narrative_ontology:measurement(stat_su_t60, state_killing_legitimacy__abolition_reading, suppression_requirement, 60, 0.95).
narrative_ontology:measurement(stat_su_t75, state_killing_legitimacy__abolition_reading, suppression_requirement, 75, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_legitimacy' kernel, alongside 'retributive_reading' and 'deterrence_reading'. Each reading presents a distinct structural claim about state killing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
