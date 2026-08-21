% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive Justice: Death for Death
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the retributive desert reading of state
 *   killing authority, asserting that murderers forfeit their right to life
 *   and that proportional punishment (lex talionis) requires death for death.
 *   It is a state-enforced norm, presented as a fundamental principle of
 *   justice. The constraint is claimed as a Tangled Rope because it purports
 *   to coordinate societal justice while enacting maximal extraction from the
 *   condemned. The metrics reflect the severe, actively enforced nature of
 *   this extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.95).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.9).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive Justice: Death for Death").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '90ed7350-c871-4176-b968-5feb34bc1f02').
narrative_ontology:cs_kernel_codification('90ed7350-c871-4176-b968-5feb34bc1f02', formalized).
narrative_ontology:cs_authority_grounding('90ed7350-c871-4176-b968-5feb34bc1f02', lineage).
narrative_ontology:cs_interpretation_layer_present('90ed7350-c871-4176-b968-5feb34bc1f02').
narrative_ontology:cs_reading_relation('90ed7350-c871-4176-b968-5feb34bc1f02', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('90ed7350-c871-4176-b968-5feb34bc1f02', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('90ed7350-c871-4176-b968-5feb34bc1f02', foundational, life_forfeitable_for_murder).
narrative_ontology:cs_axiom_status(life_forfeitable_for_murder, holdable).
narrative_ontology:cs_axiom_grounding('90ed7350-c871-4176-b968-5feb34bc1f02', life_forfeitable_for_murder, deontological).
narrative_ontology:cs_axiom('90ed7350-c871-4176-b968-5feb34bc1f02', foundational, lex_talionis_proportionality).
narrative_ontology:cs_axiom_status(lex_talionis_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('90ed7350-c871-4176-b968-5feb34bc1f02', lex_talionis_proportionality, deontological).
narrative_ontology:cs_reference_frame('90ed7350-c871-4176-b968-5feb34bc1f02', classical_retributive_justice).
narrative_ontology:cs_drift_state('90ed7350-c871-4176-b968-5feb34bc1f02', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('90ed7350-c871-4176-b968-5feb34bc1f02', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, society_seeking_justice).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victims_families).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_individuals).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportional_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces capital punishment, grounding its authority in the principle of proportional retribution for murder. It interprets and applies laws that mandate or permit the death penalty.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct targets of this constraint, forfeiting their right to life as a consequence of their crime. Their options are exhausted by legal appeals; their life is extracted by the state.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_individuals, payer,
    powerless, immediate, trapped, local).

% Receive a sense of justice, closure, or vindication from the state's execution of the murderer, consistent with the 'death for death' principle. Their benefit is emotional and symbolic, not material.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Benefits from the perceived upholding of a moral order and the principle of proportional justice, reinforcing collective values regarding the sanctity of life and the severity of murder.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, society_seeking_justice, beneficiary,
    organized, generational, constrained, national).

% Actively oppose capital punishment on moral or ethical grounds, arguing against the state's authority to take a life. While they can advocate, their arguments are structurally excluded from the retributive desert framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Examine the philosophical, legal, and societal implications of capital punishment from a detached perspective, analyzing its consistency with various ethical frameworks and its historical evolution.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for society to respond to murder with a sense of proportional justice, upholding a moral order where severe wrongs are met with commensurate consequences.
% TRANSFER_FUNCTION: Transfers the life of the condemned individual from their possession to the state, satisfying the principle of proportional justice and providing a sense of vindication to victims' families and society.
% ABSENT_VOICES: The condemned, once sentenced, are largely excluded from the discourse regarding the justice of their punishment. Abolitionist voices, while present, are often marginalized within the retributive framework.
% DISAPPEARANCE_RATIONALE: If the state's authority to execute for retribution vanished overnight, the entire criminal justice system's sentencing philosophy would need to be fundamentally re-evaluated, leading to significant societal and legal restructuring regarding how murder is punished and how justice is defined.
% FOUNDING_PROBLEM: How to justly respond to the ultimate crime (murder) and uphold a moral order where severe wrongs are met with proportional consequences, ensuring that the punishment 'fits the crime'.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this view cite ancient legal codes (e.g., Code of Hammurabi), philosophical traditions (e.g., Kant's categorical imperative), and historical legal precedents. Opponents (abolitionists, some legal scholars) dispute its moral validity or practical application, arguing that the founding problem is either misidentified or better solved by other means; this contestation is widely documented in legal and philosophical discourse.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is maximal (0.95) as it involves the taking of a life. Suppression is very high (0.90) due to the state's monopoly on legitimate force and the finality of execution. Theater ratio is low (0.20) because the act of execution is a direct, unambiguous enforcement of the stated principle, with little performative excess beyond the act itself. Accessibility collapse is near total for the condemned (0.95). Resistance is substantial (0.65) from abolitionist movements and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of retributive desert view this as a necessary and just coordination mechanism for societal order. Those targeted by it, and abolitionist observers, experience it as pure, irreversible extraction. The engine's classification will highlight this divergence between the claimed coordination function and the measured extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system acts as the agenda setter, enforcing the constraint. Condemned individuals are the ultimate payers, losing their lives. Victims' families and society seeking justice are beneficiaries, receiving a symbolic vindication. Abolitionist advocates are excluded, as their core premise is incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_ambiguity,
    'What constitutes ''proportional'' punishment, and is ''death for death'' the only or most appropriate interpretation of lex talionis?',
    'Ongoing philosophical debate, evolving societal norms, and judicial interpretation regarding the meaning of justice and proportionality in sentencing.',
    'If ''proportional'' is interpreted more broadly (e.g., life imprisonment), the constraint''s extractiveness would decrease, potentially shifting its classification towards a less extractive type or even dissolving it as a capital punishment constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_ambiguity, conceptual, 'Ambiguity in the interpretation of proportional justice.').

omega_variable(
    forfeiture_of_rights_ambiguity,
    'Is the ''right to life'' truly forfeitable by criminal act, or is it an inalienable right that the state cannot legitimately extinguish?',
    'Resolution of fundamental philosophical and legal debates regarding natural rights, state authority, and the limits of punishment.',
    'If the right to life is deemed inalienable, the foundational premise of this reading collapses, rendering the state''s action illegitimate and reclassifying the constraint as a Snare or even a Mountain (if inalienability is a natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forfeiture_of_rights_ambiguity, conceptual, 'Contestation over the alienability of the right to life.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal force) or internalized (societal acceptance of the principle)?',
    'Post-abolition societal trajectory: if the principle of ''death for death'' persists as a strong moral demand even after legal abolition, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the societal demand for retribution persists even without legal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for retributive justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.2).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.2).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.2).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.2).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'state_killing_authority' kernel. Each reading presents a different justification and structural profile for capital punishment, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
