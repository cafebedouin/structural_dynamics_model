% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: State Killing Legitimacy (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint describes the justification for state killing based on
 *   the retributive principle of proportional desert (lex talionis). The
 *   murderer is seen as forfeiting their life-right, and the state's action
 *   is legitimized as restoring a moral balance. This is one reading of the
 *   broader 'state_killing_legitimacy' kernel, distinct from deterrence or
 *   abolitionist arguments. The constraint is claimed as a Snare due to the
 *   high extraction from the convicted individual and the active suppression
 *   required to enforce it, despite the moral justification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.85).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.95).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Killing Legitimacy (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '08df588e-6cff-45dc-8192-c486a83898fe').
narrative_ontology:cs_kernel_codification('08df588e-6cff-45dc-8192-c486a83898fe', formalized).
narrative_ontology:cs_authority_grounding('08df588e-6cff-45dc-8192-c486a83898fe', lineage).
narrative_ontology:cs_interpretation_layer_present('08df588e-6cff-45dc-8192-c486a83898fe').
narrative_ontology:cs_reading_relation('08df588e-6cff-45dc-8192-c486a83898fe', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('08df588e-6cff-45dc-8192-c486a83898fe', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('08df588e-6cff-45dc-8192-c486a83898fe', foundational, proportional_desert_is_just).
narrative_ontology:cs_axiom_status(proportional_desert_is_just, holdable).
narrative_ontology:cs_axiom_grounding('08df588e-6cff-45dc-8192-c486a83898fe', proportional_desert_is_just, deontological).
narrative_ontology:cs_axiom('08df588e-6cff-45dc-8192-c486a83898fe', foundational, murderer_forfeits_life_right).
narrative_ontology:cs_axiom_status(murderer_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('08df588e-6cff-45dc-8192-c486a83898fe', murderer_forfeits_life_right, deontological).
narrative_ontology:cs_reference_frame('08df588e-6cff-45dc-8192-c486a83898fe', lex_talionis_principle).
narrative_ontology:cs_drift_state('08df588e-6cff-45dc-8192-c486a83898fe', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08df588e-6cff-45dc-8192-c486a83898fe', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, society_at_large).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, including trials, appeals, and execution protocols. Justifies its actions by upholding the moral order and ensuring proportional justice.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% The individual upon whom the death penalty is imposed. Forfeits their life-right as a consequence of their crime, according to the principle of lex talionis. Has no exit from the constraint once convicted and sentenced.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderer, payer,
    powerless, immediate, trapped, local).

% Benefits from the perceived restoration of moral balance and the affirmation of justice. The constraint is seen as upholding fundamental societal values and responding appropriately to heinous crimes.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% The abstract concept of justice and moral balance that is purportedly restored or upheld by the application of proportional punishment. This is the ultimate beneficiary in the retributive framework.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order).

% Argue against capital punishment on grounds of human dignity and the state's moral limits, regardless of desert. Their arguments are often dismissed or marginalized within the retributive framework, which prioritizes proportional justice.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, proportional response to the crime of murder, coordinating societal understanding of justice and the state's role in upholding it.
% TRANSFER_FUNCTION: Transfers the life of the convicted murderer to the state, in exchange for the perceived restoration of moral balance and the affirmation of justice for society.
% ABSENT_VOICES: Abolitionist advocates are excluded from the core retributive logic, as their arguments about inherent human dignity are deemed irrelevant to the principle of proportional desert. Their voices would challenge the very premise of the state's right to take a life.
% DISAPPEARANCE_RATIONALE: If the retributive justification for state killing vanished, the entire legal and moral framework for capital punishment would collapse. The state would lose a key mechanism for responding to murder, and society would have to fundamentally re-evaluate its understanding of justice and punishment.
% FOUNDING_PROBLEM: The problem of how to justly respond to the ultimate crime (murder) in a way that upholds the moral order and provides proportional desert to the offender.
% FOUNDING_PROBLEM_CORROBORATION: The state judicial system and proponents of capital punishment attest that the problem of proportional justice for murder remains live. While abolitionist groups contest the solution, the underlying philosophical problem of desert is widely acknowledged in legal and moral philosophy.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint literally extracts the life of the individual. Suppression is very high (0.95) as it involves the full coercive power of the state to enforce the sentence, with no viable exit for the convicted. Theater ratio is low (0.1) because the primary function (execution) is carried out directly, with little performative overhead beyond legal process. Resistance is substantial (0.7) from abolitionist movements and legal challenges, but the constraint persists where the retributive reading holds sway.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the convicted murderer, the constraint is a pure Snare, an inescapable extraction of their life. From the perspective of the state and society, it is framed as a just and necessary act to uphold the moral order, potentially even a form of coordination around shared values of justice. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The convicted murderer is the ultimate target (d=1.0), bearing the full cost. Society at large and the abstract 'moral order' are the beneficiaries (d=0.0-0.1), as they are seen to gain from the restoration of justice. The state judicial system acts as the agenda-setter, administering the process. Abolitionist advocates are excluded, as their arguments are outside the retributive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_desert_objectivity,
    'Is the concept of ''proportional desert'' an objective moral truth, or a socially constructed and culturally contingent value?',
    'Cross-cultural and historical analysis of justice systems, philosophical consensus on meta-ethics, and the stability of ''desert'' claims under varying social conditions.',
    'If objective, the constraint''s moral legitimacy is strengthened, potentially pushing its classification closer to a Mountain of moral law. If constructed, its persistence relies more heavily on active enforcement and cultural consensus, reinforcing its Snare-like qualities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_desert_objectivity, conceptual, 'Ambiguity regarding the objective vs. constructed nature of proportional moral desert.').

omega_variable(
    retribution_vs_revenge,
    'Does the state''s application of lex talionis genuinely constitute ''retribution'' (impersonal justice), or does it functionally operate as ''revenge'' (personal vengeance institutionalized)?',
    'Analysis of judicial process impartiality, public discourse framing, and the emotional content of victim impact statements vs. legal judgments. If the process is consistently driven by emotional satisfaction rather than abstract justice, it leans towards revenge.',
    'If it''s functionally revenge, the ''moral order'' beneficiary becomes a cover story, increasing the effective extractiveness and solidifying the Snare classification. If it''s genuinely impersonal retribution, the coordination function around justice is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retribution_vs_revenge, empirical, 'Distinction between impersonal retribution and institutionalized revenge.').

omega_variable(
    founding_problem_corroboration_strength,
    'How robust is the corroboration for the ''founding problem'' (just response to murder) from sources outside the direct beneficiaries of capital punishment?',
    'Systematic review of independent philosophical arguments, international legal scholarship, and historical records that acknowledge the problem of desert without necessarily endorsing capital punishment as the solution.',
    'Strong external corroboration would lend more weight to the ''live'' status of the founding problem, supporting the idea that the constraint addresses a genuine, enduring challenge. Weak corroboration would suggest the ''live'' status is primarily self-serving for proponents, increasing the likelihood of a Mandatrophy signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_corroboration_strength, empirical, 'Strength of external corroboration for the founding problem''s continued relevance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1900, state_killing_legitimacy__retributive_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(stat_tr_t1950, state_killing_legitimacy__retributive_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(stat_tr_t2000, state_killing_legitimacy__retributive_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__retributive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1900, state_killing_legitimacy__retributive_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(stat_be_t1950, state_killing_legitimacy__retributive_reading, base_extractiveness, 1950, 0.85).
narrative_ontology:measurement(stat_be_t2000, state_killing_legitimacy__retributive_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__retributive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1900, state_killing_legitimacy__retributive_reading, suppression_requirement, 1900, 0.9).
narrative_ontology:measurement(stat_su_t1950, state_killing_legitimacy__retributive_reading, suppression_requirement, 1950, 0.92).
narrative_ontology:measurement(stat_su_t2000, state_killing_legitimacy__retributive_reading, suppression_requirement, 2000, 0.97).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__retributive_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_legitimacy' kernel, focusing on retributive justice. It is linked to deterrence and abolitionist readings, which offer alternative justifications or rejections of state killing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
