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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: State Killing Legitimacy (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint describes the justification for state killing based on
 *   the retributive principle of proportional desert (lex talionis), where a
 *   murderer is deemed to forfeit their life-right. It is one reading of the
 *   broader 'state_killing_legitimacy' kernel. The constraint operates with
 *   extremely high extraction and suppression, as it involves the state
 *   taking a life. The claimed type is Snare because the coordination story
 *   (maintaining moral order) serves as a justification for what is
 *   fundamentally an act of pure extraction from the condemned offender.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.92).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.95).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Killing Legitimacy (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9').
narrative_ontology:cs_kernel_codification('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', formalized).
narrative_ontology:cs_authority_grounding('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', lineage).
narrative_ontology:cs_interpretation_layer_present('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9').
narrative_ontology:cs_reading_relation('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', foundational, proportional_desert_principle).
narrative_ontology:cs_axiom_status(proportional_desert_principle, holdable).
narrative_ontology:cs_axiom_grounding('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', proportional_desert_principle, deontological).
narrative_ontology:cs_reference_frame('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', lex_talionis_principle).
narrative_ontology:cs_drift_state('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8b7922ae-0f0d-4b40-87d2-c5a2c4a139e9', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, society_at_large).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_offender).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, moral_order_restoration).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces capital punishment, interpreting and applying laws based on the principle of proportional desert. It holds the monopoly on legitimate force to carry out executions.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Bears the ultimate cost of the constraint, forfeiting their life as a proportional consequence of their crime. All legal avenues for appeal and clemency have been exhausted, leaving no exit.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_offender, payer,
    powerless, immediate, trapped, local).

% Benefits from the perceived restoration of moral order and justice, believing that the punishment fits the crime and reinforces societal norms against murder. This provides a sense of collective satisfaction and security.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% May experience a sense of closure, justice, or retribution through the execution of the offender, believing that the state has adequately avenged the wrong committed against their loved one.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Actively campaign against capital punishment, arguing that it violates fundamental human rights regardless of desert. Their arguments are often framed as opposing justice by proponents of retribution, effectively excluding them from the core debate on the *legitimacy* of desert-based killing.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Analyze the philosophical, legal, and ethical justifications for capital punishment, including the retributive principle. They provide critical commentary and contribute to the ongoing intellectual debate without directly participating in the enforcement or suffering its consequences.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains a perceived moral order by ensuring that those who commit heinous crimes receive proportional punishment, thereby reinforcing societal norms against murder and providing a framework for justice.
% TRANSFER_FUNCTION: Transfers the life and liberty of the condemned offender to the state, in exchange for the perceived restoration of moral balance and justice for society and the victims' families.
% ABSENT_VOICES: Abolitionist advocates are often excluded from the core debate on the *justification* for capital punishment, instead being framed as opposing justice itself. Their arguments for inherent human dignity and against state violence are sidelined in favor of desert-based arguments.
% DISAPPEARANCE_RATIONALE: If the principle of proportional desert for murder vanished overnight, the legitimacy of capital punishment would collapse, leading to a fundamental re-evaluation of criminal justice, sentencing, and the state's role in retribution. The entire legal and moral framework for severe punishment would need to be rebuilt.
% FOUNDING_PROBLEM: To establish a system of justice that ensures heinous crimes, particularly murder, are met with a punishment that is morally proportional to the offense, thereby upholding the sanctity of life and the moral order of society.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, often including victims' families and a segment of the public, attest that the problem of ensuring proportional justice for murder remains live. Legal scholars and philosophers debate its application, but the underlying retributive impulse is widely acknowledged as a persistent societal demand, even by those who oppose capital punishment on other grounds.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is near maximal (0.92) because the constraint involves the ultimate forfeiture of life. Suppression is also near maximal (0.95) due to the state's monopoly on legitimate force and the complete collapse of alternatives for the condemned. Theater ratio is low (0.10) as the act of execution is a direct, functional application of the constraint, not a performative maintenance of an atrophied function. Resistance is moderate (0.60) due to ongoing, organized abolitionist movements, but the state's authority to apply this principle is still accepted by a significant portion of the population in jurisdictions where it is practiced.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the condemned, this is pure, inescapable extraction. From the perspective of society and victims' families, it is a necessary act of justice that restores moral balance. The engine's classification as a Snare reflects the structural reality of extraction, while acknowledging the powerful moral justification claimed by its beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system acts as the agenda-setter, enforcing the principle. The condemned offender is the clear target and payer, bearing the full cost. Society at large and victims' families are beneficiaries, gaining a perceived sense of justice and moral order. Abolitionist advocates are excluded, as their fundamental opposition to state killing is often framed as outside the bounds of the retributive debate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retributive_vs_deterrence_primacy,
    'Is the primary justification for state killing truly retribution, or is deterrence the underlying (perhaps unacknowledged) driver, even when retributive arguments are foregrounded?',
    'Analysis of legislative debates, judicial opinions, and public discourse for explicit or implicit reliance on deterrent effects, even when retribution is stated as the primary goal.',
    'If deterrence is the true primary driver, the constraint''s classification might shift towards a Tangled Rope (coordination for public safety with extraction), or even a Snare if the deterrent effect is empirically weak or non-existent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_vs_deterrence_primacy, conceptual, 'Ambiguity in the primary justification for capital punishment.').

omega_variable(
    proportionality_definition_ambiguity,
    'What constitutes ''proportional'' desert, and who defines it? Is it truly lex talionis (an eye for an eye), or a more abstract concept of justice that allows for varying interpretations and applications?',
    'Comparative legal analysis across jurisdictions and historical periods, examining how ''proportionality'' has been defined and applied in practice, and the philosophical arguments underpinning these definitions.',
    'If proportionality is highly subjective or inconsistently applied, the claim of ''just desert'' as a coordination function weakens, potentially increasing the perceived extractiveness and reducing the legitimacy of the Snare''s justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_definition_ambiguity, conceptual, 'Ambiguity in the definition and application of proportional desert.').

omega_variable(
    moral_order_as_beneficiary_ambiguity,
    'Is ''moral order'' a genuine beneficiary that collects real value, or is it a rhetorical cover for state power and societal demands for vengeance, which are themselves extractive?',
    'Sociological studies on the actual impact of capital punishment on societal moral norms and crime rates, contrasted with public opinion surveys on satisfaction with justice outcomes.',
    'If ''moral order'' is primarily a rhetorical device, the constraint''s coordination function is further undermined, reinforcing its Snare classification and highlighting the pure extractive nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_order_as_beneficiary_ambiguity, empirical, 'Whether ''moral order'' is a genuine beneficiary or a rhetorical cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__retributive_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__retributive_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__retributive_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__retributive_reading, base_extractiveness, 10, 0.91).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__retributive_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__retributive_reading, base_extractiveness, 30, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.94).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__retributive_reading, suppression_requirement, 10, 0.95).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__retributive_reading, suppression_requirement, 20, 0.95).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__retributive_reading, suppression_requirement, 30, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_legitimacy' kernel, each representing a distinct justification or rejection of state killing. They are linked to capture the contested nature of the underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
