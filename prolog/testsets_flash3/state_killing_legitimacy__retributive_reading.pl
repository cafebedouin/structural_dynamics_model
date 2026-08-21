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
 *   This constraint represents the retributive reading of state killing
 *   legitimacy, where a murderer is seen to forfeit their life-right through
 *   proportional desert (lex talionis). It is a reading of the
 *   'state_killing_legitimacy' kernel, distinct from deterrence or
 *   abolitionist readings. The constraint is classified as a Snare due to its
 *   high extractiveness (the ultimate forfeiture of life) and suppression
 *   (the state's monopoly on violence and legal process). The 'moral_order'
 *   and 'society_as_a_whole' are declared as beneficiaries, reflecting the
 *   perceived restoration of justice and affirmation of moral principles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.85).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.9).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Killing Legitimacy (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, 'e52206cf-fb80-4fa4-89c6-aa0202337b7b').
narrative_ontology:cs_kernel_codification('e52206cf-fb80-4fa4-89c6-aa0202337b7b', formalized).
narrative_ontology:cs_authority_grounding('e52206cf-fb80-4fa4-89c6-aa0202337b7b', lineage).
narrative_ontology:cs_interpretation_layer_present('e52206cf-fb80-4fa4-89c6-aa0202337b7b').
narrative_ontology:cs_reading_relation('e52206cf-fb80-4fa4-89c6-aa0202337b7b', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e52206cf-fb80-4fa4-89c6-aa0202337b7b', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('e52206cf-fb80-4fa4-89c6-aa0202337b7b', foundational, life_forfeit_by_murder).
narrative_ontology:cs_axiom_status(life_forfeit_by_murder, holdable).
narrative_ontology:cs_axiom_grounding('e52206cf-fb80-4fa4-89c6-aa0202337b7b', life_forfeit_by_murder, deontological).
narrative_ontology:cs_axiom('e52206cf-fb80-4fa4-89c6-aa0202337b7b', foundational, proportional_desert_is_justice).
narrative_ontology:cs_axiom_status(proportional_desert_is_justice, holdable).
narrative_ontology:cs_axiom_grounding('e52206cf-fb80-4fa4-89c6-aa0202337b7b', proportional_desert_is_justice, deontological).
narrative_ontology:cs_reference_frame('e52206cf-fb80-4fa4-89c6-aa0202337b7b', lex_talionis_principle).
narrative_ontology:cs_drift_state('e52206cf-fb80-4fa4-89c6-aa0202337b7b', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e52206cf-fb80-4fa4-89c6-aa0202337b7b', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, society_as_a_whole).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces capital punishment, interpreting and applying laws based on retributive principles. Its legitimacy is partly derived from its perceived ability to deliver justice for heinous crimes.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct targets of the constraint, facing the forfeiture of their life-right. Their agency is entirely suppressed within the system once convicted and sentenced to death.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderers, payer,
    powerless, immediate, trapped, local).

% Benefits from the perceived restoration of balance and justice when a life is taken for a life. This abstract entity represents the normative framework that the retributive reading seeks to uphold.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order).

% Benefits from the satisfaction of justice and the affirmation of moral principles. This includes victims' families and the broader public who believe in 'just deserts' for severe crimes.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, society_as_a_whole, beneficiary,
    organized, generational, constrained, national).

% Argue against capital punishment on moral grounds, asserting that state killing is inherently wrong regardless of desert. They are excluded from the retributive framework's internal logic but exert external pressure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Analyze the efficacy of capital punishment in preventing future crimes. While their focus is utilitarian, their findings can influence the political viability of capital punishment, even if not directly part of the retributive justification.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, deterrence_theorists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates society's response to murder by establishing a clear, proportional consequence that aligns with a sense of moral justice, thereby affirming the value of human life and the gravity of its violation.
% TRANSFER_FUNCTION: Transfers the life-right from the convicted murderer to the state, in proportion to the life taken, as a form of moral balancing or 'just deserts'.
% ABSENT_VOICES: Abolitionist advocates are structurally excluded from the retributive framework, as their core premise (state killing is always wrong) directly contradicts the desert-based justification. Their arguments are treated as external moral claims, not internal critiques of proportionality.
% DISAPPEARANCE_RATIONALE: If the retributive justification for state killing vanished, the entire legal and moral framework for capital punishment would collapse. Society would be forced to find alternative justifications (e.g., pure deterrence, incapacitation) or abandon the practice entirely, leading to a fundamental reorganization of criminal justice and moral philosophy regarding severe crimes.
% FOUNDING_PROBLEM: The problem of how to justly respond to the ultimate violation of human life (murder) in a way that upholds moral order and provides proportional punishment.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, victims' families, and a segment of the public attest that the problem of justly punishing murder remains live and that retributive justice is the appropriate response. This is corroborated by historical legal traditions and philosophical arguments for lex talionis, independent of the state judicial system's self-justification.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the constraint demands the ultimate cost (life) from the convicted. Suppression is also very high (0.90) as the state holds a monopoly on legitimate force and the legal process offers no true 'exit' for the condemned. Theater ratio is low (0.10) because, within this reading, the act of execution is seen as a direct, functional fulfillment of justice, not a performance masking atrophy. Resistance is high (0.70) due to ongoing moral and legal challenges from abolitionist movements, but this resistance does not fundamentally alter the retributive logic itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the convicted murderer, the constraint is a pure Snare, an inescapable mechanism of extraction. From the perspective of the state judicial system and society, it is a legitimate act of justice, framed as a necessary function of upholding moral order. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Convicted murderers are the full targets (d=1.0) as they bear the ultimate cost. The state judicial system, as the enforcer, and 'society as a whole' are beneficiaries (d near 0.0-0.15) as they gain the perceived restoration of justice. The abstract 'moral_order' is also a beneficiary, representing the normative framework upheld. Abolitionist advocates are excluded, as their arguments fundamentally challenge the premise of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_desert_objectivity,
    'Is ''moral desert'' an objective, universally applicable principle, or a culturally contingent construct?',
    'Cross-cultural philosophical analysis and empirical study of moral intuitions across diverse societies. If found to be purely contingent, the foundational axiom of this reading is weakened.',
    'If desert is purely contingent, the constraint''s claim to universal legitimacy is undermined, potentially reclassifying it as a more localized or culturally specific Snare, rather than a universally applicable one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_desert_objectivity, conceptual, 'Ambiguity regarding the objectivity of moral desert as a justification for state killing.').

omega_variable(
    proportionality_measurement,
    'How is ''proportionality'' (lex talionis) objectively measured when the ''life-for-a-life'' principle is applied, given the inherent asymmetry of taking a life?',
    'Development of a universally accepted, non-arbitrary metric for ''proportionality'' in capital cases, or a demonstration that such a metric is impossible.',
    'If proportionality cannot be objectively measured, the constraint''s claim to ''just'' extraction is undermined, potentially exposing it as an arbitrary Snare rather than a precisely calibrated one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_measurement, empirical, 'The challenge of objectively measuring ''proportionality'' in the context of lex talionis.').

omega_variable(
    mandate_drift_from_justice_to_vengeance,
    'Has the operational mandate of capital punishment, even under a retributive reading, drifted from ''justice'' to ''vengeance'' in practice?',
    'Sociological and psychological studies of public and judicial motivations in capital cases, comparing stated retributive goals with actual emotional and social drivers.',
    'If the primary driver is found to be vengeance rather than justice, the constraint''s claimed coordination function (upholding moral order) is undermined, potentially reclassifying it as a more purely extractive Snare with a theatrical justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_drift_from_justice_to_vengeance, empirical, 'Whether the practice of capital punishment under a retributive reading has drifted from justice to vengeance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__retributive_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__retributive_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__retributive_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__retributive_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__retributive_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__retributive_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__retributive_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__retributive_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__retributive_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__retributive_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__retributive_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__retributive_reading, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__retributive_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__retributive_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
