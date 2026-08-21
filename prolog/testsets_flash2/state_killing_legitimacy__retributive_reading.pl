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
 *   This constraint models the legitimacy of state killing (capital
 *   punishment) through a retributive lens, where a murderer forfeits their
 *   life-right due to proportional desert (lex talionis). It is one reading
 *   of the broader 'state_killing_legitimacy' kernel. The constraint is
 *   claimed as a Tangled Rope because it purports to coordinate societal
 *   justice while extracting the ultimate cost (life) from the convicted,
 *   requiring active enforcement and facing significant resistance. The
 *   metrics reflect high extraction and suppression inherent in a system that
 *   takes a life.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.85).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.9).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "State Killing Legitimacy (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '464b8e30-186a-4e1a-9db1-d5cbd709c657').
narrative_ontology:cs_kernel_codification('464b8e30-186a-4e1a-9db1-d5cbd709c657', formalized).
narrative_ontology:cs_authority_grounding('464b8e30-186a-4e1a-9db1-d5cbd709c657', lineage).
narrative_ontology:cs_interpretation_layer_present('464b8e30-186a-4e1a-9db1-d5cbd709c657').
narrative_ontology:cs_reading_relation('464b8e30-186a-4e1a-9db1-d5cbd709c657', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('464b8e30-186a-4e1a-9db1-d5cbd709c657', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('464b8e30-186a-4e1a-9db1-d5cbd709c657', foundational, proportional_desert_justifies_state_killing).
narrative_ontology:cs_axiom_status(proportional_desert_justifies_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('464b8e30-186a-4e1a-9db1-d5cbd709c657', proportional_desert_justifies_state_killing, deontological).
narrative_ontology:cs_axiom('464b8e30-186a-4e1a-9db1-d5cbd709c657', secondary, lex_talionis_is_moral_imperative).
narrative_ontology:cs_axiom_status(lex_talionis_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('464b8e30-186a-4e1a-9db1-d5cbd709c657', lex_talionis_is_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('464b8e30-186a-4e1a-9db1-d5cbd709c657', classical_retributive_justice).
narrative_ontology:cs_drift_state('464b8e30-186a-4e1a-9db1-d5cbd709c657', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('464b8e30-186a-4e1a-9db1-d5cbd709c657', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, society_as_a_whole).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, adjudicates cases, and carries out executions. Its legitimacy is grounded in upholding justice and the moral order through proportional desert.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct targets of the constraint, forfeiting their lives as a consequence of their actions. They are legally and physically trapped, with no exit from the system once convicted and sentenced.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderers, payer,
    powerless, immediate, trapped, local).

% Benefits from the perceived restoration of balance and justice when proportional desert is enacted. This is an abstract beneficiary, representing the normative ideal upheld by the constraint.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order).

% Benefits from the affirmation of its moral values and the sense that justice has been served. This provides a collective psychological and social benefit, distinct from deterrence.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, society_as_a_whole, beneficiary,
    organized, generational, constrained, national).

% May experience a sense of closure or justice from the execution, fulfilling the retributive demand. Their benefit is primarily emotional and psychological.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Argue that state killing is inherently immoral and violates fundamental human rights, regardless of desert. They are excluded from the retributive framing's core premise but actively contest the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal response to heinous crimes by establishing a framework for proportional punishment, affirming shared moral principles, and providing a mechanism for perceived justice.
% TRANSFER_FUNCTION: Transfers the life of the convicted murderer to the state, in exchange for the perceived restoration of moral balance and the affirmation of societal values.
% ABSENT_VOICES: Abolitionist advocates are structurally excluded from the retributive framing, as their core premise rejects the state's right to take a life, even for desert. They would argue for alternative forms of justice that do not involve state killing.
% DISAPPEARANCE_RATIONALE: If the retributive justification for state killing vanished, the entire legal and moral framework for capital punishment would collapse. Societies would need to fundamentally re-evaluate their approach to extreme crimes, leading to significant legal and philosophical reorganization.
% FOUNDING_PROBLEM: The problem of how to justly respond to the ultimate crime (murder) in a way that upholds moral order and provides proportional desert for the offender.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, including some legal scholars and victims' families, attest that the problem of proportional desert for murder remains live. Opponents (abolitionists) contest the validity of the 'problem' itself, arguing that state killing is never a just solution, but within the retributive framework, the problem is considered ongoing.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the constraint demands the ultimate cost (life) from the convicted. Suppression is also high (0.9) as the state must actively enforce this ultimate penalty against the will of the condemned, with no viable exit. Theater ratio is low (0.2) because the act of execution, while ritualized, is a direct and irreversible consequence, not primarily performative. Resistance is high (0.75) due to ongoing moral and legal challenges from abolitionist movements and human rights organizations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state judicial system and society, the constraint is a necessary mechanism for justice. From the perspective of the convicted and abolitionist advocates, it is an unjust and barbaric act. The engine's classification will highlight this divergence, showing a 'Tangled Rope' from the perspective of the convicted, and potentially a 'Rope' or 'Scaffold' from the perspective of the beneficiaries, depending on how the coordination function is weighed against the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'moral_order' and 'society_as_a_whole' are beneficiaries, as the constraint is framed to uphold their values and sense of justice. 'Convicted_murderers' are the clear victims/targets, bearing the full cost. 'Victims_families' are also beneficiaries, receiving a form of closure. 'Abolitionist_advocates' are excluded, as their fundamental premise rejects the constraint's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (proportional desert) is considered 'live' within this reading, preventing it from being classified as a Piton. However, the high resistance and ongoing contestation suggest that its coordination function is heavily intertwined with its extractive nature, consistent with a Tangled Rope. The classification prevents mislabeling it as pure coordination by acknowledging the victims and active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_ambiguity,
    'Is the death penalty truly a proportional response to murder, or does it exceed what is morally permissible, even under a retributive framework?',
    'Philosophical consensus on the limits of lex talionis, or a shift in societal moral intuitions regarding the sanctity of life versus desert.',
    'If deemed disproportionate, the extractiveness of the constraint would be re-evaluated as excessive, potentially shifting its classification towards a Snare, as the ''justice'' coordination function would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_ambiguity, conceptual, 'Ambiguity regarding the moral proportionality of capital punishment.').

omega_variable(
    moral_order_beneficiary_status,
    'Is ''moral_order'' a genuine beneficiary that collects from the constraint, or is it a rhetorical construct used to justify extraction?',
    'Analysis of the constraint''s persistence in the absence of demonstrable societal benefit beyond the rhetorical, or a shift in the public discourse away from desert-based justifications.',
    'If ''moral_order'' is found to be a rhetorical construct, the constraint''s beneficiary structure would be weakened, potentially increasing its effective extractiveness and pushing it closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_order_beneficiary_status, conceptual, 'Whether the ''moral_order'' is a genuine beneficiary or a rhetorical justification.').

omega_variable(
    deterrence_vs_retribution_overlap,
    'To what extent does the retributive reading implicitly rely on deterrence effects for its societal acceptance, even if not explicitly claimed?',
    'Sociological studies on public support for capital punishment, disentangling stated retributive motives from implicit deterrence expectations.',
    'If significant implicit reliance on deterrence is found, the ''retributive_reading'' would be seen as structurally coupled to the ''deterrence_reading'', potentially complicating its classification and highlighting a hidden coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_retribution_overlap, empirical, 'Overlap between retributive and deterrence justifications in public perception.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__retributive_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__retributive_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__retributive_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__retributive_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__retributive_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__retributive_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__retributive_reading, base_extractiveness, 30, 0.84).
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

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_legitimacy' kernel. This 'retributive_reading' focuses on proportional desert, distinct from deterrence or abolitionist arguments. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
