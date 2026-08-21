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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: State Authority to Execute: Retributive Desert Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'retributive desert' reading of state
 *   authority to execute, where capital punishment is justified by the
 *   murderer's forfeiture of their right to life and the principle of lex
 *   talionis (death for death). It is one reading of the broader
 *   'state_killing_authority' kernel, distinct from deterrence-based or
 *   abolitionist readings. The constraint is classified as a Tangled Rope
 *   because it performs a coordination function (establishing a proportional
 *   response to murder) but involves asymmetric extraction (the condemned
 *   person's life) and requires active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.65).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.95).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Authority to Execute: Retributive Desert Reading").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'dd41b1d6-20c9-41b6-9864-4670fb368998').
narrative_ontology:cs_kernel_codification('dd41b1d6-20c9-41b6-9864-4670fb368998', formalized).
narrative_ontology:cs_authority_grounding('dd41b1d6-20c9-41b6-9864-4670fb368998', lineage).
narrative_ontology:cs_interpretation_layer_present('dd41b1d6-20c9-41b6-9864-4670fb368998').
narrative_ontology:cs_reading_relation('dd41b1d6-20c9-41b6-9864-4670fb368998', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('dd41b1d6-20c9-41b6-9864-4670fb368998', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('dd41b1d6-20c9-41b6-9864-4670fb368998', foundational, life_forfeitable_by_murder).
narrative_ontology:cs_axiom_status(life_forfeitable_by_murder, holdable).
narrative_ontology:cs_axiom_grounding('dd41b1d6-20c9-41b6-9864-4670fb368998', life_forfeitable_by_murder, deontological).
narrative_ontology:cs_axiom('dd41b1d6-20c9-41b6-9864-4670fb368998', foundational, lex_talionis_as_proportional_justice).
narrative_ontology:cs_axiom_status(lex_talionis_as_proportional_justice, holdable).
narrative_ontology:cs_axiom_grounding('dd41b1d6-20c9-41b6-9864-4670fb368998', lex_talionis_as_proportional_justice, deontological).
narrative_ontology:cs_reference_frame('dd41b1d6-20c9-41b6-9864-4670fb368998', classical_retributive_justice).
narrative_ontology:cs_drift_state('dd41b1d6-20c9-41b6-9864-4670fb368998', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dd41b1d6-20c9-41b6-9864-4670fb368998', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumous).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victim_families).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_justice_advocates).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, death_row_inmates).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportional_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, including trials, appeals, and execution protocols. Its legitimacy is grounded in upholding justice and the rule of law, including the principle of proportionality.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct targets of the constraint, facing the forfeiture of life as punishment. Their legal avenues for appeal are exhausted, and their physical liberty is entirely suppressed.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons, payer,
    powerless, immediate, trapped, local).

% The murdered person's right to life is posthumously vindicated by the state's action, affirming the moral order that was violated. This is a symbolic, non-agentic benefit.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumous, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murder_victims_posthumous).

% Receive a form of closure or justice through the execution, seeing the principle of 'a life for a life' upheld. Their benefit is emotional and symbolic, not material.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victim_families, beneficiary,
    moderate, biographical, constrained, local).

% Benefit from the legal system upholding their philosophical commitment to justice as desert and proportionality. Their influence is through legal and political advocacy.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retributive_justice_advocates, beneficiary,
    organized, generational, mobile, national).

% Advocate for the abolition of capital punishment, arguing against the state's authority to take a life regardless of the crime. They are excluded from the retributive framing's core logic but exert external pressure.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, human_rights_organizations, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's response to murder by establishing a clear, proportional consequence that aligns with a specific moral philosophy of justice, ensuring that the punishment 'fits the crime' in a retributive sense.
% TRANSFER_FUNCTION: Transfers the right to life from the condemned person to the state, in exchange for the symbolic vindication of the victim's right to life and the restoration of a perceived moral balance.
% ABSENT_VOICES: Those who advocate for categorical abolition of state killing, or those who prioritize rehabilitation over retribution, are structurally excluded from the retributive desert framework. Their arguments are not considered valid within this specific reading of justice.
% DISAPPEARANCE_RATIONALE: If the state's authority to execute based on retributive desert vanished, the entire criminal justice system's philosophical grounding would shift. Sentences for murder would need re-evaluation, the concept of 'justice' would be redefined away from lex talionis, and the state's moral authority would be re-articulated.
% FOUNDING_PROBLEM: The problem of how to justly respond to the ultimate violation of a human life (murder), ensuring that the perpetrator receives a punishment proportional to the gravity of their crime, and that the moral order is restored.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, including some victim families and legal scholars, attest that the problem of achieving retributive justice for murder remains live. Opponents (e.g., human rights organizations) argue that the problem is framed incorrectly, or that the solution is morally unacceptable, but acknowledge the underlying societal need for a response to murder.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the ultimate cost (life) is extracted from the condemned. Suppression is very high (0.95) as the state's power to execute is nearly absolute once legal avenues are exhausted, with no exit for the condemned. Theater ratio is low (0.1) because the act of execution, while ritualized, is directly functional to this reading's goal of retribution; there is little performative maintenance of an atrophied function. Resistance is high (0.7) due to ongoing legal challenges and public debate from abolitionist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of retributive justice advocates, this constraint is a necessary and just application of law, a form of coordination that upholds moral order. From the perspective of the condemned or human rights organizations, it is a pure act of state extraction and suppression. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system acts as the agenda-setter, enforcing the constraint. Murder victims (posthumously) and their families are beneficiaries, as their sense of justice is served. Retributive justice advocates also benefit from the upholding of their philosophical principles. Condemned persons are the clear victims, losing their lives. Human rights organizations are excluded, as their arguments against state killing are outside the retributive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_vs_inalienable_rights,
    'Is the right to life truly forfeitable through criminal acts, or is it an inalienable right that the state cannot justly revoke?',
    'Conceptual analysis and philosophical debate within political philosophy and legal theory; no empirical resolution.',
    'If the right to life is inalienable, the retributive desert reading''s foundational premise collapses, reclassifying the constraint as pure extraction (Snare) from the condemned, with no legitimate coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forfeiture_vs_inalienable_rights, conceptual, 'Ambiguity regarding the philosophical basis of the right to life and its forfeiture.').

omega_variable(
    proportionality_measurement,
    'How is ''proportionality'' (lex talionis) objectively measured such that ''death for death'' is the uniquely just outcome, rather than an arbitrary or culturally specific interpretation?',
    'Cross-cultural legal anthropology and comparative criminal justice studies to identify universal principles of proportionality, or a formal axiomatic derivation of punishment from crime.',
    'If proportionality is found to be culturally contingent or unquantifiable, the claim of ''just desert'' weakens, potentially shifting the constraint towards a Snare if the coordination function is seen as a cover for state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement, empirical, 'The objective measurability and universality of retributive proportionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.1).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.1).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.1).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.1).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.94).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.95).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.95).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_authority' kernel. This 'retributive_desert' reading focuses on justice as proportional desert, distinct from deterrence or categorical abolition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
