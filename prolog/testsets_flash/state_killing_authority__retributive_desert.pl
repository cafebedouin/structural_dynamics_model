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
 *   human_readable: State Killing Authority: Retributive Desert
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'retributive desert' reading of state
 *   killing authority, where capital punishment is justified by the principle
 *   of lex talionis: murderers forfeit their right to life, and proportional
 *   justice demands 'death for death'. It is a reading that grounds state
 *   authority in a moral imperative for retribution, distinct from
 *   utilitarian considerations like deterrence or categorical prohibitions
 *   against state killing. The constraint is actively enforced by the state
 *   judicial system, extracting the life of the condemned.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.65).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.95).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Killing Authority: Retributive Desert").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'ec8631bc-62fa-4398-b1e3-94516ab598fe').
narrative_ontology:cs_kernel_codification('ec8631bc-62fa-4398-b1e3-94516ab598fe', formalized).
narrative_ontology:cs_authority_grounding('ec8631bc-62fa-4398-b1e3-94516ab598fe', lineage).
narrative_ontology:cs_interpretation_layer_present('ec8631bc-62fa-4398-b1e3-94516ab598fe').
narrative_ontology:cs_reading_relation('ec8631bc-62fa-4398-b1e3-94516ab598fe', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('ec8631bc-62fa-4398-b1e3-94516ab598fe', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('ec8631bc-62fa-4398-b1e3-94516ab598fe', foundational, murder_forfeits_right_to_life).
narrative_ontology:cs_axiom_status(murder_forfeits_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('ec8631bc-62fa-4398-b1e3-94516ab598fe', murder_forfeits_right_to_life, deontological).
narrative_ontology:cs_axiom('ec8631bc-62fa-4398-b1e3-94516ab598fe', foundational, lex_talionis_is_proportional_justice).
narrative_ontology:cs_axiom_status(lex_talionis_is_proportional_justice, holdable).
narrative_ontology:cs_axiom_grounding('ec8631bc-62fa-4398-b1e3-94516ab598fe', lex_talionis_is_proportional_justice, deontological).
narrative_ontology:cs_reference_frame('ec8631bc-62fa-4398-b1e3-94516ab598fe', classical_retributive_justice).
narrative_ontology:cs_drift_state('ec8631bc-62fa-4398-b1e3-94516ab598fe', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ec8631bc-62fa-4398-b1e3-94516ab598fe', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, state_judicial_system).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murdered_person_posthumous_vindication).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_person).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, family_of_condemned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victims_families).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportional_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, interpreting and applying the principle of 'death for death' as a just and proportional response to murder. Benefits from maintaining its authority and the perceived moral order.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% The individual sentenced to death, who has forfeited their right to life according to this reading. Bears the ultimate cost of the constraint, with no legal or physical exit from the punishment.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_person, payer,
    powerless, immediate, trapped, local).

% Bears the emotional and social costs associated with the execution, including stigma and loss. Their options are limited to legal appeals and advocacy, with no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, family_of_condemned, payer,
    powerless, biographical, constrained, local).

% The murdered individual, whose right to life is posthumously vindicated by the state's proportional response. This is a conceptual beneficiary, representing the moral balance restored by the punishment.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murdered_person_posthumous_vindication, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murdered_person_posthumous_vindication).

% May experience a sense of justice or closure from the execution, seeing it as a proportional response to their loss. Their benefit is emotional and symbolic, not material.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Advocate for the inherent impermissibility of state killing, regardless of the crime. Their arguments are excluded from the retributive desert framework, which focuses on proportionality and forfeiture.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, categorical_abolitionists, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, proportional response to murder, aiming to coordinate societal understanding of justice and the sanctity of life by asserting that a life taken demands a life in return.
% TRANSFER_FUNCTION: Transfers the right to life from the condemned person to the state, in exchange for the perceived restoration of moral balance and vindication of the victim's right to life.
% ABSENT_VOICES: Categorical abolitionists are excluded from the retributive desert framework, as their premise of inalienable life directly contradicts the forfeiture principle. Their arguments for inherent impermissibility are not considered within this reading's logic.
% DISAPPEARANCE_RATIONALE: If the principle of retributive desert for murder vanished, the entire legal and moral justification for capital punishment in many jurisdictions would collapse. Sentencing guidelines would need fundamental revision, and the societal understanding of justice for murder would shift dramatically, likely towards life imprisonment as the maximum penalty.
% FOUNDING_PROBLEM: The problem of how to justly respond to the ultimate crime of murder, ensuring that the punishment fits the gravity of the offense and upholds the moral order.
% FOUNDING_PROBLEM_CORROBORATION: Philosophical traditions, historical legal codes (e.g., Hammurabi), and some public opinion polls attest to the enduring belief in 'an eye for an eye' as a principle of justice, corroborating the live status of the founding problem from outside the immediate judicial system.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because the constraint directly extracts life. Suppression is very high (0.95) due to the state's monopoly on legitimate force and the complete lack of exit options for the condemned. Theater ratio is low (0.20) as the primary function of execution (retribution) is genuinely pursued, though some performative aspects exist (e.g., public statements, symbolic gestures). Accessibility collapse is high (0.80) as the legal system offers few alternatives once a death sentence is final. Resistance is high (0.70) from abolitionist movements and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state judicial system, this constraint is a necessary mechanism for upholding justice and moral order. From the perspective of the condemned and their families, it is a pure extraction, a final and irreversible loss. The murdered person's posthumous vindication is a conceptual benefit, experienced by the state and victims' families as a restoration of balance.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system is a beneficiary (d=0.0) as it maintains its authority and the perceived moral order. The murdered person (posthumous vindication) is also a beneficiary (d=0.0) conceptually. The condemned person (d=1.0) and their family (d=0.9) are clear targets, bearing the ultimate costs. Victims' families are beneficiaries (d=0.2) through the sense of justice. Categorical abolitionists are excluded (d=0.8) as their arguments are outside this reading's framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate is tied to the enduring belief in retributive justice. Mandatrophy would occur if society broadly rejected the 'death for death' principle, even if the state continued executions for other reasons (e.g., deterrence). The current contestation over its founding problem status ('live' vs. 'dead') indicates a potential for future mandatrophy if the 'dead' argument gains wider acceptance. The classification as Tangled Rope reflects the coordination of justice principles with the extraction of life, requiring active enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retribution_vs_deterrence_justification,
    'Is the primary justification for capital punishment in practice truly retributive desert, or is it a cover for deterrence-based instrumentalism?',
    'Analysis of judicial opinions and legislative debates: if deterrence arguments consistently outweigh retributive ones, reclassify as deterrence-instrument.',
    'If deterrence is the true driver, the constraint''s classification would shift towards a more instrumental (and potentially less morally grounded) form of extraction, potentially altering its ethical evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retribution_vs_deterrence_justification, conceptual, 'Ambiguity in the primary justification for capital punishment.').

omega_variable(
    forfeiture_vs_inalienable_rights,
    'Does murder truly result in the forfeiture of the right to life, or is the right to life inalienable, even for murderers?',
    'Philosophical consensus shift or international legal precedent establishing inalienability as a universal norm.',
    'If the right to life is deemed inalienable, the core premise of this reading collapses, leading to a reclassification towards categorical abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forfeiture_vs_inalienable_rights, conceptual, 'Contestation over the fundamental nature of the right to life.').

omega_variable(
    victim_vindication_reality,
    'Is the ''posthumous vindication'' of the murdered person a genuine benefit, or a rhetorical construct to justify state action?',
    'Sociological studies on the impact of executions on victims'' families and broader societal perceptions of justice, alongside philosophical analysis of conceptual personhood.',
    'If it''s primarily a rhetorical construct, the ''beneficiary'' status of the murdered person would be re-evaluated, potentially increasing the perceived extractiveness from the condemned without a corresponding moral balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_vindication_reality, empirical, 'The reality of posthumous victim vindication as a benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__retributive_desert, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t1990, state_killing_authority__retributive_desert, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(stat_tr_t2000, state_killing_authority__retributive_desert, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(stat_tr_t2010, state_killing_authority__retributive_desert, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(stat_tr_t2020, state_killing_authority__retributive_desert, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__retributive_desert, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__retributive_desert, base_extractiveness, 1976, 0.6).
narrative_ontology:measurement(stat_be_t1990, state_killing_authority__retributive_desert, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(stat_be_t2000, state_killing_authority__retributive_desert, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(stat_be_t2010, state_killing_authority__retributive_desert, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(stat_be_t2020, state_killing_authority__retributive_desert, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__retributive_desert, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__retributive_desert, suppression_requirement, 1976, 0.9).
narrative_ontology:measurement(stat_su_t1990, state_killing_authority__retributive_desert, suppression_requirement, 1990, 0.95).
narrative_ontology:measurement(stat_su_t2000, state_killing_authority__retributive_desert, suppression_requirement, 2000, 0.98).
narrative_ontology:measurement(stat_su_t2010, state_killing_authority__retributive_desert, suppression_requirement, 2010, 0.95).
narrative_ontology:measurement(stat_su_t2020, state_killing_authority__retributive_desert, suppression_requirement, 2020, 0.95).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__retributive_desert, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel. Its core premise of retributive desert and forfeiture of the right to life distinguishes it from deterrence-based justifications and categorical prohibitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
