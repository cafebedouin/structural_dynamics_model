% ============================================================================
% CONSTRAINT STORY: truth_procedure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_truth_procedure_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: truth_procedure_reading
 *   human_readable: Fiat Political Axiom as Truth-Procedure (Badiouian Reading)
 *   domain: debate_theory/political_philosophy
 *
 * SUMMARY:
 *   In competitive policy debate, a debater declares a fiat axiom
 *   ('healthcare belongs to everyone') and, when pressed on whether the state
 *   would ever actually implement it, retreats to a Badiouian framework: the
 *   axiom's efficacy is not located in empirical state responsiveness but in
 *   the truth-procedure it constitutes — a subjective, ontological event that
 *   reconstitutes the declaring subject's (and collective's) fidelity to the
 *   claim, independent of policy uptake. This story is ONE READING among
 *   several of the shared kernel of what makes fiat efficacious. It should
 *   not be read as adjudicating between readings; it isolates the structural
 *   profile of the truth-procedure account specifically. Under this reading,
 *   the Holocaust survivor, torture victim, or uninsured patient named by the
 *   axiom is redescribed as a potential subject-of-truth rather than as a
 *   passive beneficiary awaiting policy — success is measured in fidelity to
 *   the axiom, not in outcomes.
 *
 * KEY AGENTS:
 *   - declaring_debaters: primary agenda-setter and beneficiary — deploys the reading to win rounds and to resolve the efficacy objection philosophically
 *   - truth_procedure_theorists: secondary beneficiary — the pedagogical apparatus that sustains the reading's legitimacy across seasons
 *   - competitive_debate_circuit: institutional enforcer — ballots and judge training that make fidelity-to-axiom cashable as competitive success
 *   - policy_outcome_stakeholders: excluded — the material referents of the axiom, absent from the room whose verdict decides the axiom's 'success'
 *   - debate_theory_observers: analytical observer — sees the full structure and questions whether the reframe resolves or evades the original objection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(truth_procedure_reading, 0.28).
domain_priors:suppression_score(truth_procedure_reading, 0.35).
domain_priors:theater_ratio(truth_procedure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(truth_procedure_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(truth_procedure_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(truth_procedure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(truth_procedure_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(truth_procedure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(truth_procedure_reading, tangled_rope).
narrative_ontology:human_readable(truth_procedure_reading, "Fiat Political Axiom as Truth-Procedure (Badiouian Reading)").
narrative_ontology:topic_domain(truth_procedure_reading, "debate_theory/political_philosophy").

domain_priors:requires_active_enforcement(truth_procedure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(truth_procedure_reading, '40b88ae3-491e-4d5a-93de-3e941a6b3a51').
narrative_ontology:cs_kernel_codification('40b88ae3-491e-4d5a-93de-3e941a6b3a51', distributed).
narrative_ontology:cs_authority_grounding('40b88ae3-491e-4d5a-93de-3e941a6b3a51', practice).
narrative_ontology:cs_interpretation_layer_present('40b88ae3-491e-4d5a-93de-3e941a6b3a51').
narrative_ontology:cs_reading_relation('40b88ae3-491e-4d5a-93de-3e941a6b3a51', fiat_efficacy_kernel__empirical_precedent_reading, coexists_with).
narrative_ontology:cs_reading_relation('40b88ae3-491e-4d5a-93de-3e941a6b3a51', fiat_efficacy_kernel__scholarship_reading, coexists_with).
narrative_ontology:cs_reading_relation('40b88ae3-491e-4d5a-93de-3e941a6b3a51', fiat_efficacy_kernel__predictive_synthesis_reading, coexists_with).
narrative_ontology:cs_reading_relation('40b88ae3-491e-4d5a-93de-3e941a6b3a51', fiat_efficacy_kernel__empathy_simulation_reading, influences).
narrative_ontology:cs_reading_relation('40b88ae3-491e-4d5a-93de-3e941a6b3a51', fiat_efficacy_kernel__utopian_fiction_reading, influences).
narrative_ontology:cs_axiom('40b88ae3-491e-4d5a-93de-3e941a6b3a51', foundational, efficacy_located_in_subjective_ontological_break).
narrative_ontology:cs_axiom_status(efficacy_located_in_subjective_ontological_break, holdable).
narrative_ontology:cs_axiom_grounding('40b88ae3-491e-4d5a-93de-3e941a6b3a51', efficacy_located_in_subjective_ontological_break, deontological).
narrative_ontology:cs_axiom('40b88ae3-491e-4d5a-93de-3e941a6b3a51', secondary, state_responsiveness_irrelevant_to_success_criterion).
narrative_ontology:cs_axiom_status(state_responsiveness_irrelevant_to_success_criterion, holdable).
narrative_ontology:cs_axiom_grounding('40b88ae3-491e-4d5a-93de-3e941a6b3a51', state_responsiveness_irrelevant_to_success_criterion, conventional).
narrative_ontology:cs_reference_frame('40b88ae3-491e-4d5a-93de-3e941a6b3a51', policy_debate_pre_critical_turn).
narrative_ontology:cs_drift_state('40b88ae3-491e-4d5a-93de-3e941a6b3a51', post_performative_turn_circuit, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40b88ae3-491e-4d5a-93de-3e941a6b3a51', '').
narrative_ontology:cs_kernel_id(truth_procedure_reading, fiat_efficacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(truth_procedure_reading, declaring_debaters).
narrative_ontology:constraint_beneficiary(truth_procedure_reading, truth_procedure_theorists).
narrative_ontology:constraint_beneficiary(truth_procedure_reading, competitive_debate_circuit).
narrative_ontology:constraint_victim(truth_procedure_reading, policy_outcome_stakeholders).
narrative_ontology:constraint_victim(truth_procedure_reading, opposing_debaters_forced_into_frame).
narrative_ontology:constraint_victim(truth_procedure_reading, material_beneficiaries_of_actual_reform).
narrative_ontology:constraint_vindicates(truth_procedure_reading, badiouian_event_ontology).
narrative_ontology:constraint_vindicates(truth_procedure_reading, subject_constituted_through_fidelity_to_axiom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares the fiat axiom ('healthcare belongs to everyone') inside a debate round and insists its efficacy is located in the subjective break it produces in the declaring subject and in the collective's fidelity to it, not in whether any legislature moves. Wins or loses the round on this framing; carries the framework to future rounds regardless of outcome.
narrative_ontology:constraint_stakeholder(truth_procedure_reading, declaring_debaters, agenda_setter,
    moderate, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(truth_procedure_reading, declaring_debaters, beneficiary).

% Coaches, camps, and judges who have built a pedagogical and adjudicative apparatus around Badiouian truth-procedure argumentation. They benefit from the framework's continued legitimacy as a competitive strategy and as an intellectual credential, independent of any policy result it produces outside the round.
narrative_ontology:constraint_stakeholder(truth_procedure_reading, truth_procedure_theorists, beneficiary,
    organized, generational, mobile, national).

% The tournament and judging infrastructure treats the truth-procedure move as a legitimate, sometimes winning, argument. It supplies the enforcement mechanism (ballots, norms, judge training) that makes fidelity-to-axiom cashable as competitive success, which is what keeps the reading alive round after round.
narrative_ontology:constraint_stakeholder(truth_procedure_reading, competitive_debate_circuit, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(truth_procedure_reading, competitive_debate_circuit, agenda_setter).

% Must answer the truth-procedure claim on its own ontological terms or lose the argumentative ground; cannot simply contest the policy's feasibility without being told feasibility is the wrong register. Pays in strategic flexibility and often in the ballot when judges credit the framework.
narrative_ontology:constraint_stakeholder(truth_procedure_reading, opposing_debaters_forced_into_frame, payer,
    moderate, immediate, constrained, local).

% The uninsured patient, the torture survivor, the person actually named by the axiom — they are not in the debate room and their material situation is untouched by who wins the round. Under this reading they are redescribed as potential subjects-of-truth whose stake is in the axiom's fidelity, not as people waiting on a policy change.
narrative_ontology:constraint_stakeholder(truth_procedure_reading, policy_outcome_stakeholders, excluded,
    powerless, biographical, trapped, national).

% Populations who would benefit from the policy actually being enacted have no voice in whether the debate community treats fidelity-to-axiom or policy-adoption as the measure of success. If the truth-procedure frame displaces empirical/outcome argumentation as the prestige move, resources and attention that could go toward outcome-tracking arguments go instead toward ontological argumentation.
narrative_ontology:constraint_stakeholder(truth_procedure_reading, material_beneficiaries_of_actual_reform, excluded,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(truth_procedure_reading, material_beneficiaries_of_actual_reform, payer).

% Scholars and critics of debate pedagogy who examine whether the truth-procedure framework is a genuine philosophical contribution or a rhetorical technology for winning rounds while insulating claims from real-world accountability.
narrative_ontology:constraint_stakeholder(truth_procedure_reading, debate_theory_observers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides debaters and judges a shared, adjudicable standard for evaluating fiat-based political claims that does not require empirical proof of state responsiveness — coordinating around 'fidelity to the axiom' as a measurable, arguable quantity inside a time-limited round.
% TRANSFER_FUNCTION: Moves argumentative burden and competitive credit toward the declaring debater and away from opposing debaters who must contest feasibility; moves attention and pedagogical resources within the debate community toward ontological/philosophical argumentation and away from outcome-tracking argumentation, without moving anything toward the populations named by the axiom.
% ABSENT_VOICES: The uninsured patient, the torture survivor, and other material referents of the axiom are structurally absent from the room whose ballot decides whether the axiom 'succeeded'; they would object that a subjective transformation in the declaring debater does not put anyone in a hospital bed.
% DISAPPEARANCE_RATIONALE: Debate coaches, theorists, and the circuit's ballot-writing apparatus would say the practice of ontological/truth-procedure argumentation would collapse and a real pedagogical tradition would be lost; policy-outcome-focused critics would say nothing in the material world changes, because the framework's efficacy claim was never actually about outcomes to begin with — its disappearance only removes a competitive strategy, not a policy result.
% FOUNDING_PROBLEM: Fiat-based policy debate faced a recurring objection: 'the state will never actually do this, so why does the debate matter?' The truth-procedure reading was constructed to answer that objection by relocating efficacy from state responsiveness to the declaring subject's ontological transformation, following Badiou's account of truth-events and fidelity.
% FOUNDING_PROBLEM_CORROBORATION: Truth-procedure theorists and coaches attest the problem (efficacy skepticism) is live and the framework solves it philosophically. Debate theory critics and scholars outside the competitive circuit — including critics of academic debate pedagogy more broadly — attest that the reframing does not resolve the original objection but rather changes the subject, since it never establishes that anything named by the axiom actually improves; no source outside people invested in continuing to run or judge the argument attests that it closes the original gap between declaration and material change.
narrative_ontology:disappearance_verdict(truth_procedure_reading, contested).
narrative_ontology:founding_problem_status(truth_procedure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(truth_procedure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(truth_procedure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(truth_procedure_reading, 0.28, 'claude-sonnet-5', 'fiat_efficacy_kernel_2026_20260803_102258', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(truth_procedure_reading_tests).
:- end_tests(truth_procedure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-moderate (0.28 at interval end) because the coordination function is real: the reading gives debaters and judges a genuinely adjudicable standard, and no one is materially dispossessed by its operation in the narrow sense of resources seized. But it is not zero, because attention, pedagogical prestige, and competitive credit are diverted toward ontological argumentation and away from outcome-accountable argumentation — a genuine, if diffuse, transfer away from the populations the axiom names. Theater ratio is moderate and rising (0.25 to 0.42) because as the framework matures into a recognized 'move,' an increasing share of its deployment is citation-performance (invoking Badiou correctly) rather than substantive engagement with whether fidelity actually does anything for the referent population. Suppression is moderate: opposing debaters are structurally constrained from contesting the axiom on feasibility grounds once the ontological register is invoked, though this is a rhetorical constraint internal to a game, not a material one.
 *
 * DIRECTIONALITY LOGIC:
 *   Declaring debaters and the theorist/coaching apparatus sit near the beneficiary end: they collect competitive and intellectual credit from the framework's continued legitimacy, and their exit options (switching arguments, circuits, or careers) are mobile. Opposing debaters pay in strategic flexibility within the immediate, local scope of a single round — constrained exit, since the round itself cannot be exited without forfeiture. Policy outcome stakeholders and material beneficiaries of actual reform are the deepest targets: trapped exit options, powerless power atom, and a directionality that the derivation correctly pushes toward the full-target end even though they are structurally outside the constraint's operation entirely — their d reflects that the framework's success or failure has zero causal bearing on their situation, which is itself a form of being extracted from (attention and legitimacy that could go toward outcome-accountable politics is spent elsewhere).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'debate about policy that will never be implemented is pointless' — is contested rather than resolved. The truth-procedure reading does not close this gap; it relocates the site of evaluation from policy uptake (where the gap is real and unclosed) to subjective fidelity (where the gap is definitionally closed, because fidelity is measured by the same community that adjudicates it). This is precisely the mandatrophy risk: a mandate (address the efficacy objection) that appears resolved only because its success criterion was redefined by the same party whose practice depended on the objection going away. The classification prevents simple extraction-labeling because the coordination function (a workable adjudicative standard) is genuinely real for the debate community; it also prevents naive coordination-labeling because the reframing's central move — redescribing absent material stakeholders as 'potential subjects-of-truth' — is doing definitional work that conveniently forecloses the original objection rather than answering it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_measurement_circularity,
    'Is ''fidelity to the axiom'' measured by any standard independent of the same community (judges, coaches, theorists) that benefits from the framework''s continued legitimacy, or is the measure entirely internal to the beneficiary set?',
    'Identify whether any adjudicative or scholarly standard for fidelity exists that is administered by parties without a stake in the competitive debate circuit''s continued legitimacy — e.g., philosophy departments evaluating the argument''s fidelity to Badiou''s actual texts, independent of ballot outcomes.',
    'If no independent standard exists, the reading''s success criterion is self-referential and the coordination function is closer to pure in-group credentialing than to genuine philosophical accountability, pushing the classification toward snare; if an independent standard exists and constrains circuit practice, the coordination function is more genuine, supporting tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_measurement_circularity, conceptual, 'Whether fidelity-to-axiom is measured by a standard independent of its beneficiaries.').

omega_variable(
    subject_of_truth_redescription_ethics,
    'Does redescribing the uninsured patient or torture survivor as a ''potential subject-of-truth'' rather than a passive victim genuinely enrich their ontological status, or does it function primarily to relieve the declaring debater and adjudicating community of accountability to material outcomes?',
    'Solicit assessment from populations actually named by such axioms (e.g., patient advocacy groups, survivor organizations) on whether they experience the truth-procedure framing as recognition or as a further removal of their concerns from the site of decision-making.',
    'If the redescription is experienced as recognition, the coordination function extends beyond the debate community; if experienced as further erasure, the extraction component (diversion of attention/prestige from outcome-accountability) is more severe than the current 0.28 extractiveness score suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subject_of_truth_redescription_ethics, preference, 'Whether the ontological redescription of victims serves them or serves the argument''s users.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the fiat_efficacy_kernel supports at least six structurally distinct readings, is there a principled basis — outside the debate community''s own preferences — for treating the truth_procedure_reading as the dominant or default account of fiat efficacy, or is reading-selection itself an internal political act within the debate circuit?',
    'Track which reading tends to be favored by which judging pools, coaching lineages, or institutional affiliations; a reading favored disproportionately by ideologically aligned judge pools would suggest reading-selection functions as gatekeeping rather than neutral theoretical pluralism.',
    'If reading-selection correlates strongly with judge ideology/lineage rather than argument quality, the appearance of six ''live'' readings may mask a de facto hierarchy enforced through adjudication rather than through open theoretical contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the multiplicity of kernel readings is genuinely open or is gatekept by circuit sociology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(truth_procedure_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trut_tr_t0, truth_procedure_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trut_tr_t4, truth_procedure_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(trut_tr_t8, truth_procedure_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement(trut_tr_t12, truth_procedure_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(trut_tr_t16, truth_procedure_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(trut_tr_t20, truth_procedure_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(trut_be_t0, truth_procedure_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(trut_be_t4, truth_procedure_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(trut_be_t8, truth_procedure_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(trut_be_t12, truth_procedure_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(trut_be_t16, truth_procedure_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(trut_be_t20, truth_procedure_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(trut_su_t0, truth_procedure_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(trut_su_t4, truth_procedure_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(trut_su_t8, truth_procedure_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(trut_su_t12, truth_procedure_reading, suppression_requirement, 12, 0.31).
narrative_ontology:measurement(trut_su_t16, truth_procedure_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement(trut_su_t20, truth_procedure_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(truth_procedure_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(truth_procedure_reading, 0.08).
narrative_ontology:affects_constraint(truth_procedure_reading, empirical_precedent_reading).
narrative_ontology:affects_constraint(truth_procedure_reading, scholarship_reading).
narrative_ontology:affects_constraint(truth_procedure_reading, predictive_synthesis_reading).
narrative_ontology:affects_constraint(truth_procedure_reading, empathy_simulation_reading).
narrative_ontology:affects_constraint(truth_procedure_reading, utopian_fiction_reading).

% DUAL FORMULATION NOTE:
% This story is one of six siblings decomposing the natural-language concept 'fiat efficacy' (fiat_efficacy_kernel) into structurally distinct claims per the ε-invariance principle. Each sibling locates efficacy in a different site: truth-procedure (ontological transformation of the declaring subject), empirical precedent (historical policy uptake), scholarship (academic contribution), predictive synthesis (forecasting value), empathy simulation (perspective-taking), and utopian fiction (imaginative value). Each carries its own ε, beneficiary/victim structure, and classification. This story should not be read as the single account of fiat efficacy; it is linked to all five siblings via affects_constraints, and each sibling should reciprocally link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
