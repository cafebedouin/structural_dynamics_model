% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Retributive Execution by Proportional Desert (Lex Talionis)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the retributive reading of the
 *   state_killing_legitimacy kernel: the claim that a murderer morally
 *   deserves death and that the state legitimately exacts this payment to
 *   restore proportionality and moral order. The offender is structurally
 *   positioned as a payer who owes a life for a life; the citizenry and
 *   victims' families are positioned as beneficiaries of restored moral
 *   balance; the state apparatus is the agenda setter that administers the
 *   constraint. The natural-language concept of 'capital punishment'
 *   conflates retributive, deterrence, and abolitionist constraints; this
 *   story isolates the retributive claim as a structurally distinct
 *   constraint with its own epsilon and stakeholder structure.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus (institutional/agenda_setter): prosecutes, sentences, and carries out executions under retributive statutes
 *   - condemned_offenders (powerless/payer): convicted murderers who lose liberty and life as the proportional payment
 *   - citizenry (organized/beneficiary): the moral community receiving restored order through state-administered desert
 *   - victims_families (moderate/beneficiary): survivors afforded retributive closure
 *   - abolitionist_movement (organized/excluded): human rights and reform voices structurally barred from the sentencing calculus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.86).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.84).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Retributive Execution by Proportional Desert (Lex Talionis)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, 'a594a39a-299a-495f-bf64-bef3fea98d71').
narrative_ontology:cs_kernel_codification('a594a39a-299a-495f-bf64-bef3fea98d71', formalized).
narrative_ontology:cs_authority_grounding('a594a39a-299a-495f-bf64-bef3fea98d71', lineage).
narrative_ontology:cs_interpretation_layer_present('a594a39a-299a-495f-bf64-bef3fea98d71').
narrative_ontology:cs_reading_relation('a594a39a-299a-495f-bf64-bef3fea98d71', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a594a39a-299a-495f-bf64-bef3fea98d71', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('a594a39a-299a-495f-bf64-bef3fea98d71', foundational, murderer_forfeits_life_right).
narrative_ontology:cs_axiom_status(murderer_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('a594a39a-299a-495f-bf64-bef3fea98d71', murderer_forfeits_life_right, deontological).
narrative_ontology:cs_axiom('a594a39a-299a-495f-bf64-bef3fea98d71', foundational, proportional_desert_requires_execution).
narrative_ontology:cs_axiom_status(proportional_desert_requires_execution, holdable).
narrative_ontology:cs_axiom_grounding('a594a39a-299a-495f-bf64-bef3fea98d71', proportional_desert_requires_execution, deontological).
narrative_ontology:cs_reference_frame('a594a39a-299a-495f-bf64-bef3fea98d71', retributive_desert_framework).
narrative_ontology:cs_drift_state('a594a39a-299a-495f-bf64-bef3fea98d71', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a594a39a-299a-495f-bf64-bef3fea98d71', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, citizenry).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_offenders).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, lex_talionis_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, retributive_justice_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital statutes, prosecutes capital charges, conducts executions, and maintains the carceral pipeline that delivers condemned offenders to the death chamber. Sets the procedural and substantive rules defining proportional desert.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Constituted as the moral community whose collective outrage is answered by execution. Receives the symbolic restoration of moral order and the affirmation of the life-right of the law-abiding. Bears no direct cost and does not individually administer the constraint.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, citizenry, beneficiary,
    organized, generational, constrained, national).

% Survivors of murder victims who are afforded retributive closure through state-administered execution of the offender. Their grievance is channeled into the public prosecution rather than private vengeance.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, national).

% Convicted murderers sentenced to death under retributive statutes. They lose all liberty and ultimately life. Their exit is physically barred by incarceration and legally barred by the sentencing judgment; they are the seat through whom the proportional payment is exacted.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, national).

% Court-appointed or retained attorneys who challenge death sentences through appeals and post-conviction review. They argue against desert claims and procedural fairness but do not collect or pay the constraint's extraction; they observe and resist from within the procedural frame.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, defense_counsel, observer,
    moderate, biographical, constrained, national).

% Human rights organizations, religious groups, and legal reformers who reject retributive desert as a justification for state killing. They are structurally excluded from the sentencing calculus and execution protocol; their arguments are treated as external to the retributive framework.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_movement, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective grievance generated by murder through a sovereign monopoly on vengeance, substituting measured, proportional state punishment for private blood feuds and thereby restoring the moral order violated by the offense.
% TRANSFER_FUNCTION: Moves the offender's life and remaining years from the offender to the state's punitive apparatus and the community's moral ledger, exacting a payment said to proportionally balance the harm done.
% ABSENT_VOICES: The condemned offender's normative voice is reduced to procedural defense; abolitionist ethicists and human rights bodies are excluded from the sentencing calculus; future victims of wrongful execution are absent by definition.
% DISAPPEARANCE_RATIONALE: If retributive execution vanished overnight, prosecutorial charging practices would shift away from death verdicts, victim-survivor expectations would reorient toward restorative or incapacitative frames, and the carceral pipeline would terminate at life imprisonment rather than the chamber.
% FOUNDING_PROBLEM: Murder created cycles of private vengeance and blood feuds that destabilized communal peace and overwhelmed informal dispute resolution; the state needed a sovereign monopoly on lethal punishment to replace retaliation with proportionate justice.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians corroborate the transition from private feud to sovereign punishment; criminologists and abolitionist ethicists outside the beneficiary set contest that execution is necessary to maintain that monopoly, citing stable homicide rates in abolitionist jurisdictions.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.86, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.86) because the constraint exacts the maximum transferable good: the offender's life. Suppression is high (0.84) because persistence depends on active carceral and execution infrastructure, exclusion of abolitionist alternatives from the sentencing frame, and the physical incapacitation of the condemned. Theater_ratio is moderate (0.45) because while the execution itself is terminal and real, the surrounding ritual â lengthy appeals, last meals, witnessing protocols â performs moral proportionality for the community. Accessibility_collapse is high (0.82): once sentenced under a retributive capital statute, alternatives vanish. Resistance is moderate (0.62): substantial abolitionist opposition exists but is institutionally excluded from the capital sentencing room.
 *
 * PERSPECTIVAL GAP:
 *   The state_execution_apparatus experiences the constraint as legitimate justice and sovereign duty; the condemned_offender experiences it as terminal extraction; the citizenry experiences it as moral restoration. The engine computes per-seat classifications from this structural asymmetry â the agenda-setter seat may compute toward tangled_rope or rope, while the condemned seat computes toward snare.
 *
 * DIRECTIONALITY LOGIC:
 *   citizenry and victims_families are beneficiaries (low d, low chi). condemned_offenders are victims (high d, high chi). state_execution_apparatus is agenda_setter: it does not personally collect the extraction but wields it, placing its d near the middle but slightly toward beneficiary because its authority is constituted by the constraint. defense_counsel is observer (analytical exit, neutral d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â replacing private blood feuds with sovereign punishment â may be substantially solved by non-lethal incapacitation, yet the retributive arrangement persists because it is read as intrinsically required by desert rather than instrumentally required by peace. This creates a mandatrophy risk: if the coordination function (ending feuds) is dead but the constraint persists because of desert-narrative lock-in, the reading drifts toward piton or snare. The contested status of the founding problem and the rising theater ratio over the interval document this tension without pre-judging it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_as_moral_fact_or_fiction,
    'Is the offender''s ''forfeiture'' of life-right an intrinsic moral fact discoverable by reason, or a retroactive legal fiction constructed to authorize state killing?',
    'Cross-cultural comparative jurisprudence and neurolaw evidence on moral responsibility; if desert collapses into social utility or consensus, the retributive ground shifts toward deterrence or snare.',
    'If desert is a fiction, the constraint''s legitimacy dissolves into raw extraction and the reading reclassifies toward snare; if desert is a genuine moral property, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_as_moral_fact_or_fiction, conceptual, 'Whether proportional desert is an intrinsic moral property or constructed legal fiction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the condemned offender''s compliance structural (physical custody bars exit) or internalized (acceptance of moral desert reduces resistance)?',
    'Post-commutation or exoneration trajectory: if released offenders continue to affirm their desert, suppression is partly internalized; if they uniformly reject it, suppression is purely structural.',
    'Internalized suppression raises effective extraction beyond the structural measure because the target carries the constraint even after exit becomes possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in condemned offenders.').

omega_variable(
    retributive_coordination_genuineness,
    'Does the retributive framework genuinely coordinate a shared moral response, or does it merely theatricalize vengeance for public consumption?',
    'Empirical measurement of victim-survivor outcomes in retributive versus restorative frameworks; if retributive systems produce worse psychological and social outcomes, the coordination function is cover.',
    'If the coordination function is shown to be theatrical, the constraint loses its rope component and reclassifies as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_coordination_genuineness, empirical, 'Whether retributive execution produces genuine coordination or theatrical cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_retrib_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(skl_retrib_tr_t10, state_killing_legitimacy__retributive_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(skl_retrib_tr_t20, state_killing_legitimacy__retributive_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(skl_retrib_tr_t30, state_killing_legitimacy__retributive_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(skl_retrib_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(skl_retrib_tr_t50, state_killing_legitimacy__retributive_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(skl_retrib_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(skl_retrib_be_t10, state_killing_legitimacy__retributive_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(skl_retrib_be_t20, state_killing_legitimacy__retributive_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(skl_retrib_be_t30, state_killing_legitimacy__retributive_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(skl_retrib_be_t40, state_killing_legitimacy__retributive_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(skl_retrib_be_t50, state_killing_legitimacy__retributive_reading, base_extractiveness, 50, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(skl_retrib_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(skl_retrib_su_t10, state_killing_legitimacy__retributive_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(skl_retrib_su_t20, state_killing_legitimacy__retributive_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(skl_retrib_su_t30, state_killing_legitimacy__retributive_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(skl_retrib_su_t40, state_killing_legitimacy__retributive_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(skl_retrib_su_t50, state_killing_legitimacy__retributive_reading, suppression_requirement, 50, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is the retributive reading of the state_killing_legitimacy kernel, decomposing capital punishment legitimacy into a desert-based justification distinct from deterrence-based or abolitionist framings. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
