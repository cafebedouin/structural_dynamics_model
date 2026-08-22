% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   State execution authority, under the deterrence reading, justifies
 *   capital punishment as a mechanism that prevents future murders by raising
 *   the cost of capital crimes to a level that rational potential offenders
 *   will not pay. Future potential victims are the beneficiary class
 *   (anonymous, prospective); executed offenders are the instrumental cost
 *   (identified, actual). The constraint is claimed as tangled_rope because
 *   it solves a genuine coordination problem (unified state authority over
 *   capital punishment, procedural standardization) AND enforces asymmetric
 *   extraction (the executed offender bears a cost borne by no other class).
 *   Extractiveness is moderate (0.58) because the reading's justification
 *   depends entirely on deterrence efficacy — if life imprisonment deters
 *   equally, extraction and justification both collapse. The theater ratio
 *   rises modestly (0.33→0.41) as abolition advocacy grows and deterrence
 *   evidence becomes contested; procedural legitimacy (appeals, safeguards)
 *   becomes a larger share of the constraint's functional activity relative
 *   to its deterrent operation.
 *
 * KEY AGENTS:
 *   - state_execution_authority: agenda setter, institutional power; administers the constraint and claims deterrent justification
 *   - executed_offenders: powerless, trapped; instrumental cost of the deterrent mechanism
 *   - future_potential_crime_victims: beneficiaries, structurally anonymous; benefit from reduced incidence of capital crime
 *   - crime_deterred_persons: beneficiaries, moderate power; rationally desist from capital crime because execution raises the cost
 *   - families_of_executed: payers, powerless; collateral cost, excluded from justificatory narrative
 *   - abolition_advocates: excluded, organized; would object on categorical grounds the reading does not admit
 *   - criminology_researchers: observers, institutional; generate empirical data the reading depends on but do not determine persistence
 *   - wrongfully_convicted_persons: victims, powerless; represent utilitarian loss and error-rate cost to the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.58).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '6e20e02c-cd95-43a2-a1b6-77431d965423').
narrative_ontology:cs_kernel_codification('6e20e02c-cd95-43a2-a1b6-77431d965423', formalized).
narrative_ontology:cs_authority_grounding('6e20e02c-cd95-43a2-a1b6-77431d965423', lineage).
narrative_ontology:cs_interpretation_layer_present('6e20e02c-cd95-43a2-a1b6-77431d965423').
narrative_ontology:cs_reading_relation('6e20e02c-cd95-43a2-a1b6-77431d965423', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e20e02c-cd95-43a2-a1b6-77431d965423', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('6e20e02c-cd95-43a2-a1b6-77431d965423', foundational, execution_deters_capital_crime).
narrative_ontology:cs_axiom_status(execution_deters_capital_crime, holdable).
narrative_ontology:cs_axiom_grounding('6e20e02c-cd95-43a2-a1b6-77431d965423', execution_deters_capital_crime, empirically_contingent).
narrative_ontology:cs_axiom('6e20e02c-cd95-43a2-a1b6-77431d965423', foundational, future_crime_prevention_justifies_present_execution).
narrative_ontology:cs_axiom_status(future_crime_prevention_justifies_present_execution, holdable).
narrative_ontology:cs_axiom_grounding('6e20e02c-cd95-43a2-a1b6-77431d965423', future_crime_prevention_justifies_present_execution, instrumental).
narrative_ontology:cs_reference_frame('6e20e02c-cd95-43a2-a1b6-77431d965423', state_capital_punishment_authority_justified_by_deterrence).
narrative_ontology:cs_drift_state('6e20e02c-cd95-43a2-a1b6-77431d965423', contemporary_empirical_contestation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6e20e02c-cd95-43a2-a1b6-77431d965423', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_crime_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, crime_deterred_persons).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, families_of_executed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers capital punishment policy. Justifies execution by its deterrent effect on future capital crimes: raising the cost of murder to death deters rational offenders from committing it. Conducts trials, reviews appeals, authorizes and carries out executions. Collects legitimacy from deterrence effectiveness and procedural fairness claims.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_execution_authority, agenda_setter,
    institutional, generational, analytical, national).

% Potential victims of murders that would occur absent the deterrent effect of execution. They are structurally anonymous — not yet identified, not yet targeted. Under the deterrence reading they benefit because execution raises the cost of capital crimes to the point where some would-be murderers rationally desist. This benefit is prospective and counterfactual: measured by crimes prevented, not crimes solved.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_crime_victims, beneficiary,
    powerless, immediate, trapped, national).

% Persons who contemplate committing a capital crime and rationally decide against it because execution raises the cost above their willingness to pay. Under the deterrence reading they benefit from the constraint by remaining free and alive. No record captures them — their benefit is the absence of their own criminal act.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, crime_deterred_persons, beneficiary,
    moderate, biographical, constrained, national).

% Persons convicted of capital crimes and sentenced to death. Under the deterrence reading they are instrumental costs — their death is the mechanism by which the deterrent effect operates. They bear the ultimate cost of the constraint. Exit options are exhausted by conviction and sentence; legal appeals are part of the constraint's enforcement machinery.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% Family members of the executed. They experience the loss of the executed person; under the deterrence reading they are collateral costs of the mechanism. They are also systematically excluded from the justificatory narrative — their loss is treated as an externality to the deterrence calculation rather than as a voice in the moral argument.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_executed, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, families_of_executed, excluded).

% Persons and organizations who argue that execution is categorically impermissible on deontological grounds (intrinsic right to life, inalienable dignity, state's delegitimization through killing). They would object to the deterrence reading's empirical premise and its utilitarian framework if seated in the decision. Their exclusion is structural: the deterrence reading does not admit moral objections in principle, only empirical challenges to efficacy.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Academic researchers studying the deterrent effect of capital punishment. They generate the empirical data the deterrence reading depends on. Systematic disagreement persists: some studies find deterrent effect, others find none or negative effects (incapacitation and brutalization). The constraint's justification depends on their findings but does not fully determine its persistence.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminology_researchers, observer,
    institutional, generational, analytical, global).

% An alternative institutional arrangement (life imprisonment without parole) that the deterrence reading must differentiate itself against. If life imprisonment deters equally, the reading's justification transfers to that mechanism instead, making execution functionally unnecessary. This alternative is not seated in the current constraint's operation but structures its contestation.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, life_sentence_alternatives, excluded,
    analytical, biographical, analytical, national).
narrative_ontology:stakeholder_non_agent(state_execution_authority__deterrence_reading, life_sentence_alternatives).

% Persons executed for capital crimes they did not commit. Under the deterrence reading they represent utilitarian loss: the constraint's error rate is a cost that reduces the net deterrent benefit. The reading's framework requires minimizing execution of the innocent to maintain justification, but the constraint's operation produces some nonzero error rate.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_persons, payer,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, state-administered system for authorizing capital punishment in capital cases. Solves the coordination problem of how a society enforces the ultimate penalty consistently, with procedural safeguards, and with centralized authority rather than vigilante justice. Deterrence is NOT a coordination function — it is a postulated causal mechanism justifying the constraint.
% TRANSFER_FUNCTION: Transfers the lives of executed offenders to the state execution authority as the mechanism by which deterrent effect operates. Transfers future crime risk from potential victims (who benefit from reduced murder incidence) to executed offenders (who bear the cost). The constraint moves authority to decide life-and-death questions from individuals to the state, and moves the cost of deterrence from potential victims to actual offenders.
% ABSENT_VOICES: Abolition advocates are structurally excluded: the deterrence reading does not admit categorical moral objections in principle, only empirical challenges to whether deterrence works. Families of the executed are excluded from the justificatory narrative — their loss is treated as an externality to deterrence calculations, not as a voice in the legitimacy question. Wrongfully convicted persons are excluded by structural invisibility — the constraint does not know in advance who is innocent.
% DISAPPEARANCE_RATIONALE: If execution authority vanished overnight, states would substitute life imprisonment or other severe penalties as deterrent mechanisms (or abandon deterrence-based justification for capital sentences entirely). The empirical question of deterrence would become moot; institutions would reorganize around whatever alternative mechanism was chosen. Potential victims would experience changed risk profiles based on the new penalty structure's efficacy.
% FOUNDING_PROBLEM: Capital crimes (murder, especially multiple murders, murders of state officials, crimes of extreme brutality) require a maximum penalty that serves as a credible signal of the state's resolve to deter such acts and as a mechanism ensuring the offender cannot commit the crime again.
% FOUNDING_PROBLEM_CORROBORATION: Deterrence advocates and some criminologists attest the founding problem is live and that execution uniquely solves it by creating maximal cost for capital crime. Abolition advocates, many criminologists, and international human rights bodies attest the founding problem is addressed equally well by life imprisonment, rendering execution functionally unnecessary and therefore unjustifiable. No consensus corroboration outside the deterrence-reading's own benefiting parties exists.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58 (moderate) because the deterrence reading's structural justification rests entirely on empirical efficacy: the executed offender is a cost ONLY if execution deters crimes that would otherwise occur. If life imprisonment deters equally, the structure transfers to that mechanism, making execution pure extraction. The reading's own framework requires that deterrence be the primary function for the cost to be justified. Suppression is high (0.72) because the constraint's persistence depends on actively preventing substitution with life imprisonment — abolition advocates and some criminologists argue life imprisonment is equivalent, so suppression must maintain the execution framework against this contestation. Theater grows from 0.33 to 0.41 because procedural legitimacy (appeals, safeguards, error-correction) becomes an increasingly visible share of enforcement activity as public doubt about deterrence efficacy rises. The time series is relatively flat in suppression and theater after t=25, reflecting a stable contested equilibrium: execution persists but must be defended against a permanent challenge to its efficacy, and procedural safeguards consume more resources as scrutiny intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The state execution authority (agenda-setter) experiences this constraint as genuine coordination: establishing unified authority prevents vigilantism and ensures consistent, procedurally fair application. From their seat the deterrent effect justifies the extraction of the executed offender's life. The executed offender (powerless target) and their families experience this constraint as pure extraction justified by a causally uncertain claim (whether execution actually deters). Criminology researchers (observers) see a constraint whose justification depends entirely on evidence they have not decisively settled — deterrence efficacy remains contested. Future potential victims (anonymous beneficiaries) cannot verify they are benefiting because the benefit is the absence of a crime that would have occurred, which is structurally unobservable. The disagreement is not over values but over whether the postulated mechanism (deterrence) operates as claimed.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed offenders have d→1.0 (full targets): they are convicted of capital crimes, sentenced to death, and have exhausted legal exits; death is the direct cost to them. Families of executed have d→1.0 (full targets): they experience loss without consent or exit. Future potential crime victims have d→0.0 (full beneficiaries): they benefit from reduced murder incidence without bearing direct cost. Crime-deterred persons are near 0.5 (symmetric): they benefit from being free and alive, but this benefit is available to them only because they did NOT commit the crime — the constraint's benefit is conditional on their own criminal restraint. Abolition advocates have d→1.0 (full targets of suppression): the constraint directly suppresses their moral objections and prevents their alternative reading from being seated. Wrongfully convicted persons have d→1.0 (full targets): they are extracted at error, bearing the constraint's uncertainty cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading avoids premature mandatrophy classification because the founding problem ('reduce capital crime through maximal deterrence') remains live and contested, not decisively dead. However, the constraint exhibits a pre-mandatrophy condition: growing evidence that life imprisonment deters equally well. If that evidence were to become decisive (falsifying the deterrence claim), the founding problem would shift from 'live' to 'dead' (the problem is solved better by alternative means). At that point the constraint would meet the mandatrophy pattern: a rule whose original function is no longer uniquely performed by the rule itself, yet the rule persists because of path dependence and institutional inertia. Currently the constraint is tangled_rope because coordination function (unified state authority, procedural fairness) plus asymmetric extraction (executed offender pays the cost) are both present. If deterrence were falsified, the constraint would reclassify toward piton (execution persists because it is established and habitual, not because it deters; procedural safeguards become pure theater maintaining a function that no longer operates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical,
    'Does execution deter capital crimes more effectively than life imprisonment without parole?',
    'Systematic review of econometric criminology literature, accounting for methodological variation across studies. Natural experiments from jurisdictions that abolished execution while keeping life imprisonment. Meta-analysis of deterrence elasticity estimates.',
    'If life imprisonment deters equally or better, the deterrence reading''s structural justification transfers to that mechanism, making execution pure extraction. Classification would shift from tangled_rope (coordination + justified extraction) to snare (extraction masked by false causal claim). The founding_problem would shift from ''live'' to ''dead'' (solved better by alternative), triggering mandatrophy pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical, empirical, 'Whether execution operates the postulated deterrent mechanism with greater efficacy than alternatives.').

omega_variable(
    wrongful_execution_error_rate,
    'What is the actual error rate (conviction of innocent persons) in capital cases, and what is the baseline error rate that preserves the deterrence reading''s utilitarian justification?',
    'Retrospective analysis of exonerations from DNA evidence and post-conviction review. Comparison of capital-case error rates to non-capital serious felonies. Threshold analysis: at what error rate does the utilitarian cost of executing the innocent exceed the utility of deterrence?',
    'High error rates undermine the deterrence reading''s framework because executing the innocent produces pure cost (deterrence effect applies only to the guilty). If error rate exceeds a critical threshold, the reading becomes unjustifiable on its own terms. This feeds the omega above: if deterrence efficacy is uncertain AND error rates are high, the reading collapses entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_error_rate, empirical, 'Whether the constraint''s error rate is consistent with its utilitarian justification.').

omega_variable(
    competing_reading_foreclosure,
    'Does the deterrence reading''s empirical premise (execution deters better than alternatives) logically foreclose the retributive and abolition readings, or do those readings remain live positions in the same legal framework?',
    'Conceptual analysis: the retributive reading depends on moral proportionality (past crime justifies punishment), which does not logically depend on deterrence at all; the abolition reading depends on categorical moral status (the state lacks the right to kill), which is not answered by efficacy evidence. Neither reading is logically foreclosed by deterrence evidence.',
    'If neither reading is foreclosed, the three readings coexist_with each other (they compete across jurisdictions and factions, but no single framework admits all three simultaneously). If one reading could foreclose another, the classification would be forecloses rather than coexists_with. This affects the reading_relations field in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether the deterrence reading''s logical structure forecloses or coexists with the retributive and abolition readings.').

omega_variable(
    substitution_transparency,
    'If life imprisonment were adopted as the maximum penalty, would the deterrent effect that currently attaches to execution transfer to life imprisonment, or is deterrence specific to the irreversibility of execution?',
    'Comparative study of crime rates in jurisdictions using life imprisonment as the maximum penalty, controlling for other factors. Theoretical analysis of rational-choice criminology: does the threat of life imprisonment (0% escape probability, 100% incapacitation) deter as effectively as execution?',
    'If deterrence transfers to life imprisonment, the reading remains intact but the constraint becomes unnecessary — execution is not the only mechanism that deters at the required level. If deterrence is specific to execution''s irreversibility, the reading retains uniqueness. Either way, this omega relates to the first omega (efficacy comparison).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_transparency, empirical, 'Whether deterrence effect depends on the irreversibility of execution or can be achieved by alternative severe penalties.').

omega_variable(
    future_victim_anonymity,
    'Can the deterrence reading''s beneficiary (future potential victims who do not exist yet) be meaningfully included in the justificatory framework, or does their anonymity make them unsuitable as a moral/political constituency?',
    'Philosophical and political analysis: how do contemporary frameworks (rights, dignity, democratic consent) treat claims grounded in preventing future unidentified harms? Comparison to other constraints justified by preventing future risks (pandemic preparedness, climate policy, nuclear regulation).',
    'If future victims are unsuitable as a constituency (they cannot consent, verify benefit, or participate in the decision), the deterrence reading''s beneficiary class becomes abstract to the point of insubstantiality. Extraction becomes extraction BY the state FOR an anonymous future, which may be difficult to distinguish from pure state power. This supports reclassification toward snare (the beneficiaries exist only in the reading''s justificatory narrative, not in observable political reality).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_victim_anonymity, conceptual, 'Whether unidentified future victims can ground a legitimate political justification for present costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.33).
narrative_ontology:measurement(stat_tr_t5, state_execution_authority__deterrence_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__deterrence_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(stat_tr_t15, state_execution_authority__deterrence_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__deterrence_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(stat_tr_t25, state_execution_authority__deterrence_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__deterrence_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(stat_be_t5, state_execution_authority__deterrence_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__deterrence_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(stat_be_t15, state_execution_authority__deterrence_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__deterrence_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(stat_be_t25, state_execution_authority__deterrence_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__deterrence_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t5, state_execution_authority__deterrence_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__deterrence_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(stat_su_t15, state_execution_authority__deterrence_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__deterrence_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(stat_su_t25, state_execution_authority__deterrence_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__deterrence_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__deterrence_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% The state_execution_authority kernel instantiates three constraint stories, one per reading. The deterrence_reading (this story) claims execution prevents future murders through cost-raising; the retributive_reading claims execution restores moral balance; the abolition_reading claims execution is categorically impermissible. All three share the kernel (state authority over capital punishment) but differ in their justificatory structure (consequentialist vs. retributive vs. deontological). The ε values differ substantially: deterrence_reading has moderate ε (0.58) because its justification depends on empirical efficacy and can be substituted if life imprisonment deters equally; retributive_reading has higher ε because proportionality does not depend on future outcomes; abolition_reading has ε ≈ 0.0 because it rejects the constraint entirely. The three readings coexist across different jurisdictions and political factions but compete within the same legal framework — they cannot all be true simultaneously in a single authority's practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__deterrence_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
