% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution as Deterrent Against Capital Crime
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   Under the deterrence reading, state execution of capital offenders is
 *   justified as a mechanism to prevent future murders by raising the cost of
 *   such crimes above the threshold where rational actors would commit them.
 *   Future potential victims are cast as the beneficiary set (receiving the
 *   preventive protection), while executed offenders and their families bear
 *   the cost. The constraint's persistence depends on maintained belief in
 *   deterrent efficacy and active suppression of the abolition and
 *   retributive alternative readings. This reading instantiates a utilitarian
 *   logic: the state killing of one guilty offender is justified by the
 *   prospective prevention of multiple innocent killings. Wrongful execution
 *   is a system failure requiring error-rate minimization, not a categorical
 *   impermissibility. This is ONE reading of the contested kernel 'state
 *   execution authority'—not a description of all readings or an attempt to
 *   reconcile them.
 *
 * KEY AGENTS:
 *   - future_potential_victims: beneficiary, diffusely distributed, unable to voice preferences, benefit is counterfactual
 *   - executed_offender: payer (powerless), bears ultimate cost under instrumental logic, trapped by judicial process
 *   - criminal_justice_authorities: agenda_setter (institutional), custodian of deterrence mechanism, arbitrage exit available
 *   - criminological_researchers: observer (analytical), measure efficacy empirically, findings contested
 *   - abolition_advocates: excluded, hold categorical premise (incompatible with deterrence frame)
 *   - wrongfully_convicted: payer (powerless), represent irreversible error cost
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
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution as Deterrent Against Capital Crime").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '4226cb08-a5fa-44ad-80f2-b0d0399d7141').
narrative_ontology:cs_kernel_codification('4226cb08-a5fa-44ad-80f2-b0d0399d7141', fixed_text).
narrative_ontology:cs_authority_grounding('4226cb08-a5fa-44ad-80f2-b0d0399d7141', lineage).
narrative_ontology:cs_interpretation_layer_present('4226cb08-a5fa-44ad-80f2-b0d0399d7141').
narrative_ontology:cs_reading_relation('4226cb08-a5fa-44ad-80f2-b0d0399d7141', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('4226cb08-a5fa-44ad-80f2-b0d0399d7141', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('4226cb08-a5fa-44ad-80f2-b0d0399d7141', foundational, future_victim_benefit_justifies_state_killing).
narrative_ontology:cs_axiom_status(future_victim_benefit_justifies_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('4226cb08-a5fa-44ad-80f2-b0d0399d7141', future_victim_benefit_justifies_state_killing, empirically_contingent).
narrative_ontology:cs_axiom('4226cb08-a5fa-44ad-80f2-b0d0399d7141', foundational, rational_actors_respond_to_cost_incentives).
narrative_ontology:cs_axiom_status(rational_actors_respond_to_cost_incentives, holdable).
narrative_ontology:cs_axiom_grounding('4226cb08-a5fa-44ad-80f2-b0d0399d7141', rational_actors_respond_to_cost_incentives, empirically_contingent).
narrative_ontology:cs_reference_frame('4226cb08-a5fa-44ad-80f2-b0d0399d7141', rational_deterrence_framework).
narrative_ontology:cs_drift_state('4226cb08-a5fa-44ad-80f2-b0d0399d7141', contemporary_post_meta_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4226cb08-a5fa-44ad-80f2-b0d0399d7141', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, law_abiding_citizens).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offender).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, offender_family_members).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced likelihood of capital murder due to the elevated cost imposed on would-be perpetrators. They are diffusely distributed, unknown, and unable to organize or voice preferences about the constraint. Their benefit is prospective and counterfactual — prevention of harms that do not materialize.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, generational, trapped, national).

% Experience reduced fear of violent predation when execution is presented as deterrent. The benefit is partly psychological (assurance of state protection) and partly empirical (if deterrence is efficacious). They organize through electoral and legislative processes to maintain the constraint.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, law_abiding_citizens, beneficiary,
    organized, generational, constrained, national).

% Bears the ultimate cost: loss of life. Under the deterrence reading, the offender is treated as an instrument — their death is justified by its future preventive effect on others, not by proportionality to their crime or inherent desert. The offender has no exit: judicial process is the only gateway, and they exhaust it before execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offender, payer,
    powerless, immediate, trapped, local).

% Suffer the loss of a relative, social stigma, and psychological trauma. Under deterrence framing they are collateral costs — not the target of the constraint, but bearing its burden. They have constrained exit: they may advocate for clemency or procedural change, but the constraint's enforcement machinery is designed to override their voice.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, offender_family_members, payer,
    moderate, biographical, constrained, local).

% Represent the irreversible error cost of the deterrence system. Under the deterrence reading, wrongful execution is a utilitarian loss — a failure of the error-detection machinery that the constraint depends on. No exit exists; they are trapped until exoneration (which may come too late) or execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted, payer,
    powerless, immediate, trapped, local).

% Design, enforce, and administer the capital punishment system. They set prosecution policy, oversee appeals, and carry out executions. Under deterrence framing they are custodians of a prevention mechanism. They have arbitrage: they can shift the constraint's terms (e.g., adopt life-without-parole as substitute) if deterrence efficacy can be maintained.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminal_justice_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Authorize and maintain the constraint through electoral and legislative process. They respond to constituent demand for security and can shift the constraint's scope (expand/contract eligible crimes) or substitute alternative mechanisms (life-without-parole, longer sentences). They have mobile exit: they can shift to alternative deterrent structures without abandoning deterrence entirely.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, legislators_and_voters, agenda_setter,
    organized, generational, mobile, national).

% Study the empirical question: does execution deter capital murder? Their findings are cited by all seats but are structurally independent of the constraint. They occupy an observer position — they measure the constraint's efficacy but do not set its terms. A consensus that execution does NOT deter would undermine the deterrence reading's justification.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminological_researchers, observer,
    analytical, biographical, analytical, global).

% Hold the foundational premise that execution is categorically impermissible (the abolition reading) and are structurally excluded from the deterrence reading's framework. They would argue that no preventive benefit can justify state killing, but this argument is ruled out by the deterrence reading's core axiom. They organize to change law but their voice is outside the deterrence frame.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Hold that execution is justified by moral desert and proportionality to heinous crime (the retributive reading), not by deterrent effect. They occupy a different normative frame than the deterrence reading and would dispute that future-victim benefit is the legitimate basis for execution. Their argument competes with deterrence framing in public discourse but is not integrated into the deterrence reading's structure.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, retributive_justifiers, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, criminal_justice_authorities).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified state enforcement mechanism that raises the cost of capital crime above the point where rational actors would commit it, coordinating individual deterrence effects into a system-wide reduction in homicide. Solves the collective-action problem of distributed security: rather than each potential victim family arming and pursuing private vengeance, the state assumes the role of deterrent authority.
% TRANSFER_FUNCTION: Transfers the risk of death from potential future victims (beneficiary set) to those convicted of capital murder and their families. Moves the authority to determine death from individual or family (vigilante justice) to the state. Also transfers resources from taxpayers to the criminal justice system for prosecution, appeals, and execution administration.
% ABSENT_VOICES: The future potential victims whose benefit the deterrence reading claims cannot speak to whether they want to benefit through state killing. Abolition advocates (who would argue no preventive benefit justifies execution) and retributive justifiers (who would argue proportionality, not prevention, is the basis) are structurally excluded from the deterrence frame itself — they would object to the reading's foundational premise. Wrongfully convicted and their families are silenced by irreversible error.
% DISAPPEARANCE_RATIONALE: If execution as deterrent vanished, homicide rates would shift (whether up, down, or unchanged is the empirical question); criminal justice authorities would substitute alternative deterrence mechanisms (life-without-parole with genuine finality, longer sentences) or abandon deterrence-framing entirely; the resource flow to capital cases would redirect; family members of victims would lose the claimed preventive assurance. The constraint's removal reorganizes the incentive structure for would-be murderers and the justification structure for the state.
% FOUNDING_PROBLEM: Capital murder (premeditated killing, often of innocents) imposes irreversible harm on victims and communities. Rational cost-raising can reduce the frequency of rational actors committing such crimes by making the expected utility of the crime negative.
% FOUNDING_PROBLEM_CORROBORATION: Criminal justice authorities and deterrence-framing legislators attest the founding problem is live and execution is the solution. Criminological research on deterrence is deeply contested: meta-analyses by Nagin (2013) and others find no credible evidence that execution deters beyond the general deterrent effect of certainty and severity of punishment (which could be achieved without execution); advocates for deterrence cite Cebula (2013) and Shepherd (2005) as showing modest deterrent effects. No consensus exists among practitioners or researchers outside the benefiting parties. Legislative history and victim advocacy groups support deterrence-framing; international criminological consensus (exemplified by Amnesty International and the UN Mandela Rules) opposes execution as ineffective and disproportionate. The founding problem is alive but the mechanism is unvalidated.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Base extractiveness sits at 0.58 because the deterrence reading's justification depends entirely on efficacy: if execution does not deter (or deters no better than life-without-parole), the constraint becomes pure extraction. The measured value reflects the empirical contestation: criminological consensus has not validated deterrent effect above general-severity effects, so the extraction is substantial and the justification is weak. Suppression is higher (0.72) because the constraint requires active suppression of alternative readings (abolition and retributive frames), which would substitute different cost-benefit analyses. Theater ratio is moderate (0.41) and rising: the constraint's educational and symbolic function (deterrence through visibility of execution) is substantial, but forensic analysis shows most capital cases involve incapacitation rather than marginal deterrence. The measurement series shows extractiveness rising to T=25 (peak judicial confidence in the deterrent mechanism) and then stabilizing as meta-analyses accumulate showing null deterrent effects. Suppression remains constant because the political commitment to the constraint does not abate despite empirical doubt. Theater ratio continues rising as the mechanism becomes increasingly ceremonial relative to functional (the constraint persists through symbol and political will, not through demonstrated deterrence).
 *
 * PERSPECTIVAL GAP:
 *   The deterrence reading treats the executed offender as instrumental: their death is justified by its causal effect on others' behavior. This utilitarian logic forecloses the retributive reading's frame (which treats execution as proportionate punishment for inherent desert) but coexists with it in practice—different jurisdictions and parties hold different readings simultaneously. The abolition reading is excluded not by logical foreclosure but by the deterrence frame's foundational axiom: if you accept that future-victim benefit can justify state killing, you have already rejected the abolition reading's categorical claim. The deterrence reading does not logically foreclose abolition; rather, it operates within a different normative universe where prevention-justification is possible. This is coexistence, not foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Future-potential-victims occupy the beneficiary end (d ≈ 0.1–0.2): they receive claimed protection from the constraint, though that protection is counterfactual and unverifiable. Law-abiding citizens are near-beneficiary (d ≈ 0.2–0.3): they receive psychological assurance and (if deterrence works) real safety, but the protection is diffuse and the constraint's persistence does not depend on their continued support — it depends on institutional commitment. The executed offender is full-target (d = 1.0): death is the cost, they bear it irreversibly. Offender family members are high-target (d ≈ 0.85): they bear collateral trauma and stigma, have constrained exit, and receive no benefit. Wrongfully convicted are full-target (d = 1.0): irreversible error, no exit. Criminal justice authorities are near-beneficiary (d ≈ 0.15): they benefit from institutional authority and resource flow but are not primary beneficiaries; they can exit to alternatives. Abolition advocates are excluded (d = undefined in this frame): their voice is structural outside the deterrence reading. Criminological researchers are analytical observer (d undefined).
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading declares a founding problem ('capital murder imposes irreversible harm, cost-raising can deter rational actors') and a mechanism ('execute offenders to raise the cost'). The empirical literature (Nagin 2013, meta-analyses from 2005–2020) shows no credible evidence that execution deters beyond general-severity effects. The founding problem is still live (homicides occur), but the mechanism is unvalidated. This is classic mandatrophy in formation: the justification (deterrence) is not corroborated outside the benefiting parties (criminal justice and victim advocacy); the procedure persists through institutional inertia and political will, not through demonstrated function. The constraint does NOT compute as rope (pure coordination with participant benefit) because the victims cannot validate the benefit and the executory cost is irreversible. It computes as tangled_rope because there IS a genuine coordination function (state-managed security replacing private vengeance, reducing vigilante escalation), but it is asymmetric: the mechanism's persistence depends on suppressing alternatives (abolition, retributive) and on unvalidated claims about deterrence. The theater_ratio rising from 0.25 to 0.41 signals that the constraint is increasingly performative—the ritual of execution persists as symbol while the empirical deterrent effect remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical,
    'Does execution deter capital murder more effectively than alternative severe punishments (life-without-parole, lengthy sentences)?',
    'Meta-analysis of criminological literature (Nagin 2013, later Cochrane reviews); natural experiments from jurisdictions that abolished execution (compare homicide rate trajectories); controlled econometric studies isolating deterrent effect from incapacitation effect.',
    'If execution shows no credible deterrent effect beyond severity/certainty, the constraint''s justification collapses and extractiveness becomes ~0.9 (pure extraction); the deterrence reading becomes indefensible as a normative frame. If execution deters (modest effect shown), extractiveness stays ~0.58 (moderate—efficacy is unproven but claimed). If life-without-parole deters equally, substitute substitutes itself and execution becomes redundant extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical, empirical, 'Whether execution prevents capital murders more effectively than alternatives.').

omega_variable(
    wrongful_execution_rate_irreversibility,
    'At what error rate does the utilitarian calculus (n executions preventing m future murders) break down and become indefensible?',
    'Empirical estimates of wrongful conviction rate in capital cases (currently ~4%, Gross et al. 2014); counterfactual comparison of prevented murders under deterrence to innocent deaths under execution error.',
    'If error rate is high (>2%) and prevented murders cannot be quantified, the utilitarian logic becomes incoherent—the constraint is killing innocents under an unvalidated efficacy claim. This would push the constraint toward snare (pure extraction under cover of false deterrence). If error rate is low (<0.5%) and deterrence is proven, the constraint remains tangled_rope (genuine coordination with asymmetric cost). If error rate is high AND deterrence is proven, the constraint is a tragic necessity with legitimacy cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_rate_irreversibility, empirical, 'Whether wrongful-execution errors invalidate the deterrence utilitarian calculus.').

omega_variable(
    reading_foreclosure_logical_structure,
    'Does the deterrence axiom (''future-victim benefit can justify state killing'') logically foreclose the abolition axiom (''state killing is categorically impermissible''), or do they coexist as incommensurable frames held by different parties?',
    'Conceptual analysis: a logical foreclosure requires that accepting deterrence-justification necessitates rejecting categorical abolition within the same normative framework. A coexistence admits that abolitionists and deterrence-framers hold incompatible axioms but neither can prove the other''s axiom false.',
    'If foreclosure: the deterrence reading is incompatible with abolition at the framework level, and the constraint''s persistence requires suppressing the abolition reading''s voice (high suppression justified by incompatibility). If coexistence: both readings remain live, the suppression is political (not logical), and the contest is indefinite. Affects the classification of ''reading_relations'': forecloses vs. coexists_with. This is a question about the logical structure of the readings, not about empirical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_logical_structure, conceptual, 'Whether deterrence and abolition readings are logically incompatible or politically competing.').

omega_variable(
    instrumental_cost_assignment_asymmetry,
    'Is the assignment of instrumental cost (death of the offender) to the executed party justified by their causal role in the capital crime, or is it arbitrary allocation of a necessary cost to the available powerless party?',
    'Philosophical analysis of causal responsibility and just distribution of burden; comparison to alternative cost-assignment models (e.g., restorative justice distributing harm-repair across offender and community).',
    'If justified: the constraint remains tangled_rope with asymmetric burden legitimized by causal responsibility. If arbitrary: the constraint becomes snare (pure extraction of instrumental cost from powerless parties to benefit diffuse beneficiaries). Affects suppression assessment—suppression of what alternatives?—and the legitimacy of the beneficiary/victim distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumental_cost_assignment_asymmetry, conceptual, 'Whether instrumental cost allocation to the offender is justified by causal role or imposed by power asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t25, state_execution_authority__deterrence_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t35, state_execution_authority__deterrence_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(stat_tr_t35, observed).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t25, state_execution_authority__deterrence_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t35, state_execution_authority__deterrence_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement_basis(stat_be_t35, observed).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t25, state_execution_authority__deterrence_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t35, state_execution_authority__deterrence_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(stat_su_t35, observed).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__deterrence_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% The kernel 'state execution authority' instantiates three distinct constraint stories, each a different normative reading: deterrence_reading (this file) treats execution as justified by prevention of future murders; retributive_reading treats execution as justified by proportionate punishment for heinous crime; abolition_reading treats execution as categorically impermissible regardless of crime or procedure. These are not alternative measurements of the same constraint—they are three different constraints grounded in the same practice but operating from incompatible axioms. Each reading authors its own ε (efficacy/justification), its own beneficiary/victim structure, its own classification. The deterrence reading claims moderate extractiveness (0.58) because its justification depends on unvalidated empirical efficacy; the retributive reading would claim moderate extractiveness justified by proportionality; the abolition reading would claim high extractiveness (the entire practice is pure extraction of life without legitimate justification). Each story is linked to its siblings via network.affects_constraints to mark the shared practice under contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
