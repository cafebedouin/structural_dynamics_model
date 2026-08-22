% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   State execution in the abolition reading is a categorical prohibition:
 *   the state possesses no legitimate authority to impose the death penalty,
 *   regardless of the crime's severity, the offender's culpability, or the
 *   quality of procedural safeguards. All executed persons — including those
 *   guilty of heinous crimes — are structural victims of an irreversible
 *   extraction justified by retribution or deterrence narratives that the
 *   abolition reading rejects as illegitimate cover stories. This reading
 *   emerges from 20th-century human rights law and deontological philosophy
 *   that treats the right to life as inalienable and state killing as a
 *   per-se rights violation. The constraint under this reading has no
 *   beneficiaries in the genuine sense: executing jurisdictions may derive
 *   political benefit (appearing tough on crime, satisfying retributive
 *   constituencies), but the abolition reading treats those benefits as
 *   products of false consciousness or as reliance on extraction, not as
 *   coordination functions.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus: Administers capital punishment; defends it as serving legitimate state ends (deterrence, retribution, incapacitation); treats the constraint as justified
 *   - executed_persons: Undergo irreversible loss of life; powerless to resist; victims under the abolition reading
 *   - death_row_prisoners: Await execution in identity-locked status (criminal-justice identity with trapped exit); suppression is total
 *   - families_of_executed: Suffer collateral extraction (grief, legal costs, social stigma); constrained exit through political organizing
 *   - abolitionist_movements: Excluded from decision-making in executing jurisdictions; their objections are moral argument, not binding testimony
 *   - wrongfully_convicted_exonerees: Proof of systemic fallibility; their existence demonstrates the constraint's irreversible harm
 *   - liberal_democracies_banning_execution: Structural evidence that execution is non-essential; their functioning without capital punishment undermines retribution and deterrence justifications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority (Abolition Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'b8b558da-3306-4b3f-93eb-92e1de06c051').
narrative_ontology:cs_kernel_codification('b8b558da-3306-4b3f-93eb-92e1de06c051', formalized).
narrative_ontology:cs_authority_grounding('b8b558da-3306-4b3f-93eb-92e1de06c051', extraction).
narrative_ontology:cs_interpretation_layer_present('b8b558da-3306-4b3f-93eb-92e1de06c051').
narrative_ontology:cs_reading_relation('b8b558da-3306-4b3f-93eb-92e1de06c051', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('b8b558da-3306-4b3f-93eb-92e1de06c051', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('b8b558da-3306-4b3f-93eb-92e1de06c051', foundational, human_right_to_life_inalienable).
narrative_ontology:cs_axiom_status(human_right_to_life_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('b8b558da-3306-4b3f-93eb-92e1de06c051', human_right_to_life_inalienable, deontological).
narrative_ontology:cs_axiom('b8b558da-3306-4b3f-93eb-92e1de06c051', foundational, state_killing_categorical_impermissibility).
narrative_ontology:cs_axiom_status(state_killing_categorical_impermissibility, holdable).
narrative_ontology:cs_axiom_grounding('b8b558da-3306-4b3f-93eb-92e1de06c051', state_killing_categorical_impermissibility, deontological).
narrative_ontology:cs_reference_frame('b8b558da-3306-4b3f-93eb-92e1de06c051', human_rights_framework).
narrative_ontology:cs_drift_state('b8b558da-3306-4b3f-93eb-92e1de06c051', contemporary_global_abolitionism, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('b8b558da-3306-4b3f-93eb-92e1de06c051', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, death_row_prisoners).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, victims_of_capital_crimes).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, executing_jurisdictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital punishment through legislation, appellate jurisprudence, and execution protocols. Claims execution serves legitimate state interests (retribution, deterrence, incapacitation). Maintains the constraint through active enforcement: appellate restrictions on exculpatory evidence, procedural rules that limit appeals, and legitimacy-theater that reframes execution as just punishment. Has constitutional authority to set policy and chooses to retain execution despite abolition being available (arbitrage exit).
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Undergo irreversible loss of life through state execution. Under the abolition reading, all executed persons — including those guilty of heinous crimes — are victims of categorical rights violation. They have no alternative to execution, no appeal that can save them once procedures are exhausted, and no remedy if error is discovered. Their power is zero and their exit options are zero: death is final.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, national).

% Live under extreme psychological and legal duress awaiting execution. They are locked into the criminal-justice identity by conviction and incarceration; their identity has fused with their legal status ('condemned person'). Escape depends entirely on state discretion (pardon, exoneration, commutation). The death sentence is the immediate enforcement mechanism; the suppression is total because they cannot act in the world outside the prison system.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_row_prisoners, payer,
    powerless, biographical, identity_locked, national).

% Suffer permanent loss of a family member and collateral extraction: legal costs from appeals, emotional trauma, and social stigma. They can organize politically (constrained exit through activism) but face institutional resistance from executing-jurisdiction governments and death-penalty-supporting publics.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed, payer,
    moderate, generational, constrained, national).

% Seek justice and closure after serious crimes. Some support execution as retribution; others oppose it, viewing state killing as compounding trauma. Under the abolition reading, neither retributive satisfaction nor closure justifies the extraction from the condemned person. They benefit from the legal system's accountability mechanisms but not from execution itself; some members of this group actively oppose execution (mobile exit through abolitionist organizing).
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, victims_of_capital_crimes, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__abolition_reading, victims_of_capital_crimes, observer).

% Claim execution deters capital crime and provides proportionate retribution for heinous crimes. Politically, they benefit from execution's appearance of swift, severe justice, which satisfies crime-control constituencies and generates electoral support. The constraint persists because these jurisdictions benefit from its maintenance and choose not to abolish it despite abolition being available as an option (arbitrage exit: they could abandon execution but do not).
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executing_jurisdictions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__abolition_reading, executing_jurisdictions, beneficiary).

% Advocate for categorical abolition of state execution, citing human rights, the irreversibility of execution error, and the moral impermissibility of state killing. They are excluded from the decision-making apparatus in executing jurisdictions: their testimony is not solicited, their moral objections are treated as non-binding values rather than binding claims on policy. They can exit (mobile: their activism moves between jurisdictions and between issue domains) but face structural barriers to influence.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_movements, excluded,
    powerful, generational, mobile, global).

% Survivors who escaped the execution apparatus through exoneration. Their existence proves the constraint's irreversible harm: had they been executed, no remedy would exist. They serve as evidence that procedural safeguards are insufficient to prevent execution error. Their testimony is powerful but often dismissed by executing jurisdictions as outlier cases or as arguments for reform (better procedures) rather than abolition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_exonerees, observer,
    powerful, biographical, mobile, national).

% Have abolished execution and treat capital punishment as incompatible with human rights law. They provide structural proof that modern legal systems can function and maintain public safety without capital punishment, undermining deterrence and retribution justifications. Their existence is inconvenient to the apparatus: it demonstrates that execution is not necessary, that abolition is a live option available to any jurisdiction.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, liberal_democracies_banning_execution, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, executing_jurisdictions).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The abolition reading rejects the framing of execution as coordination. No genuine collective-action problem is solved by state execution; the constraint offers no coordination benefit.
% TRANSFER_FUNCTION: Moves human life from the condemned person to the state apparatus, which gains closure-theater, political benefit (appearing tough on crime), and legitimacy in the eyes of retributive constituencies.
% ABSENT_VOICES: Abolitionist movements and wrongfully convicted exonerees are excluded from executing jurisdictions' decision apparatus. Their objections are treated as non-binding moral argument rather than binding testimony. International human rights bodies and liberal democracies that have abolished execution are structurally marginal to the apparatus's decision-making.
% DISAPPEARANCE_RATIONALE: If state execution disappeared, criminal justice would reorganize around life imprisonment as the maximum penalty. Retributive narratives would seek alternative expressions; deterrence would operate through certainty of prosecution rather than severity of punishment. The legal and penal infrastructure would shift to treat life-without-parole as the boundary constraint, which modern liberal democracies already do without significant social disruption.
% FOUNDING_PROBLEM: Early modern states lacked the capacity for secure, permanent incapacitation without execution. Long-term prisons were expensive and dangerous; escape and uprising were common risks. Execution was the only method available to achieve permanent incapacitation of the most dangerous offenders.
% FOUNDING_PROBLEM_CORROBORATION: Modern liberal democracies have abolished execution and rely exclusively on life imprisonment; homicide rates in abolitionist jurisdictions (EU, Canada, Australia) are comparable to or lower than execution-retaining jurisdictions (US, Japan); wrongful-conviction research funded by abolitionist organizations and mainstream academic criminology attests that procedural safeguards cannot eliminate execution error. The founding problem is demonstrably solved by life imprisonment. Executing jurisdictions continue the practice despite the founding problem's resolution, which is the key evidence for the mandatrophy claim: the constraint persists as zombie infrastructure maintained by political benefit and retributive narratives, not by necessity.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.95, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximal (0.95 at interval end) because the constraint removes the ultimate good — human life — and offers no substitute, no appeal, no exit, and no remedy if error occurs. Under the abolition reading, the very premise that execution can ever be justified is rejected; thus there is no metric value for extractiveness that would satisfy the constraint. Suppression is very high (0.88) because the constraint's persistence depends on silencing or marginalizing abolitionist objections, restricting access to exculpatory evidence in appellate review, and treating the foundational moral objection (the right to life) as a non-binding value preference rather than a binding human right. Theater ratio is moderate (0.42) because the constraint does real institutional work (it processes serious crimes, provides closure-theater, generates legitimacy-narratives) but an increasing share of its operation is pure defense of the extraction mechanism itself rather than the coordination functions retribution and deterrence supposedly serve. Accessibility collapse is very high (0.91): once a person is condemned and appeals are exhausted, no alternative remains; death-row conditions and the finality of execution make the collapse near-complete. Resistance is substantial (0.72): abolitionist movements, wrongful-conviction exonerees, and international human rights bodies resist the constraint, but their resistance is muted by the structural exclusion of abolitionists from executing jurisdictions' decision apparatus and by public opinion in death-penalty states that continues to support execution.
 *
 * PERSPECTIVAL GAP:
 *   The executing-jurisdiction agenda-setter seat and the executed-person victim seat compute radically differently. The apparatus experiences the constraint as legitimate punishment and deterrence machinery it administers in service of public safety; the abolition reading denies any such legitimacy and treats the apparatus's justifications as cover stories for extraction. From the apparatus's seat, execution is a necessary but regrettable part of proportionate justice; from the victim's seat (and under the abolition reading), it is an irreversible rights violation with no justification. The wrongfully-convicted-exoneree seat experiences the constraint as having nearly killed them; their survival is contingent on evidentiary discovery that should not have been necessary had appeals been truly independent. This perspective — that procedural safeguards failed and can fail — directly contradicts the apparatus's framing of execution as safe when procedures are followed.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons are the full target (d=1.0): the constraint extracts their life, offers no alternatives, and is enforced against their interests completely. Death-row prisoners sit near the target end (d~0.95): they face identity-lock (criminal-justice identity) and trapped exit; escape depends wholly on state discretion. Families of the executed have high d (~0.75): they suffer collateral extraction and constrained exit through political organizing, which is mobile but faces institutional resistance. The executing-jurisdiction agenda-setter has low d (~0.1-0.2): it benefits from the constraint's enforcement, collects political rents, and has arbitrage-grade exit (it could abolish execution but chooses not to). The apparatus derives its d from the beneficiary declaration — it receives legitimacy-narratives and political benefit — but under the abolition reading those are not genuine benefits; they are products of false consciousness about what justice requires. The derivation chain produces d~0.1 from the beneficiary position, which is correct for the apparatus's structural position: it maintains the constraint against the victim's interest and faces real resistance from abolitionist movements, but its power-atom (institutional) and exit options (arbitrage: it could dismantle execution) keep it at the beneficiary end. This is structurally accurate: a powerful actor that derives benefit from a constraint and can choose to maintain or abandon it sits at the beneficiary end, even if (under a reading) the claimed benefit is morally illegitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolition reading's mandatrophy claim is central: the founding problem (the need for irreversible incapacitation of dangerous offenders in the absence of secure long-term incarceration) is demonstrably dead. Modern liberal democracies have abolished execution and rely exclusively on life imprisonment without parole; their homicide rates are comparable to or lower than execution-retaining jurisdictions; wrongful-conviction research shows that procedural safeguards cannot eliminate execution error. The constraint persists despite the founding problem's resolution because: (1) executing jurisdictions benefit politically from execution's appearance of swift, severe justice; (2) retributive constituencies continue to demand execution as proportionate punishment; (3) deterrence beliefs persist despite criminological evidence to the contrary; (4) abolitionists are excluded from the decision apparatus, so their moral objections do not enter the cost-benefit analysis. The classification as snare (pure extraction) rather than tangled_rope (hybrid coordination/extraction) depends on this mandatrophy analysis: if execution served a genuine coordination function (incapacitation that could not be served otherwise, or deterrence that materially reduced serious crime), it would be tangled rope — coordination benefit to the public, extraction from the condemned. But the abolition reading rejects both justifications: incapacitation is achieved equally or better by life imprisonment; deterrence is either false (evidence suggests no deterrent effect) or morally illegitimate (the state has no right to kill to deter, even if killing did deter). Thus the constraint is pure extraction riding on dead justifications, which is snare, not tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_authority,
    'Is the state''s authority to execute a natural right (derived from the nature of sovereignty and punishment) or a constructed authority (a particular institutional choice)?',
    'Genealogical analysis of execution as a state practice: does it predate the modern state form (suggesting natural law status) or emerge contingently with particular institutional developments? Comparative constitutional law: do all sovereign states require execution authority, or is abolition compatible with sovereignty?',
    'If execution authority is natural law, the abolition reading may be logically incoherent (the state cannot renounce what it naturally possesses); if constructed, abolition is a legitimate policy choice among alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether state execution authority is a natural entailment of sovereignty or a contingent institutional choice').

omega_variable(
    suppression_mechanism_decomposition,
    'What proportion of the measured suppression (0.88) is structural (legal barriers, appellate restrictions, isolation) versus internalized (identity fusion, normalization, belief in deserved punishment)?',
    'Post-release trajectory study of exonerated death-row survivors: does suppression persist after all structural barriers are removed? Clinical assessment of identity-reintegration in commuted prisoners versus those released after wrongful conviction.',
    'If suppression is primarily structural, removal of legal barriers would enable resistance; if primarily internalized, the constraint carries its suppression with the victim even after release, and the actual suppression is higher than the measured figure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_decomposition, empirical, 'Structural versus internalized suppression in death-row constraint').

omega_variable(
    deterrence_empirical_claim,
    'Does execution actually deter murder, or is the deterrence claim an empirically false rationalization?',
    'Meta-analysis of criminological studies comparing homicide rates in execution-retaining versus execution-abolishing jurisdictions, controlling for socioeconomic variables, policing intensity, and arrest-to-conviction rates.',
    'If execution has no deterrent effect, the deterrence reading loses its empirical grounding; if execution does deter materially, the abolition reading must rely on the moral argument (the state has no right to kill to deter) rather than the empirical argument (execution doesn''t deter anyway).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_claim, empirical, 'Whether execution serves a genuine deterrent function').

omega_variable(
    alternative_incapacitation,
    'Can life imprisonment without parole incapacitate dangerous offenders as completely as execution?',
    'Comparative study of escape rates, violence rates within prisons, and recidivism upon release (if any): does life imprisonment fail to incapacitate dangerous offenders in ways execution does not?',
    'If life imprisonment incapacitates equally, the retribution and deterrence readings become pure extraction without genuine coordination benefit; if life imprisonment fails (prisoners escape, life-sentenced prisoners kill in prison or upon release after commutation), the incapacitation reading retains empirical support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_incapacitation, empirical, 'Whether life imprisonment adequately substitutes for execution as incapacitation').

omega_variable(
    moral_disagreement_structure,
    'Is the disagreement between the abolition reading and the retributive reading a disagreement about moral facts (does proportionate retribution require execution?) or about moral values (do we value the right to life more than proportionate retribution)?',
    'Analytic philosophy and jurisprudential analysis of retributive theory: can a retributive theory be constructed that demands execution without appeal to the right to life? Or is every such theory self-refuting (it asserts moral equality but denies the equal right to life)?',
    'If disagreement is about moral facts, foreclosure is possible (one reading logically rules out the other); if disagreement is about moral values, coexistence is permanent (both readings remain live, their choice depends on value priorities).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_disagreement_structure, conceptual, 'Whether the disagreement between abolition and retributive readings is factual or value-based').

omega_variable(
    kernel_over_time,
    'Is the state_execution_authority kernel still a live site of contestation, or has global abolitionism essentially resolved it in favor of the abolition reading?',
    'Global survey of execution-retaining versus execution-abolishing jurisdictions over a 20-year window: is the proportion of execution-retaining jurisdictions rising or falling? Are new jurisdictions abolishing execution faster than they adopt it?',
    'If abolitionism is winning, the kernel may be moving toward foreclosure (the retributive and deterrence readings becoming historically marginal); if execution-retaining jurisdictions are stable or growing, the kernel remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_over_time, empirical, 'Whether the state_execution_authority kernel is still live or moving toward resolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, wrongful_conviction_irreversibility).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, prisoner_appellate_access).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel state_execution_authority. The abolition reading categorically denies the state's authority to execute; the retributive and deterrence readings affirm it under different justifications. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and type classifications. All three are linked via network.affects_constraints to show family kinship. The abolition reading (this file) treats execution as pure extraction (snare); the retributive reading treats it as coordinative punishment (tangled rope or rope); the deterrence reading treats it as protective coordination (tangled rope or rope). The ε-invariance principle requires separate stories: each reading has a different referent (the state execution arrangement as the abolition reading sees it versus as the retributive reading sees it) and yields different classification consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
