% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint embodies the retributive reading of capital punishment:
 *   execution restores moral balance by imposing punishment proportionate to
 *   heinous crime. It is one of three contested readings of the kernel
 *   'state_execution_authority'. The retributive reading grounds execution
 *   authority in proportionality doctrine and moral restoration, distinct
 *   from deterrence-based justifications (another reading) and the
 *   deontological prohibition (abolition reading). The constraint is claimed
 *   as tangled_rope: it coordinates a unified, appellate-checked punishment
 *   system (coordination function) while extracting the executed offender's
 *   life and suppressing alternatives through appellate deference and
 *   procedural finality doctrines (extraction and suppression functions).
 *
 * KEY AGENTS:
 *   - state_sovereign_authority — Institutional agenda-setter that administers execution and claims moral sovereignty.
 *   - victims_families — Moderate-power beneficiaries who receive formal recognition and closure within the retributive frame.
 *   - executed_offender — Powerless payer whose life is the extracted cost, deemed legitimate by proportionality logic.
 *   - death_row_population_risk — Powerless identity-locked payers carrying the risk of wrongful execution; the framework acknowledges the risk but treats accurate guilt determination as the trial/appellate system's responsibility.
 *   - abolition_advocates — Excluded by the retributive frame's foundational premise; their deontological claim (execution categorically forbidden) does not engage proportionality logic.
 *   - innocence_plaintiffs — Structurally absent except as rare procedural exceptions; appellate finality doctrines foreclose most post-conviction innocence claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.68).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '6683ccd4-6f8e-45ac-91b5-285807c5752a').
narrative_ontology:cs_kernel_codification('6683ccd4-6f8e-45ac-91b5-285807c5752a', formalized).
narrative_ontology:cs_authority_grounding('6683ccd4-6f8e-45ac-91b5-285807c5752a', extraction).
narrative_ontology:cs_interpretation_layer_present('6683ccd4-6f8e-45ac-91b5-285807c5752a').
narrative_ontology:cs_reading_relation('6683ccd4-6f8e-45ac-91b5-285807c5752a', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6683ccd4-6f8e-45ac-91b5-285807c5752a', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('6683ccd4-6f8e-45ac-91b5-285807c5752a', foundational, execution_proportionate_to_heinous_crime).
narrative_ontology:cs_axiom_status(execution_proportionate_to_heinous_crime, holdable).
narrative_ontology:cs_axiom_grounding('6683ccd4-6f8e-45ac-91b5-285807c5752a', execution_proportionate_to_heinous_crime, deontological).
narrative_ontology:cs_axiom('6683ccd4-6f8e-45ac-91b5-285807c5752a', foundational, state_sovereign_duty_moral_restoration).
narrative_ontology:cs_axiom_status(state_sovereign_duty_moral_restoration, holdable).
narrative_ontology:cs_axiom_grounding('6683ccd4-6f8e-45ac-91b5-285807c5752a', state_sovereign_duty_moral_restoration, deontological).
narrative_ontology:cs_reference_frame('6683ccd4-6f8e-45ac-91b5-285807c5752a', proportional_punishment_doctrine).
narrative_ontology:cs_drift_state('6683ccd4-6f8e-45ac-91b5-285807c5752a', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6683ccd4-6f8e-45ac-91b5-285807c5752a', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_sovereign_authority).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offender).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, death_row_population_risk).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportional_justice_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, retributive_moral_philosophy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state criminal justice apparatus that sentences, appeals, houses, and executes capital offenders. It claims execution authority derives from the sovereign's legitimate monopoly on proportionate punishment, grounded in social contract theory. The state administers the constraint, chooses which offenses merit capital punishment, sets appellate standards, and carries out the sentence. From the state's perspective, execution is a necessary expression of moral sovereignty — the capacity to impose ultimate penalty proportionate to ultimate crime.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_sovereign_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Families of murdered victims. The retributive reading assigns them as beneficiaries: execution of the offender is presented as restoring moral balance, providing closure, and vindicating the gravity of the crime against their loved one. Their access to witnessing, victim impact statements, and clemency opposition creates a defined role in the execution process. Their exit options are constrained by law — they cannot demand punishment, cannot commute sentences, but gain formal recognition in sentencing and execution proceedings.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, national).

% The individual convicted and sentenced to death. In the retributive frame, the offender is the legitimate cost-bearer: the imposed penalty is proportionate to the crime. The offender is stripped of civil rights, confined to death row, and executed. Their exit options are only appeals (constrained by appellate standards that presume conviction validity) and clemency petitions (which depend on executive discretion). The retributive reading treats execution as a justified imposition, not a harm to be minimized.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offender, payer,
    powerless, immediate, trapped, national).

% Individuals convicted and sentenced to death who may be innocent, may have had inadequate legal representation, or may face execution despite procedural errors. The retributive reading acknowledges wrongful execution as a tragic error but does not invalidate the framework — the framework assumes accurate guilt determination. This population carries structural risk: they are trapped by the appellate system's deference to trial verdicts, and their identity as 'convicted capital offender' forecloses most exit options even when evidence of innocence emerges post-conviction.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, death_row_population_risk, payer,
    powerless, biographical, identity_locked, national).

% Organizations, legal scholars, religious bodies, and international human rights frameworks that argue execution is categorically impermissible regardless of crime or procedure. They are structurally excluded from the retributive framework's legitimacy conversation — their core claim (that proportionate punishment cannot justify execution) is the competing reading, not a voice within the retributive reading itself. Their exclusion is structural: the retributive reading does not engage the deontological premise that some punishments are categorically forbidden.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Empirical researchers and policymakers who defend execution on deterrence grounds: that the threat of capital punishment raises the cost of murder and prevents future killings. They are excluded from the retributive reading in the sense that their justification is different — they argue execution prevents harm, not that it restores balance. If deterrence is empirically false, the deterrence reading fails; the retributive reading remains, grounded in proportionality rather than prevention.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, deterrence_advocates, excluded,
    organized, generational, constrained, national).

% Individuals on death row with credible evidence of innocence whose appeals have been exhausted or rejected under appellate standards that presume trial verdict validity. They would object vehemently to inclusion in the 'legitimate cost' category but are procedurally excluded from reopening their cases in many jurisdictions. The retributive framework does not deny their innocence claim — it simply relegates guilt determination to the trial and appellate process, treating post-conviction innocence claims as outside its scope unless they meet narrow procedural gates.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, innocence_plaintiffs, excluded,
    powerless, immediate, trapped, national).

% Constitutional courts and legislatures that adjudicate whether execution violates fundamental rights (e.g., 8th Amendment Cruel and Unusual Punishment, European Convention on Human Rights Article 3). They observe and rule on whether the constraint's implementation meets procedural standards, whether alternatives (life imprisonment without parole) satisfy justice, and whether evolving standards of decency restrict or eliminate execution. They are analytical seats with the authority to reshape the constraint through constitutional interpretation.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, constitutional_authority, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, state-administered system for imposing ultimate punishment proportionate to ultimate crime. Replaces vigilante justice, private blood revenge, and inconsistent ad-hoc punishment with a formal, appellate-checked, publicly witnessed procedure grounded in rational proportionality.
% TRANSFER_FUNCTION: Transfers the executed offender's life — the ultimate cost — from the executed person to the state, justified as restoration of moral balance to victims' families and vindication of the murdered person's worth. The state claims to collect moral authority (sovereignty to punish), victims' families claim closure and recognition.
% ABSENT_VOICES: Innocence plaintiffs (convicted innocents whose guilt determination is presumed final) are absent from the conversation except as rare procedural exceptions; abolition advocates are excluded because their deontological premise (execution is categorically forbidden) does not engage the retributive frame; deterrence advocates occupy a different justification (prevention, not proportion) and would dispute whether execution actually deters, creating a parallel reading not integrated here.
% DISAPPEARANCE_RATIONALE: If execution authority vanished, the criminal justice system would reorganize around alternative ultimate punishments (life without parole, indefinite detention). Victims' families would lose the formal recognition and closure execution provides within the retributive frame. The state would lose sovereignty to impose proportionate punishment for heinous crimes. Moral balance restoration — as the retributive reading defines it — would no longer be available as a punishment option.
% FOUNDING_PROBLEM: How should the state proportionately punish those who commit heinous murders — crimes so grave they strike at the moral order itself? How can justice restore balance when an innocent life has been taken? The retributive reading answers: through proportionate punishment imposed by sovereign authority, including execution for the gravest crimes.
% FOUNDING_PROBLEM_CORROBORATION: The state criminal justice apparatus and retributive moral philosophy scholars (e.g., Michael Moore, Jeffrie Murphy) attest the founding problem is live and execution is the proportionate answer. Abolition advocates (American Civil Liberties Union, Amnesty International), international human rights bodies (European Court of Human Rights, UN Human Rights Committee), and death penalty abolition scholars dispute whether execution is necessary or permissible; they argue life imprisonment answers the proportionality question without execution. The constitutive contest is not whether heinous murder is grave, but whether proportionate punishment includes death as a permissible option. No external non-benefiting corroboration exists for the founding problem; the state and retributive philosophers are themselves the institutional beneficiaries of this reading.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint extracts the offender's life, and this extraction is justified solely by the proportionality requirement — it cannot be substituted with imprisonment without violating the retributive reading's core claim (that proportionate punishment for heinous crimes requires execution). The constraint is not a mere coordination problem solvable by alternatives; it is extractive BY DESIGN within this reading. Suppression is high (0.72) because appellate standards that presume trial verdict validity, procedural finality doctrines, and the near-impossibility of reopening cases after conviction suppress the primary exit option: post-conviction exoneration. Theater ratio is moderate (0.48) and rising slightly: formal ritual (appeals, witness protocol, final statements) comprises a growing share of the process as executions become rarer, but the core function (imposing proportionate punishment) is not theatrical. Resistance is high (0.79) because the constraint meets sustained organized opposition from abolition advocates, innocence networks, and international human rights regimes — this is not a constraint quietly accepted by all stakeholders. Accessibility_collapse is high (0.71) because once convicted and sentenced, alternatives (innocence claim, escape, clemency) become near-impossible for the executed offender; the retributive frame presumes finality of guilt determination. The measurement series show extractiveness and suppression rising early (0-30) then plateauing (30-60), consistent with a constraint that became more defined and resistant-to-challenge as the abolition movement grew, but whose core extraction cannot increase further without changing the reading itself.
 *
 * PERSPECTIVAL GAP:
 *   The retributive reading's core claim is that execution is a legitimate, proportionate punishment for heinous crime. From the state's institutional perspective, it is a valid expression of sovereign authority and proportionality doctrine. From the executed offender's perspective (and especially for wrongfully convicted individuals), it is unjustified killing by the state. From victims' families' perspective, it provides closure and moral restoration, but it does not restore the murdered loved one. From the abolition perspective, the entire framing is illegitimate — no proportionality justifies execution. The engine computes these different seatings and their resulting type classifications; the constraint story's claim (tangled_rope) and metrics (high extraction, high suppression) do not adjudicate these divergences — they describe the structural tension itself.
 *
 * DIRECTIONALITY LOGIC:
 *   State_sovereign_authority: d ≈ 0.1 (beneficiary, administrative position, analytical exit). Victims_families: d ≈ 0.35 (beneficiary in retributive frame, but constrained by law, biographical horizon, moderate power — they receive recognition but cannot execute, commute, or appeal independently). Executed_offender: d ≈ 0.95 (full target, powerless, immediate time horizon, trapped exit, the extraction object itself). Death_row_population_risk: d ≈ 0.92 (full target on the extraction axis, identity-locked exit amplifies the structural extraction — the system locks them into 'convicted' identity despite innocence evidence). Abolition_advocates and innocence_plaintiffs are excluded roles; they do not receive directionality values from the stakeholder surface but feature in the omega variables and commentary.
 *
 * MANDATROPHY ANALYSIS:
 *   The retributive reading's founding problem (how to proportionately punish heinous crime) is contested for status: the state says the problem is live and execution is necessary; abolition advocates and many international bodies say the problem is solved by life imprisonment without parole and execution is no longer justified. Theater ratio rising slightly but staying moderate (0.48) indicates the constraint has not yet become purely theatrical — executions are rare, but when they occur, the proportionality function is still operative. However, the rising theater ratio combined with the founding_problem_status='contested' suggests a future pathway: if execution becomes so infrequent and so procedurally ritualized that the proportionality function no longer operates at scale, the constraint could degrade toward piton (maintained by institutional inertia rather than live function). No mandatrophy is presently resolved, but the structural trajectory is visible in the measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calibration,
    'Does a specific proportionality standard exist and is it consistently applied, or is proportionality interpreted ad-hoc by sentencing judges?',
    'Systematic analysis of sentencing patterns across jurisdictions: does the severity of crime-to-sentence mapping show consistency (suggesting principled proportionality) or variance (suggesting ad-hoc judgment)?',
    'If applied ad-hoc, the constraint''s claim to be grounded in proportionality doctrine is weakened, and the extraction appears more discretionary; if consistent, the proportionality framing is more defensible. High variance would support reclassification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_calibration, empirical, 'Whether proportionality operates as a principle or as cover for discretionary punishment.').

omega_variable(
    wrongful_execution_frequency,
    'How many executed individuals were innocently convicted? What is the false conviction rate at capital level?',
    'DNA exonerations, Innocence Project case compilations, and statistical studies of wrongful conviction rates in capital cases.',
    'If wrongful execution is rare (<1%), the retributive frame''s assumption of accurate guilt determination is more tenable. If substantial (>5%), the extracted cost includes innocence, which the retributive frame cannot absorb without violating its own proportionality logic — this would produce a major omega toward reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_frequency, empirical, 'Whether the constraint''s assumption of accurate guilt determination holds empirically.').

omega_variable(
    closure_legitimacy,
    'Do victims'' families actually experience closure and moral restoration from execution, or is execution a state function that victims'' families participate in but do not control?',
    'Qualitative research with murder victims'' families in execution jurisdictions and abolition jurisdictions; surveys of emotional outcomes before and after execution.',
    'If victims'' families experience genuine closure, the beneficiary classification is supported. If they experience ritual participation without actual restoration, the beneficiary role becomes ambiguous and the constraint appears more extractive for the state than restorative for families.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(closure_legitimacy, empirical, 'Whether victims'' families are genuine beneficiaries or ritual participants in a state extraction.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the retributive reading logically foreclose the abolition reading, or are they merely competing interpretations of the same kernel?',
    'Philosophical analysis: Does the claim ''proportionate punishment can include execution'' logically rule out ''execution is categorically impermissible''? Or do both positions remain live depending on one''s foundational premises?',
    'If forecloses, then in any single framework only one reading can hold; if coexist, both remain live across different parties. This determines the reading_relations field in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'The logical relationship between the retributive and abolition readings of state_execution_authority.').

omega_variable(
    appellate_finality_justification,
    'Is appellate deference to trial verdicts (the ''presume guilt'' standard that suppresses post-conviction innocence claims) justified by proportionality doctrine, or is it a separate institutional constraint that enables retributive extraction?',
    'Examine the history of appellate standards: were they adopted to implement proportionality, or did they arise for other reasons (efficiency, finality, institutional stability)?',
    'If appell ate finality is integral to proportionality (cannot execute proportionately without finality), the suppression is part of the coordination function. If finality is institutional baggage, the suppression is extractive overhead not justified by proportionality alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_finality_justification, empirical, 'Whether appellate finality doctrines are required by proportionality or are extractive institutional constraints.').

omega_variable(
    moral_restoration_vs_deterrence,
    'Is the retributive reading''s claim (that execution restores moral balance) empirically distinct from the deterrence reading''s claim (that execution prevents future murder)? Can both be true, or do they compete?',
    'If deterrence studies show execution does NOT prevent murder, can the retributive reading stand on proportionality alone, independent of prevention? If yes, the readings coexist (different justifications); if no, deterrence failure undermines retribution.',
    'This tests whether the sibling readings influence or foreclose each other. If deterrence is empirically false and retribution is undamaged, coexists_with is correct; if deterrence failure is thought to undermine moral restoration, influences or forecloses may apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_restoration_vs_deterrence, empirical, 'Whether retributive and deterrence justifications are logically separable or mutually dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t45, state_execution_authority__retributive_reading, theater_ratio, 45, 0.48).
narrative_ontology:measurement_basis(stat_tr_t45, observed).
narrative_ontology:measurement(stat_tr_t60, state_execution_authority__retributive_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement_basis(stat_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t45, state_execution_authority__retributive_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement_basis(stat_be_t45, observed).
narrative_ontology:measurement(stat_be_t60, state_execution_authority__retributive_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(stat_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t45, state_execution_authority__retributive_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement_basis(stat_su_t45, observed).
narrative_ontology:measurement(stat_su_t60, state_execution_authority__retributive_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(stat_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__retributive_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel state_execution_authority. The sibling readings (deterrence_reading and abolition_reading) decompose the single institutional practice 'capital punishment' into structurally distinct claims: retributive reading grounds authority in proportionality and moral restoration (ε ≈ 0.68, high because extraction cannot be substituted); deterrence reading grounds authority in crime prevention (different ε because substitution is possible if deterrence is effective); abolition reading denies authority altogether (different ε because the entire practice is contested as illegitimate). Each reading has its own beneficiary set, extraction logic, and type classification. The three readings form a constraint family linked by network edges. The retributive reading influences the other two: if proportionality fails (wrongful executions, ad-hoc sentencing), the retributive frame collapses and deterrence becomes the only live justification, which then becomes the target of abolition critique. If proportionality succeeds but deterrence is false, retribution stands alone and abolition gains ground. If both fail, execution authority loses all justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__retributive_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
