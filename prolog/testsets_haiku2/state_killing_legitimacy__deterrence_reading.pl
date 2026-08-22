% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Execution Justified as Deterrent Signal
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   The deterrence reading of state execution authority justifies capital
 *   punishment as a rational signal to potential murderers that homicide
 *   risks maximal state response (death), thereby preventing future murders
 *   through fear. Under this reading, the condemned offender is
 *   instrumentalized—their execution is the means by which the state
 *   communicates threat to the broader population of would-be murderers. The
 *   beneficiary is the unnamed class of potential future murder victims
 *   protected by the deterrent effect. This is one of three readings of the
 *   contested kernel 'state killing legitimacy'; the other readings are
 *   retributive (execution as proportional desert) and abolition (execution
 *   categorically violates human dignity). The deterrence reading is
 *   empirically vulnerable: if execution does not reliably deter, the
 *   constraint becomes pure extraction without the coordination
 *   justification. The extraction measured here (0.58) reflects contested
 *   empirical premises and rising theater ratio over time as the deterrence
 *   claim faces skepticism.
 *
 * KEY AGENTS:
 *   - state_enforcement_apparatus: institutional agenda-setter, administers legal framework that executes and justifies via deterrence
 *   - condemned_offender: powerless payer, instrumentalized as signal, trapped exit, immediate time horizon
 *   - potential_future_murder_victims: powerless, unnamed, diffuse beneficiary, counterfactual benefit (murders prevented)
 *   - would-be murderers (criminogenic): theoretical audience for deterrent signal, excluded from legal conversation, benefit through deterred action
 *   - empirical_criminology_researchers: analytical observer, central to verifying whether deterrence thesis is sound
 *   - abolition_advocates: excluded in retentionist jurisdictions, would argue dignity violation overrides deterrence calculus
 *   - retributive_justice_theorists: excluded from deterrence framing, offer alternative grounding (proportional desert rather than utility)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.58).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Execution Justified as Deterrent Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'c6c36ad8-5034-4ab1-b4fe-92c59048b420').
narrative_ontology:cs_kernel_codification('c6c36ad8-5034-4ab1-b4fe-92c59048b420', formalized).
narrative_ontology:cs_authority_grounding('c6c36ad8-5034-4ab1-b4fe-92c59048b420', extraction).
narrative_ontology:cs_interpretation_layer_present('c6c36ad8-5034-4ab1-b4fe-92c59048b420').
narrative_ontology:cs_reading_relation('c6c36ad8-5034-4ab1-b4fe-92c59048b420', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6c36ad8-5034-4ab1-b4fe-92c59048b420', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('c6c36ad8-5034-4ab1-b4fe-92c59048b420', foundational, execution_credibly_signals_murder_deterrent).
narrative_ontology:cs_axiom_status(execution_credibly_signals_murder_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('c6c36ad8-5034-4ab1-b4fe-92c59048b420', execution_credibly_signals_murder_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('c6c36ad8-5034-4ab1-b4fe-92c59048b420', foundational, state_may_instrument_offender_for_future_victim_protection).
narrative_ontology:cs_axiom_status(state_may_instrument_offender_for_future_victim_protection, holdable).
narrative_ontology:cs_axiom_grounding('c6c36ad8-5034-4ab1-b4fe-92c59048b420', state_may_instrument_offender_for_future_victim_protection, deontological).
narrative_ontology:cs_reference_frame('c6c36ad8-5034-4ab1-b4fe-92c59048b420', utilitarian_crime_prevention_rationality).
narrative_ontology:cs_drift_state('c6c36ad8-5034-4ab1-b4fe-92c59048b420', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c6c36ad8-5034-4ab1-b4fe-92c59048b420', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_murder_victims).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offender).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, would_be_murderers).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, victims_families_original).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legal framework that authorizes, sentences, and executes convicted murderers. Justifies execution as a rational deterrent signal to potential murderers. Administers the apparatus that carries out the sentence. Benefits from the constraint by centralizing state authority over the ultimate sanction and by framing state killing as rational crime prevention. Can change the law unilaterally; not bound by the constraint if political will shifts.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The convicted murderer executed under the deterrence framework. Their death is inflicted as a signal to potential murderers; they are instrumentalized as a means to communicate threat. No meaningful participation in the justification; legal appeal processes are exhausted. No exit except through executive clemency (rare and often politically contingent). The offender bears the full cost of the constraint without negotiating its terms.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offender, payer,
    powerless, immediate, trapped, national).

% The unnamed, counterfactual beneficiary class: any person who would be murdered but is protected (in deterrence theory) by the credible threat of state execution. They benefit from the deterrent signal without organizing, participating, or being named. Their benefit is passive and requires the empirical premise that execution deters murder. No seat negotiates on their behalf in real time.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_murder_victims, beneficiary,
    powerless, immediate, trapped, national).

% The theoretical audience for the deterrent signal: persons who would commit murder absent the threat of execution but refrain because they believe execution risk is real. They are beneficiaries of the constraint only by counterfactual avoidance (murders not committed). They are excluded from the legal conversation—the deterrence theory assumes they will be deterred by observing executions without participating in a justification dialogue. Their 'benefit' (life preservation through avoided crime) is assumed rather than negotiated.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, would_be_murderers, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, would_be_murderers, excluded).

% Families of the original murder victim and the condemned offender both occupy this seat structurally. Original victims' families may perceive execution as closure or justice, though they are not the primary beneficiaries under the deterrence reading (the theory benefits future victims, not the present ones). Offender families perceive additional loss. They are often offered procedural participation (victim impact statements, witness attendance) but occupy a secondary role in the deterrence framework.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, victims_families_original, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, victims_families_original, observer).

% Judges, prosecutors, and executing officers who carry out the legal process and execution. They administer the deterrence framework without necessarily endorsing the empirical premises (whether execution actually deters). Their role is enforcement of the constraint; the deterrence theory's truth is presumed by the legal system they inhabit. Many carry professional conflict between the deterrence justification and personal moral or empirical doubts. They are essential to the constraint's maintenance but are excluded from choosing whether to enforce it (structural role is agenda-setter, not independent evaluator).
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminal_justice_professionals, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, criminal_justice_professionals, observer).

% Study the empirical relationship between execution risk and homicide rates. Under the deterrence reading, they are the analytic seat that could verify or falsify the core justification. Their findings are used by both deterrence defenders and abolitionists. Their seat is structurally excluded from the enforcement decision (judges do not defer execution pending criminological consensus), but analytically central to whether the constraint's justification is sound. Mixed findings in the literature reflect genuine empirical uncertainty about deterrence magnitude.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminology_researchers, observer,
    analytical, biographical, analytical, global).

% Argue that state execution violates human dignity regardless of deterrent effect, and that framing murder prevention in deterrence terms obscures the real issue: whether the state has the right to kill. They are structurally excluded from the legal conversation in retentionist jurisdictions; their objections are treated as policy disagreement rather than constitutional constraint. In abolitionist jurisdictions, this seat is dominant. They represent an alternative reading of the kernel (abolition) that renders the deterrence reading illegitimate by rejecting the premise that deterrence justifies state killing.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolition_advocates, excluded,
    moderate, biographical, constrained, national).

% Argue that the state's authority to execute derives from proportional desert—the offender forfeits the right to life through the gravity of murder—not from deterrent utility to future victims. They occupy a different committer axis (deontological proportionality vs. utilitarian deterrence). Under retribution, the offender is a subject of judgment, not a means to an end. They are excluded from the deterrence-reading framework but remain available as an alternative reading within the same kernel. Retributive and deterrence readings offer different grounds for the same legal rule but rest on incommensurable first principles.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, retributive_justice_theorists, excluded,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, state_enforcement_apparatus).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a centralized state authority to signal credibly to potential murderers that homicide risks capital punishment, thereby preventing future murders through rational fear of death. The coordination problem solved is: how does a legal system establish a deterrent signal strong enough to modify behavior at the threshold of lethal violence? The solution is maximal-penalty execution—the state matches lethal violence with lethal response, communicating inevitability and severity.
% TRANSFER_FUNCTION: Transfers the condemned offender's life to the state enforcement apparatus as the means by which a credible deterrent signal is broadcast to the broader population of would-be murderers. The offender pays with death; the unnamed future victim class receives theoretical protection (murders prevented through fear). No actual victim negotiates or consents to the exchange; the future benefit is counterfactual (murders that do not occur).
% ABSENT_VOICES: Would-be murderers whose behavior the system assumes will be deterred are excluded from the legal conversation—they are the theoretical audience for the deterrent signal but have no seat in the justification or enforcement process. Abolition advocates are excluded in retentionist jurisdictions: they would argue that human dignity imposes a non-negotiable constraint on state killing that overrides any deterrence calculus. Criminologists who find weak or no deterrent effect are excluded from the enforcement decision: judges do not postpone execution pending empirical consensus on deterrence, so dissenting research findings do not feed back into the legal process. Retributive justice theorists occupy a different framing and are not present to argue that proportionality, not deterrence, should ground execution authority.
% DISAPPEARANCE_RATIONALE: If the state ceased executing murderers overnight and the deterrent signal vanished, the would-be murderer population would recalculate risk; homicide rates would shift (deterrence theory predicts they would rise; abolitionists predict they would not, or would fall if alternative public health measures were taken). The arrangement's persistence depends on the credibility of the execution threat. Removal would unmake a central institution of criminal-justice signaling and would force the state to rely on alternative deterrents (long prison sentences, certainty of apprehension, social stigma) or alternative crime-prevention theories (rehabilitation, incapacitation, retribution). The entire rational-deterrence framework would be disabled.
% FOUNDING_PROBLEM: Early modern criminal law lacked a credible response to lethal violence. Without severe and visible sanction, murder would proliferate unchecked. Execution emerged as the maximal signal: the state matches lethal violence with lethal response, communicating to potential murderers that homicide is met with death. The founding problem is: how does a legal system deter murder through rational fear and calculation of consequences?
% FOUNDING_PROBLEM_CORROBORATION: The deterrence thesis rests on disputed empirical claims about behavioral deterrence. Criminological research from outside the justice system shows mixed results: Ehrlich (1975) found deterrent effects (3-18 murders prevented per execution), but replication efforts (Baldus & Cole 1975, Yunker 1976, Zeisel 1976) contested methodology and found negligible effects. Meta-analyses and subsequent studies (Bailey 1998, Donohue & Wolfers 2005, Nagin 2013, Sharkey 2010) show no consensus: some find small effects, others find none; heterogeneity near zero across methods. The state enforcement system asserts the founding problem is live and deterrence response effective; major criminological organizations (American Society of Criminology, Criminology & Public Policy consensus) state the evidence for deterrence is inconclusive. Abolitionists argue the founding problem is misdiagnosed: execution does not reliably deter and does not prevent murder through rational fear; the real function is retribution or state dominance dressed as rational policy. No corroborating voice from outside the justice system affirms that execution reliably prevents future murders.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the empirical contest: the deterrence thesis rests on measurable deterrent effects, but criminology literature is mixed (some studies show small effects, meta-analyses show heterogeneity near zero, replication concerns are substantial). Extractiveness is not at snare-level because the constraint has a genuine coordination function (centralized state authority that prevents some murders), but it is elevated from pure rope because the offender bears the cost without meaningful participation in the justification and the future beneficiaries are unnamed and counterfactual. Suppression (0.72) is high because the constraint's persistence depends on preventing exit (abolition advocates cannot leave the jurisdiction; would-be offenders are presumed deterred by observation, not by choice to participate; criminological dissent is excluded from enforcement decisions). Theater_ratio rises from 0.25 to 0.44 over the interval, suggesting that as empirical doubts about deterrence accumulate, more enforcement energy goes into theatrical justification (ritual appeals to deterrence) rather than functional deterrence. The time series reflects a widening gap between the deterrence claim and observable compliance with it. Accessibility_collapse (0.62) is moderate: alternatives to capital punishment exist and are visible (life imprisonment, jurisdictions without execution), but the deterrence framing presents execution as uniquely effective, collapsing alternatives if the empirics hold. Resistance (0.79) is high: abolitionists, many criminologists, and significant populations in retentionist jurisdictions actively resist the legitimacy of execution, and this resistance has grown over the interval (rising theater_ratio indicates the state defending rather than demonstrating deterrence).
 *
 * PERSPECTIVAL GAP:
 *   The state enforcement apparatus (agenda-setter) perceives the constraint as justified coordination: it establishes a credible signal that prevents murders. Potential victims perceive abstract protection (deterred murders). The condemned offender perceives instrumentalization: their death is inflicted to communicate to others, not for their own restoration or proportional judgment. Abolition advocates perceive human rights violation. Criminologists perceive empirical uncertainty masquerading as settled fact. The engine will compute these seats as arriving at different type classifications from the same structural data: the agenda-setter may perceive rope (genuine coordination), the payer may perceive snare (pure extraction via state power), and the observer (researcher) may perceive contested constraint whose type depends on unresolved empirical questions. This perspectival divergence is the measurable gap the constraint story captures.
 *
 * DIRECTIONALITY LOGIC:
 *   The state enforcement apparatus has institutional power, arbitrage-level exit (can change the law unilaterally), and benefits from the constraint (centralizes authority, collects legitimacy from deterrence theory, maintains capital punishment apparatus). Directionality near 0.0 (beneficiary). The condemned offender has powerless status, trapped exit (no legal recourse after conviction), and bears the cost (death). Directionality near 1.0 (full target). Potential future victims have powerless status, trapped exit (cannot opt out of murder risk), and receive counterfactual benefit (protection from murders that don't occur). Directionality near 0.5 with downward bias (beneficiary in theory, but unnamed and counterfactual). Would-be murderers are assumed deterred by observing executions; their exit is constrained by the belief that execution is inevitable, which makes them structurally targeted (high d) even though the enforcement contact is avoided if deterrence works. The measurement grid shows increasing suppression_requirement from 0.65 to 0.76, indicating the constraint requires more active coercive effort to maintain its coherence as deterrence empirics face skepticism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope because it combines a genuine coordination function (centralized state authority to signal deterrent threat) with asymmetric extraction (the condemned offender bears the full cost; future beneficiaries are unnamed and diffuse). This prevents mislabeling as pure snare (which would ignore the real coordination benefit to the system) and as pure rope (which would ignore the instrumentalization and absence of offender consent). The tangled_rope classification holds as long as the deterrent effect is presumed; if empirical evidence settles that deterrence is negligible, the constraint loses its coordination justification and becomes pure snare. The rising theater_ratio (0.25 to 0.44) is a leading indicator of mandatrophy drift: as the empirical deterrence claim faces skepticism, the constraint shifts from demonstrating deterrence (functional) to asserting deterrence (theatrical), which is the classic piton signature. The constraint may be approaching mandatrophy resolution: if deterrence is empirically false, the founding problem (how to prevent murder through rational fear) is moot, and the constraint persists as ritual (maintaining execution law despite failed justification). This is noted in the founding_problem_status field as 'contested' and will be tracked through T17 (abductive trigger for extraction accumulation without functional foundation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(stat_tr_t0, projected).
narrative_ontology:measurement(stat_tr_t5, state_killing_legitimacy__deterrence_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, state_killing_legitimacy__deterrence_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t25, state_killing_legitimacy__deterrence_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(stat_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(stat_be_t0, projected).
narrative_ontology:measurement(stat_be_t5, state_killing_legitimacy__deterrence_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, state_killing_legitimacy__deterrence_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t25, state_killing_legitimacy__deterrence_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(stat_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(stat_su_t0, projected).
narrative_ontology:measurement(stat_su_t5, state_killing_legitimacy__deterrence_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t15, state_killing_legitimacy__deterrence_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t25, state_killing_legitimacy__deterrence_reading, suppression_requirement, 25, 0.73).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(stat_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% The constraint 'state_killing_legitimacy' decomposes into three structurally distinct claims under the ε-invariance principle. The deterrence reading (this story) justifies execution as a rational signal to would-be murderers; the retributive reading justifies execution as proportional desert; the abolition reading forbids execution as violating human dignity. Each reading instantiates a different constraint with different ε values, different stakeholder structures, different empirical vulnerabilities, and different coexistence relationships. All three share the same formal kernel (state authority to execute) but derive incompatible interpretations. The three constraints are linked via network.affects_constraints to capture the kernel contest structure: if one reading's empirics collapse or normative ground is rejected, the pressure propagates to the siblings (e.g., if deterrence fails empirically, retribution becomes the primary available justification; if retribution is abandoned, abolition gains force).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
