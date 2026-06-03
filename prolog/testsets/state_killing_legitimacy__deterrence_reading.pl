% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Execution Justified as Deterrent Signal
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   The deterrence reading of state killing legitimacy justifies execution as
 *   a rational instrument to prevent future murders by signaling to potential
 *   offenders that capital crimes will result in death. This constraint
 *   instantiates one pole of a contested kernel about the legitimacy of
 *   state-imposed capital punishment. The kernel—whether and under what
 *   conditions the state may kill—admits of three major readings: the
 *   deterrence reading (execution justified by prospective crime prevention),
 *   the retributive reading (execution justified by proportional desert and
 *   offender moral status), and the abolition reading (execution
 *   categorically impermissible regardless of consequences or desert). This
 *   story generates ONLY the deterrence reading as a structurally independent
 *   constraint with its own empirical assumptions, beneficiary/victim
 *   structure, and legitimacy claims. The deterrence reading instruments the
 *   condemned offender as a means to a social end (protecting potential
 *   future victims) and justifies extraction (death) through rational
 *   calculation. The extractiveness value (0.48) reflects moderate empirical
 *   contestation: meta-analyses show weak or absent deterrent effects in most
 *   jurisdictions, yet deterrence rhetoric persists in legislative and
 *   judicial discourse, suggesting theater_ratio rising over time (0.42→0.55)
 *   as the empirical basis erodes and institutional inertia maintains the
 *   frame.
 *
 * KEY AGENTS:
 *   - Condemned Offender: Primary victim (powerless/trapped) — instrumentalized as means to deterrence signal; bears absolute extraction (death); no exit or appeal available once deterrence logic activated
 *   - Potential Future Victims: Primary beneficiary (moderate/constrained) — benefit from hypothetical deterrence effect; cannot verify counterfactual (would they have been murdered without deterrent threat?); generational horizon means benefit is abstract and distributed
 *   - State Executing Authority: Secondary beneficiary (institutional/arbitrage) — maintains monopoly on legitimate violence; consolidates state power through execution authority; benefits from deterrence frame regardless of empirical efficacy
 *   - Offender's Family: Secondary victim (powerless/trapped) — collateral bearers of extraction through grief, stigma, loss of economic support; no exit from status of family of executed person
 *   - Legal Tradition (Courts, Legislatures): Institutional actor (institutional/arbitrage) — maintains deterrence rhetoric in sentencing justifications despite empirical contestation; theater_ratio high and rising as evidence erodes
 *   - Criminological/Penological Community: Analytical observer (analytical/analytical) — produces meta-analyses showing weak deterrent effects; institutional incentives not aligned with accepting findings (legislatures ignore; media selectively cites supportive older studies)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.48).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.62).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Execution Justified as Deterrent Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '4fa33e34-8011-4e42-b63a-bd38fc32d2a3').
narrative_ontology:cs_kernel_codification('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', formalized).
narrative_ontology:cs_authority_grounding('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', lineage).
narrative_ontology:cs_interpretation_layer_present('4fa33e34-8011-4e42-b63a-bd38fc32d2a3').
narrative_ontology:cs_reading_relation('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', foundational, offender_rational_deterrent_response).
narrative_ontology:cs_axiom_status(offender_rational_deterrent_response, holdable).
narrative_ontology:cs_axiom_grounding('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', offender_rational_deterrent_response, empirically_contingent).
narrative_ontology:cs_axiom('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', foundational, execution_credible_marginal_deterrent).
narrative_ontology:cs_axiom_status(execution_credible_marginal_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', execution_credible_marginal_deterrent, empirically_contingent).
narrative_ontology:cs_reference_frame('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', rational_deterrence_framework).
narrative_ontology:cs_drift_state('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', contemporary_meta_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4fa33e34-8011-4e42-b63a-bd38fc32d2a3', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_legitimacy_via_deterrence).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, executed_offender).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offender_family).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONDEMNED OFFENDER (SNARE) — Faces irreversible extraction (death) justified as instrumental means to deter future actors. No exit, no negotiation, no appeal to desert or due process can alter the execution decision once the deterrence logic is activated. Maximum suppression and coercive overhead with minimal coordination function from the offender's perspective. The offender is pure instrumentality.
constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE OFFENDER'S FAMILY (SNARE) — Bears collateral extraction through grief, social stigma, and loss of economic support. No exit from the status of 'family of executed murderer.' Suppression operates through social shame and institutional indifference to family welfare. Trapped and powerless in their relationship to state killing legitimacy.
constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POTENTIAL FUTURE VICTIMS (ROPE) — Benefit from deterrence logic if execution actually prevents murders. Constrained by inability to know whether deterrence works or whether the condemned would have killed anyway. Generational time horizon: deterrence benefit applies to future cohorts who will not be murdered because of fear of execution. No individual victim can verify the counterfactual. Pure coordination function: the state executes to protect the vulnerable through rational signal, not to extract from them.
constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE STATE EXECUTING AUTHORITY (TANGLED ROPE) — Combines coordination function (deterring murder through credible threat) with institutional extraction (consolidating state monopoly on violence, demonstrating power, controlling the discourse on legitimate killing). The state benefits from deterrence logic regardless of whether deterrence actually works—the performative demonstration of capacity to kill authorizes the state's claim to legitimate violence. Arbitrage exit: state can choose to execute or not execute; maintains leverage. Active enforcement required to maintain the deterrence signal's credibility.
constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE LEGAL/PHILOSOPHICAL TRADITION (PITON) — Deterrence theory persists as a legitimating frame for capital punishment despite empirical contestation. Meta-analyses show no robust deterrent effect, yet deterrence rhetoric maintains institutional investment in execution as rational policy. Theater ratio high: the deterrence justification is maintained through institutional inertia and selective citation of supportive studies. The tradition has become decoupled from its empirical base but persists because the authority structure (legislatures, judiciaries) depends on deterrence framing to authorize killing. Piton classification reflects the degradation of epistemic function while the institution persists.
constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / INEVITABLE TRADEOFF VIEW (MOUNTAIN) — From a civilizational/universal perspective, the deterrence reading appears to encode an immutable tradeoff between individual sacrifice and collective welfare: some members of society must be instrumentalized as means to social ends, and this is an irreducible feature of rational governance under conditions of imperfect information. The logic appears natural and unchangeable—a law of rational choice. However, this perspective naturalizes what is actually a contingent decision about what counts as a legitimate use of state power. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_killing_legitimacy__deterrence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, TR),
    TR >= 0.70.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate, reflecting empirical contestation. The deterrence reading justifies execution as coordination (protecting future victims) with instrumental extraction (using condemned as signal). If deterrence empirically works, the coordination function is real and ε ≈ 0.45 is defensible. If deterrence fails (as meta-analyses suggest), the coordination function evaporates and ε should rise toward snare territory (0.65+). At 0.48, the score reflects the actual institutional state: deterrence is invoked in ~95% of death-penalty jurisdictions' judicial discourse, yet criminological consensus shows negligible deterrent effect. This gap between rhetoric and evidence places extractiveness in the high-contention zone. Suppression (0.62): High. The condemned has no exit (trapped), no alternative, no appeal available once death sentence is rendered. The deterrence logic operates precisely by removing alternatives—the offender must die to send the signal, so no negotiation or substitute punishment is permitted. Suppression is structural and irreducible from the condemned's perspective. Theater ratio (0.55): Moderate-high and rising. Courts invoke deterrence arguments in sentencing but increasingly rely on citations to studies that are outdated or methodologically criticized. The performance of rational deterrence justification persists despite the empirical base eroding. Rising trajectory (0.42→0.55) reflects growing disconnect between discourse (deterrence works) and evidence (negligible effect). Claimed type: Tangled Rope. Genuine coordination function (protecting future victims) exists as a stated rationale, alongside asymmetric extraction (condemned is killed, beneficiaries are protected via signal). Active enforcement required (executions must actually occur to maintain the signal's credibility). Beneficiaries and victims are distinct groups. However, empirical contestation means the coordination may be partially or wholly illusory, placing the constraint at the rope/snare boundary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence on a single structural phenomenon. The condemned sees pure extraction (Snare)—they are being killed for instrumental reasons unrelated to their desert or agency. The potential future victims see pure coordination (Rope)—the state is protecting them through rational deterrent threat. The state sees mixed coordination and extraction (Tangled Rope)—legitimizing killing power while protecting citizens. The legal tradition sees its own degraded ritual (Piton)—deterrence is invoked but empirically unsupported; the performance persists through institutional inertia. The analytical observer risks seeing an immutable rationality constraint (Mountain)—as if the tradeoff between individual sacrifice and collective safety is an irreducible feature of governance. The perspectival gap reveals how the same structural arrangement—state killing justified by deterrent effect—is experienced as snare by the victim, rope by the beneficiary, and tangled rope by the authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) represents each agent's structural relationship to the deterrence constraint. The condemned offender has d ≈ 0.95 (full target of extraction); the state has d ≈ 0.10 (beneficiary with arbitrage options); potential future victims have d ≈ 0.50 (symmetric: they benefit from deterrence but cannot guarantee it works). The f(d) sigmoid applies these values to produce experienced extractiveness. The condemned with d=0.95 experiences f(d) ≈ 1.42, amplifying the base extractiveness of 0.48 to effective χ ≈ 0.68 (snare territory). The state with d=0.10 experiences f(d) ≈ -0.01, reducing extraction to near-zero or negative (institutional beneficiary). Future victims with d=0.50 experience f(d) ≈ 0.65, producing χ ≈ 0.31 (rope territory—coordination with slight asymmetry). No directionality overrides needed; the automatic derivation from beneficiary/victim declarations and trapped/arbitrage/constrained exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading demonstrates the mandatrophy at its core: the same structural arrangement (execution) can be justified as coordination (protecting future victims via rational signal), as extraction (instrumentalizing condemned as means to social end), or as retribution (proportional desert)—or as a combination thereof. The mandatrophy is resolved by recognizing that deterrence is a READING of the kernel, not the kernel itself. The kernel (state killing legitimacy) admits of multiple legitimate-seeming framings. The deterrence reading is live insofar as: (a) rational choice theory genuinely predicts that potential offenders respond to marginal penalty changes, and (b) empirical evidence confirms deterrent effects. If (a) fails (offenders are not rational decision-makers), the reading collapses. If (b) fails (no deterrent effects exist), the reading loses its justificatory force and reclassifies as pure extraction (snare). The reading's holding status depends on resolving the omegas. Until those are resolved, all three readings (deterrence, retributive, abolition) remain live—the constraint exemplifies the oracle gap (Theorem 4): different measurement frameworks (consequentialist vs. deontological vs. abolitionist) produce incommensurable classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_efficacy,
    'Does capital punishment actually deter murder at rates significantly above zero? Do executed offenders have measurably lower recidivism (counterfactually) than imprisoned offenders?',
    'Meta-analysis of comparative criminology studies; longitudinal murder-rate data across jurisdictions with and without capital punishment, controlling for socioeconomic factors; studies of marginal deterrence (incremental effect of execution vs. long imprisonment)',
    'If no measurable deterrent effect: deterrence reading collapses into pure extraction (snare reclassifies from all perspectives except state); coordination function evaporates; ε rises to 0.72+. If measurable effect exists: deterrence reading stabilizes; coordination function is real; ε remains moderate (0.45–0.55).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_efficacy, empirical, 'Empirical efficacy of capital punishment as crime deterrent').

omega_variable(
    alternative_deterrence_mechanisms,
    'Could equivalent or superior deterrence be achieved through long-term imprisonment (life without parole) without execution? What is the marginal deterrent effect of death penalty over permanent incapacitation?',
    'Comparative analysis of murder rates in death-penalty vs. life-imprisonment jurisdictions; studies of offender decision-making under different sentence structures; economic modeling of rational actor response to different penalty regimes',
    'If equivalent deterrence available via imprisonment: execution becomes unnecessary means to coordination function; reclassifies as unjustified extraction (snare); undermines the deterrence reading''s legitimacy claim. If execution has marginal deterrent advantage: coordination function justified; deterrence reading stabilizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_deterrence_mechanisms, empirical, 'Whether capital punishment provides deterrence marginal to life imprisonment').

omega_variable(
    rationality_of_condemned_decision_maker,
    'Do offenders who commit murder-qualifying crimes (capital murder) make rational expected-utility calculations that would be affected by knowledge of execution risk? Or are capital crimes predominantly committed under passion, intoxication, mental illness, or cognitive incapacity that prevents rational response to deterrent signals?',
    'Psychological autopsies of capital murders; analysis of offender decision-making processes in solved murders; empirical studies of whether rational actors actually respond to marginal changes in penalty severity',
    'If offenders are predominantly non-rational at time of crime: deterrence logic fails at its core assumption; the condemned is being killed not to prevent future murders by rational actors but to satisfy retributive or institutional needs. Reclassifies from ''coordination with extraction'' to pure extraction (snare). If rational decision-making predominates: deterrence logic valid; tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_of_condemned_decision_maker, empirical, 'Rationality assumptions about offenders subject to capital punishment').

omega_variable(
    kernel_reading_contest,
    'Is the deterrence reading a live normative commitment that remains holdable in contemporary governance frameworks, or has it been superseded by evidence and institutional change?',
    'Tracking of legislative and judicial reliance on deterrence arguments; shifts in expert consensus in criminology and penology; international law developments (EU abolition of capital punishment, UN moratorium advocacy); persistence or decline of deterrence citations in appellate opinions',
    'If deterrence reading is overridden in its own tradition (legal systems abandoning deterrence rationale while maintaining execution, or abandoning execution altogether): reading_status shifts to ''overridden'' in cs_structure.axioms. If deterrence remains live across major legal traditions: status remains ''holdable'' and deterrence reading persists as a structural pole in the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Holding status of deterrence reading within evolving legal traditions').

omega_variable(
    alternative_framing_as_retributive,
    'When courts invoke deterrence language, are they actually applying deterrence logic or using deterrence as rhetorical cover for retributive judgment (just deserts)?',
    'Discourse analysis of judicial opinions invoking deterrence; comparison of deterrence-language opinions with retributive-language opinions on sentencing patterns; interview studies of judges on reasoning processes',
    'If deterrence language is rhetorical cover for retribution: the deterrence reading is functionally piton-like (theater_ratio high); the actual constraint operating is retributive (different reading, different constraint file). Reclassifies the deterrence reading itself from tangled_rope to piton. If deterrence logic is genuinely applied: deterrence reading has its own structural integrity independent of retributive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framing_as_retributive, empirical, 'Whether deterrence invocations mask retributive reasoning').

omega_variable(
    innocent_execution_risk,
    'What is the empirical risk that execution-eligible offenders are later exonerated through DNA evidence, witness recantation, or prosecutorial error? Does this risk change the deterrence calculation (by introducing uncertainty about guilt into the deterrent signal)?',
    'Data on exonerations in death-penalty cases; statistical models of false-conviction risk in capital cases; analysis of whether publics update deterrence beliefs based on exoneration events',
    'High innocent execution risk undermines the rationality assumption: the deterrent signal becomes unreliable (executions may not be of actual murderers), and rational actors cannot trust the punishment-crime linkage. The coordination function degrades (deterrence to whom, through what signal?). Theater_ratio rises; ε may rise to snare territory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innocent_execution_risk, empirical, 'Risk of executing innocent people and impact on deterrence signal credibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_kill_det_theater_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(state_kill_det_theater_t3, state_killing_legitimacy__deterrence_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(state_kill_det_theater_t6, state_killing_legitimacy__deterrence_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(state_kill_det_extract_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(state_kill_det_extract_t3, state_killing_legitimacy__deterrence_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(state_kill_det_extract_t6, state_killing_legitimacy__deterrence_reading, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% The deterrence reading, retributive reading, and abolition reading form a constraint family decomposed from the contested kernel 'state_killing_legitimacy.' Each reading has its own ε value, beneficiary/victim structure, and legitimacy claims. The deterrence reading (ε=0.48) instruments the condemned as means to future-crime prevention. The retributive reading (separate file) instrumentalizes the condemned as bearer of proportional desert. The abolition reading (separate file) rejects instrumentalization entirely. All three are linked via network.affects_constraints to show family membership. They are not alternative measurements of one constraint (per ε-invariance principle); they are structurally distinct readings of a single ambiguous kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
