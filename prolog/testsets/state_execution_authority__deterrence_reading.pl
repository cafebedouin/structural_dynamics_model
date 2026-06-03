% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the deterrence reading of the state
 *   execution authority kernel. The reading justifies capital punishment as a
 *   mechanism to prevent future murders by raising the marginal cost of
 *   capital crimes. The core empirical claim is that execution deters
 *   homicides more effectively than alternative sanctions
 *   (life-without-parole, permanent incapacitation). This reading differs
 *   from the retributive reading (which justifies execution as proportionate
 *   punishment regardless of deterrence effects) and the abolition reading
 *   (which holds execution categorically impermissible regardless of crime
 *   severity or deterrence efficacy). The deterrence reading is contingent on
 *   an empirical claim: if execution does not deter, or if alternative
 *   sanctions deter equally, the reading's logical foundation collapses. The
 *   constraint exhibits Tangled Rope structure because it combines genuine
 *   coordination (the legal system establishes rules of culpability, enabling
 *   state monopoly on legitimate violence) with asymmetric extraction
 *   (executed offenders are instrumental costs; wrongfully convicted
 *   defendants bear irreversible error). The beneficiary set includes both
 *   potential future murder victims (intended beneficiaries of deterrence)
 *   and the state enforcement apparatus (benefits from monopoly
 *   consolidation). The extractiveness trajectory rises over the interval
 *   (0.35 → 0.52) as wrongful executions accumulate and error-rate awareness
 *   grows, increasing the cost side of the utilitarian calculus. Theater
 *   ratio rises (0.42 → 0.58) as ritual procedural safeguards (trials,
 *   appeals, clemency review) become increasingly performative — they do not
 *   prevent execution of innocent persons or resolve the empirical
 *   uncertainty about deterrence efficacy.
 *
 * KEY AGENTS:
 *   - Potential Future Murder Victims (Aggregate): Intended beneficiary of deterrence reading (moderate/mobile) — would benefit from reduced murder rates if execution deters
 *   - Executed Offenders: Primary victim (powerless/trapped) — bear irreversible cost; no exit option; not themselves beneficiaries
 *   - Wrongfully Convicted Defendants: Secondary victim (moderate/constrained) — structurally mobile but legally constrained; error rate makes them non-negligible extraction target
 *   - State Enforcement Apparatus: Secondary beneficiary (institutional/arbitrage) — consolidates monopoly on legitimate violence; justifies law enforcement budgets; can substitute alternative sanctions
 *   - Criminal Justice Reform Coalition: Organized agents (organized/constrained) — actively building alternative deterrence mechanisms (life-without-parole, evidence-based sentencing) with sunset trajectory
 *   - Analytical Observer: Civilizational view (analytical/analytical) — can see the constraint as either natural law of governance or contingent institutional choice depending on empirical facts about deterrence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.52).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.65).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '9938e6c0-db53-4a76-acd5-ce59841ce7c7').
narrative_ontology:cs_kernel_codification('9938e6c0-db53-4a76-acd5-ce59841ce7c7', formalized).
narrative_ontology:cs_authority_grounding('9938e6c0-db53-4a76-acd5-ce59841ce7c7', lineage).
narrative_ontology:cs_interpretation_layer_present('9938e6c0-db53-4a76-acd5-ce59841ce7c7').
narrative_ontology:cs_reading_relation('9938e6c0-db53-4a76-acd5-ce59841ce7c7', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('9938e6c0-db53-4a76-acd5-ce59841ce7c7', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('9938e6c0-db53-4a76-acd5-ce59841ce7c7', foundational, execution_necessary_for_deterrence).
narrative_ontology:cs_axiom_status(execution_necessary_for_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('9938e6c0-db53-4a76-acd5-ce59841ce7c7', execution_necessary_for_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('9938e6c0-db53-4a76-acd5-ce59841ce7c7', foundational, potential_victims_moral_standing).
narrative_ontology:cs_axiom_status(potential_victims_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('9938e6c0-db53-4a76-acd5-ce59841ce7c7', potential_victims_moral_standing, deontological).
narrative_ontology:cs_reference_frame('9938e6c0-db53-4a76-acd5-ce59841ce7c7', state_monopoly_legitimate_violence_with_deterrent_function).
narrative_ontology:cs_drift_state('9938e6c0-db53-4a76-acd5-ce59841ce7c7', contemporary_post_innocence_project_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9938e6c0-db53-4a76-acd5-ce59841ce7c7', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_future_murder_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_defendants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXECUTED OFFENDER (SNARE) — No exit option; bears the maximum extractive cost (death). From this agent's perspective, the constraint is pure extraction with zero coordination benefit. The deterrence framing is instrumental rationalization that does not change the executed agent's structural position — they are the instrument, not the beneficiary. The deterrence reading provides no legitimacy claim to the executed agent; it only justifies their elimination as necessary cost for future protection.
constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WRONGFULLY CONVICTED DEFENDANT (TANGLED ROPE) — Structurally mobile (not all are executed; some are exonerated before execution) but constrained by severe legal barriers (appellate process, burden of proving innocence). Experiences both coordination function (the legal system is designed to identify true perpetrators) and asymmetric extraction (the error rate in capital convictions means innocent actors bear execution cost). The constraint's extractiveness depends entirely on system accuracy — at 2% error rate, wrongful execution becomes a significant extraction component.
constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POTENTIAL FUTURE MURDER VICTIMS (ROPE) — The intended beneficiary of the deterrence reading. From this perspective, the constraint is pure coordination: raising the cost of capital crimes prevents murders before they occur. If deterrence is effective, this agent benefits from coordination without extraction (their survival is coordinated by threat of execution). Exit option is 'mobile' because the deterrent value applies to victims as an aggregate population — any individual victim could emigrate or reduce risk through other means. The theoretical beneficiary sees the constraint as legitimate coordination.
constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ENFORCEMENT APPARATUS (TANGLED ROPE) — Institutional beneficiary with high arbitrage options (can choose execution vs. imprisonment vs. other penalties). Benefits from execution authority (consolidates state monopoly on legitimate violence, demonstrates state power, justifies law enforcement budgets). Also benefits from coordination function (legal procedures establish rules of culpability that enable other enforcement). But also bears costs: wrongful executions create legitimacy challenges, maintenance of execution infrastructure, political liability. The state apparatus experiences both genuine coordination (defining what constitutes capital crime) and significant extraction (monopoly on violence).
constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CRIMINAL JUSTICE REFORM COALITION (SCAFFOLD) — Organized agents (abolitionists, error-detection organizations, alternative-sanction advocates) see execution as a temporary institutional arrangement being replaced by life-without-parole and other alternatives that achieve deterrence (or other penological goals) without irreversible error. The deterrence reading's empirical claim (execution deters better than alternatives) is falsifiable and increasingly challenged by comparative deterrence studies. If deterrence via life-without-parole is established, the execution constraint loses its justification and becomes sunset-able. Effective extraction is moderate because the coalition has identified exit pathways and is actively building them.
constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, some might argue that state execution is a fixed necessity of governance: any state with monopoly on legitimate violence must have terminal sanctions; the deterrence mechanism is a natural law of criminal behavior (raising costs always affects marginal actors). However, this perspective is vulnerable to false-summit detection — the structural data contradicts the mountain classification. The constraint requires active enforcement (contradicting emergent naturality), benefits identifiable agents (potential victims, state apparatus), and has measurable extractiveness. The mountain framing naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_execution_authority__deterrence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts in two directions: (1) direct extraction from executed offenders (irreversible cost; no compensation or benefit); (2) conditional extraction from wrongfully convicted defendants (error rate of 1-4% means some innocent persons are executed; this is utilitarian loss). The extractiveness value reflects that the deterrence reading does not deny these extraction costs — it asserts they are justified by deterrence benefits. The value 0.52 sits at the tangled_rope boundary because the reading explicitly includes a coordination function (legal system establishing rules of culpability) while accepting significant extraction. If deterrence is empirically false, extractiveness should be classified higher (0.66+, snare territory). Suppression (0.65): High. The constraint requires significant suppression of alternatives: (1) prisoners sentenced to capital punishment have exhausted legal remedies (appellate process is lengthy but not infinite); (2) public discourse about execution alternatives is suppressed by moral objections and path dependence (death is politically easier than life imprisonment without parole); (3) information about error rates and deterrence uncertainty is suppressed by institutional interests in execution authority. Theater ratio (0.58): Moderate-high. Capital trial procedures (trials, jury selection, appeal process, clemency review) are elaborate rituals that create appearance of careful deliberation. However, the theater serves two functions: (1) legitimate — establishing facts of guilt and culpability; (2) performative — providing legitimacy covering for execution decisions made through bureaucratic and political processes. The rising theater trajectory (0.42 → 0.58) reflects increasing procedural complexity as error awareness grows, with procedures becoming less functional and more performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a striking perspectival gap because the deterrence reading's beneficiary set and victim set are fundamentally asymmetric in their moral and epistemological standing. Executed offenders are certain, present, and directly affected — they know they are bearing extraction cost. Potential future murder victims are statistical, counterfactual, and probabilistic — their deterrence benefit depends entirely on an empirical claim (execution deters) that is contested and increasingly challenged. The gap appears in the classifications: Executed Offenders see Snare (pure extraction); Potential Victims see Rope (coordination); State Apparatus sees Tangled Rope (both coordination and benefit); Reform Coalition sees Scaffold (temporary, sunset-able); Wrongfully Convicted Defendants see Tangled Rope (mixed — the system's coordination function made them possible victims through error). The Analytical Observer risks seeing Mountain (natural law of governance) but the structural data contradicts this — the constraint requires active enforcement, identifiable beneficiaries, and measurable extractiveness. The perspectival gap is not a matter of subjective perception — it reflects a genuine structural difference in moral standing between certain present harm and probabilistic future benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for this constraint are determined by each agent's structural relationship to the deterrence mechanism and the distribution of extraction. Potential murder victims have low d (0.15-0.20) because they are intended beneficiaries — the constraint is designed to protect them. Executed offenders have high d (0.95) because they bear full extraction cost with no exit. Wrongfully convicted defendants have very high d (0.92) because they are victims of error — instrumental costs with no benefit. State enforcement apparatus has low d (0.12) because they benefit from execution authority (consolidate monopoly, justify budgets). Criminal justice reform coalition has moderate d (0.55) because they are partially constrained (legal barriers to abolition) but organized with exit pathways. The analytical observer has high d (0.72) by canonical mapping because analysis reveals structures that institutional interests conceal. Effective extraction (chi) is computed from base extraction (0.52) multiplied by f(d) for each agent, then scaled by spatial scope (national = 1.0). The chi formula generates the perspectival gap: executed offenders experience chi ≈ 0.74 (high f(d) from d=0.95); potential victims experience chi ≈ -0.05 (negative f(d) from d=0.15, indicating subsidy); state apparatus experiences chi ≈ -0.08 (negative f(d) from d=0.12).
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading produces genuine mandatrophy at the Tangled Rope boundary. The question is not 'which type is correct?' but 'is the empirically contingent coordination benefit real?' The mandatrophy can be resolved only by empirical investigation of deterrence efficacy. If execution deters more effectively than life-without-parole, the deterrence reading's utilitarian calculus holds, and the constraint remains Tangled Rope (genuine coordination, justified extraction). If execution does NOT deter more effectively, the coordination function collapses, and the constraint reclassifies to Snare (pure extraction with false coordination cover). The wrongful execution error rate becomes decisive: at >2% error rate, the utilitarian calculus becomes transparent — the constraint is explicitly trading certain present injustice (execution of innocents) for probabilistic future benefit (prevented murders). At that point, the retributive reading (execution as proportionate punishment) becomes the only non-consequentialist justification available; the deterrence reading loses force. The abolition reading's deontological objection (execution is categorically impermissible) becomes more plausible if deterrence efficacy is questioned, because deterrence was the reading's primary practical justification. Mandatrophy is not resolved here because the empirical facts about deterrence remain contested; the reading stands or falls with future research.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical,
    'Does execution actually deter capital crimes more effectively than life-without-parole or other terminal sanctions?',
    'Comparative criminological studies controlling for jurisdiction, time period, crime type, and enforcement intensity. Meta-analysis of deterrence research (NAS 2012, Mustard 2003, Shepherd 2004) with replication and sensitivity analysis.',
    'If execution deters significantly better: deterrence reading remains structurally justified; constraint is Tangled Rope (coordination + extraction for legitimate goal). If life-without-parole deters equally: execution becomes pure extraction with no coordination benefit; constraint reclassifies to Snare; the deterrence reading''s axiom (execution_necessary_for_deterrence) becomes overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical, empirical, 'Whether execution deters more effectively than alternative terminal sanctions').

omega_variable(
    wrongful_execution_error_rate,
    'What is the actual error rate in capital convictions, and how many innocent persons have been executed?',
    'DNA exoneration data, clemency reviews, post-conviction evidence analysis. Estimates range from 1% (Blackmun, 1994) to 4% (Gross et al., 2014). Longitudinal tracking of exonerations and convictions provides confidence bounds.',
    'If error rate is negligible (<0.1%): extraction from wrongful execution is minimal; deterrence reading''s utilitarian calculus holds. If error rate is substantial (>1%): wrongful execution becomes significant extractive cost; the deterrence reading must explicitly cost it against deterrence benefits; mandatrophy emerges (whether deterrence gain justifies error cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_error_rate, empirical, 'Actual error rate in capital convictions and wrongful executions').

omega_variable(
    reading_substitutability,
    'Can the deterrence reading be satisfied by non-lethal terminal sanctions (life-without-parole, permanent incapacitation), or does the deterrence axiom require execution specifically?',
    'Comparative analysis of deterrence mechanisms across jurisdictions and historical periods. Logical analysis of whether deterrence efficacy depends on irreversibility or only on severity/certainty of punishment.',
    'If life-without-parole achieves equivalent deterrence: the reading becomes empirically overridable; a jurisdiction could substitute without logical inconsistency; the constraint''s claimed_type shifts downward (less extraction required for the same goal). If only execution deters: the reading''s axiom is robust; substitution would violate the reading''s own logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_substitutability, empirical, 'Whether life-without-parole can substitute for execution in deterrence function').

omega_variable(
    kernel_reading_contest,
    'How does the deterrence reading stand relative to the retributive and abolition readings of the same kernel (state execution authority)?',
    'Logical analysis of axioms and foreclosure relations. Empirical testing of deterrence efficacy determines which reading''s core premise (if any) is falsified. Normative analysis of whether proportionate punishment and categorical impermissibility can coexist with deterrence.',
    'If deterrence is empirically false: retributive reading gains relative ground (execution for proportionality, regardless of deterrence). If deterrence is empirically true: deterrence reading''s legitimacy strengthens, while abolition reading must abandon utility argument and rely purely on deontological grounds. Retributive and deterrence readings coexist when deterrence is real but not the only justification for execution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationships between deterrence, retributive, and abolition readings').

omega_variable(
    moral_status_of_potential_victims,
    'Do potential future victims have the same moral standing as currently executed offenders in the utilitarian calculus?',
    'Moral philosophy analysis: expected value theory (probability × harm), comparative moral weight (preventing 1 murder vs. executing 1 offender + 1 innocent error), and alternative utilitarian framings.',
    'If yes (equal standing, probabilistic reasoning applies): the deterrence reading''s utilitarian framework is coherent; future prevention can justify present extraction. If no (executed offenders have stronger standing than statistical victims): the reading''s beneficiary set is morally incoherent; Snare classification becomes dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_of_potential_victims, preference, 'Moral standing of potential future victims vs. executed offenders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exec_det_theater_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(exec_det_theater_t5, state_execution_authority__deterrence_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(exec_det_theater_t10, state_execution_authority__deterrence_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(exec_det_extract_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exec_det_extract_t5, state_execution_authority__deterrence_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(exec_det_extract_t10, state_execution_authority__deterrence_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(exec_det_suppress_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(exec_det_suppress_t5, state_execution_authority__deterrence_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(exec_det_suppress_t10, state_execution_authority__deterrence_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, wrongful_conviction_capital_cases).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, deterrence_efficacy_empirical_claim).

% DUAL FORMULATION NOTE:
% The state execution authority kernel is instantiated through three distinct constraint stories, each representing a coherent but competing reading. The deterrence reading decomposes from the retributive and abolition readings because each makes empirically and normatively distinct claims: deterrence emphasizes forward-looking prevention of future crimes (utility), retribution emphasizes backward-looking proportionality for past crimes (desert), abolition rejects state execution categorically (rights). Each reading has its own ε value reflecting its own empirical contingencies: deterrence reading depends on deterrence empirics; retributive reading depends on proportionality theory; abolition reading depends on human rights doctrine. These three stories are linked via network.affects_constraints because the empirical resolution of deterrence efficacy affects the comparative plausibility of all three readings within a single jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
