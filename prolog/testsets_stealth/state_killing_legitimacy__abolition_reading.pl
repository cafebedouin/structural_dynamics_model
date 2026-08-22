% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: State Killing Legitimacy - Abolition Reading: Capital Punishment as Categorical Dignity Violation
 *   domain: criminal justice/political philosophy/legal theory
 *
 * SUMMARY:
 *   This story instantiates the abolition reading of the
 *   state_killing_legitimacy kernel. The referent of every metric is the
 *   standing arrangement under contest - capital punishment as practiced in
 *   retentionist jurisdictions - assessed strictly by this reading's own
 *   lights: state killing categorically violates human dignity regardless of
 *   the offender's desert or the execution's utility. The reading's endorsed
 *   alternative (abolition) is NOT the referent; authoring epsilon for the
 *   endorsed alternative would drive it to zero for every advocacy reading
 *   and destroy the corpus's comparative value. The claim/metric relationship
 *   is deliberately unreconciled: claimed_type is authored from this
 *   reading's structural assessment of the arrangement, and the metrics are
 *   authored descriptively from the historical record of its operation. A
 *   retributive or deterrence seat examining the same world should compute a
 *   different type - that divergence is the measurement the kernel family
 *   exists to take.
 *
 * KEY AGENTS:
 *   - condemned_persons: primary target (powerless/trapped) - surrenders life itself to the arrangement
 *   - wrongfully_convicted_death_row_prisoners: error-rate casualties (powerless/trapped until release)
 *   - state_execution_authorities: administering seat (institutional/constrained) - operates the machinery and receives its central output
 *   - punitive_political_coalitions: principal beneficiary (powerful/mobile) - converts the penalty into electoral capital
 *   - prosecutorial_leverage_offices: secondary beneficiary (institutional/constrained) - converts the death question into plea leverage
 *   - retributivist_constituencies: expressive beneficiary (organized/constrained)
 *   - deterrence_theory_adherents: doctrinal beneficiary (analytical/arbitrage)
 *   - execution_teams: dual-positioned administrator-payer (moderate/constrained) - performs the act and absorbs its psychological cost
 *   - capital_jurors: burden-bearing payers (moderate/constrained)
 *   - families_of_the_executed: bereaved payers (moderate/constrained)
 *   - abolition_movements: excluded challengers (organized/constrained) - public voice, no decision seat
 *   - international_human_rights_bodies: analytical observers (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "State Killing Legitimacy - Abolition Reading: Capital Punishment as Categorical Dignity Violation").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal justice/political philosophy/legal theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '40d564ab-30b3-417d-8154-1c6c8bb55089').
narrative_ontology:cs_kernel_codification('40d564ab-30b3-417d-8154-1c6c8bb55089', formalized).
narrative_ontology:cs_authority_grounding('40d564ab-30b3-417d-8154-1c6c8bb55089', lineage).
narrative_ontology:cs_interpretation_layer_present('40d564ab-30b3-417d-8154-1c6c8bb55089').
narrative_ontology:cs_reading_relation('40d564ab-30b3-417d-8154-1c6c8bb55089', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('40d564ab-30b3-417d-8154-1c6c8bb55089', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('40d564ab-30b3-417d-8154-1c6c8bb55089', foundational, human_dignity_forbids_intentional_state_killing).
narrative_ontology:cs_axiom_status(human_dignity_forbids_intentional_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('40d564ab-30b3-417d-8154-1c6c8bb55089', human_dignity_forbids_intentional_state_killing, deontological).
narrative_ontology:cs_axiom('40d564ab-30b3-417d-8154-1c6c8bb55089', secondary, desert_and_utility_insufficient_licensing_grounds).
narrative_ontology:cs_axiom_status(desert_and_utility_insufficient_licensing_grounds, holdable).
narrative_ontology:cs_axiom_grounding('40d564ab-30b3-417d-8154-1c6c8bb55089', desert_and_utility_insufficient_licensing_grounds, deontological).
narrative_ontology:cs_reference_frame('40d564ab-30b3-417d-8154-1c6c8bb55089', inviolable_dignity_limit).
narrative_ontology:cs_drift_state('40d564ab-30b3-417d-8154-1c6c8bb55089', contemporary_global_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('40d564ab-30b3-417d-8154-1c6c8bb55089', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, punitive_political_coalitions).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, prosecutorial_leverage_offices).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, retributivist_constituencies).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, deterrence_theory_adherents).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, wrongfully_convicted_death_row_prisoners).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, families_of_the_executed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, execution_teams).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, capital_jurors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death and held under a death warrant, typically in isolated confinement for years while appeals run. Everything they retain - contact, autonomy, eventually life itself - sits in the hands of the state's penal apparatus. There is no exit: appeals can move the date but the terminus is fixed unless a governor, board, or court intervenes; they did not choose entry and cannot choose departure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, payer,
    powerless, biographical, trapped, national).

% Convicted of capital crimes they did not commit and later exonerated, usually after a decade or more on death row. They demonstrate the error rate the arrangement runs as a matter of course. Release restores liberty but not the years, health, or family bonds lost; some are freed only by journalism, DNA testing, or luck rather than the system's own review machinery.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, wrongfully_convicted_death_row_prisoners, payer,
    powerless, biographical, trapped, national).

% Relatives of executed prisoners. They lose a family member to a deliberate state act and then live inside the aftermath - some report the promised finality never arrives, others report anguish at kinship to a killing done in their name. They attend executions as witnesses only when the state permits it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, families_of_the_executed, payer,
    moderate, generational, constrained, regional).

% Departments of corrections and attorneys general that operate death rows, schedule executions, and defend the penalty in court. They receive the arrangement's central output: the demonstrated authority to terminate life under law. Maintenance costs are visible - execution drug supply collapsed after manufacturers refused participation, forcing secrecy statutes and improvised protocols.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_execution_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Correctional officers, medical staff, and contractors who prepare and carry out executions. They perform the act and absorb its immediate human cost: documented rates of post-traumatic stress, dissociation, and career-ending moral injury among volunteer teams. Individual refusal carries professional penalty; participation carries psychological ones.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, execution_teams, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, execution_teams, payer).

% Legislators and executives who campaign on the penalty and collect electoral returns from defending it. Support or opposition functions as a signature wedge position; several prominent politicians have reversed stance when constituencies shifted, indicating the attachment is strategic rather than principled.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, punitive_political_coalitions, beneficiary,
    powerful, biographical, mobile, national).

% District attorney offices that charge capital-eligible cases. The death question functions as systematic plea leverage: defendants accept life terms to remove execution risk, closing cases without trial. Capital prosecutions also confer career distinction on the prosecutors who pursue them.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, prosecutorial_leverage_offices, beneficiary,
    institutional, biographical, constrained, regional).

% Citizens and victims-rights groups whose demand for proportional payment for murder is answered by the execution. Their gain is expressive: the state affirms that the worst act receives the ultimate response. The constituency is internally divided - a visible minority of murder-victim families publicly oppose executions and report the process prolongs rather than closes their grief.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retributivist_constituencies, beneficiary,
    organized, generational, constrained, national).

% Criminologists, economists, and commentators whose scholarship holds that execution saves lives by signaling consequences. Their reputations and research programs are invested in the signal thesis; the academic market lets them move freely between posts, but the thesis itself is the foundation of their position.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, deterrence_theory_adherents, beneficiary,
    analytical, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, deterrence_theory_adherents, observer).

% NGOs, defense-bar networks, faith bodies, and exoneree-led campaigns working to end the penalty. They hold loud public voice but no seat inside the decisions that matter - clemency boards, warrant schedules, and charging decisions are closed to them, and several jurisdictions have passed secrecy rules specifically limiting their access to execution information.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolition_movements, excluded,
    organized, generational, constrained, global).

% Treaty bodies and courts - the European human-rights system, UN treaty monitors - that adjudicate abolition norms and document retentionist practice. They shape legitimacy conditions and ratification incentives but hold no enforcement seat inside sovereign criminal justice systems.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% Citizens seated in capital trials who must answer the death question directly. Instructions tell them they are finding facts, but the sentence routes through their hands; post-service studies document lasting distress, sleep disruption, and guilt among jurors who voted for death. Their service ends; the weight does not.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, capital_jurors, payer,
    moderate, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the political community's response to its most severe crimes around a shared, terminal expression of condemnation: it fixes an ultimate sanction ceiling, promises irrevocable justice for the worst acts, and marks the boundary of acts the community treats as unforgivable.
% TRANSFER_FUNCTION: Transfers the condemned person's remaining life - and the legal authority over it - to the state's penal apparatus for deliberate termination; secondarily transfers the kill-decision burden to jurors and execution teams, and converts capital cases into electoral and plea-leverage capital for prosecutors and punitive coalitions.
% ABSENT_VOICES: The condemned are absent from every forum that decides the penalty's legitimacy after sentencing - they await death in isolation while legislatures, courts, and clemency boards deliberate. The wrongfully executed can never testify; posthumous exonerations speak only through files. Executed persons' families are heard only through intermediaries. Future generations who will inherit the precedent have no seat. Abolition organizations hold public voice but no vote inside clemency and warrant procedures.
% DISAPPEARANCE_RATIONALE: Death rows, specialized appellate tracks, clemency machinery, execution protocols, and the capital-defense litigation economy would dismantle or convert overnight; life-without-parole becomes the terminal sanction; punitive coalitions lose a signature wedge issue and prosecutors lose a lever. Nothing physical or logical forces reinstatement - the rearrangement is institutional, not natural-law.
% FOUNDING_PROBLEM: Early modern states built capital punishment to demonstrate that the sovereign commanded life and death, to deter grave crime through exemplary consequence, and to satisfy proportional desert where blood had been taken - the ultimate answer to the ultimate transgression.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated as contested from outside the benefiting parties: the US National Research Council's 2012 committee found the deterrence literature incapable of supporting the signal thesis; supermajority surveys of criminologists reject the deterrence claim; European human-rights jurisprudence (Protocols 6 and 13, the Soering line) attests the sovereignty-display function is obsolete. On the other side, state prosecutors' associations and retentionist legislatures attest the desert-and-protection problem remains live. No neutral arbiter exists; the liveness itself is the dispute.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.95, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits near the ceiling (0.95) because the good this arrangement takes is life itself - the one holding that cannot be returned or compensated - and the series saturates there: the act does not dilute, and the accumulating exoneration record only raises the assessed cost of running it. Suppression (0.72 scalar) tracks enforcement capacity rather than the condemned's trappedness, which is absolute at every point: the series shows an enforcement ratchet peaking around T24-T32 (habeas narrowing, procedural-default hardening, secrecy statutes) followed by capacity decay as drug manufacturers refused supply, moratoria spread, and execution counts fell. Theater ratio rises monotonically (0.38 to 0.60): as the act became ritually managed - sterile protocols, last-meal ceremonies, clemency performances, humanness litigation, drug-sourcing secrecy - a growing share of activity dignifies or defends the act rather than performs any penal function the reading recognizes. Accessibility collapse is low (0.40) because alternatives demonstrably persist: every abolition jurisdiction runs life-without-parole, so the arrangement survives only where actively chosen and maintained. Resistance is high (0.68): abolition campaigns, exoneration-driven litigation, international treaty pressure, and supply-side refusal all press continuously. All three series share one time grid (T0 approximates 1972, the Furman-era interruption; T52 approximates the present); no cyclical pattern is asserted - the dynamics are a ratchet followed by decay, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge sharply and the engine should compute different types from the same structural data. From the condemned person's seat the arrangement is total: no exit, no compensation, no appeal to any alternative frame - maximal severity with zero mobility. From the punitive coalition's seat the same events are a low-cost, high-return expressive asset with full positional mobility (politicians flip when constituencies shift). From the retributivist constituency's seat the identical acts may compute as deserved delivery rather than harm at all - the perspectival gap here is not informational but normative, which is precisely why the kernel decomposes into readings. Inter-institutionally, international human-rights bodies observe a convergence trendline while domestic administrators manage case loads; same-level lateral divergence appears between retentionist and abolitionist jurisdictions operating identical legal heritage. The engine computes these divergences from power, exit, and directional data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Condemned persons and wrongfully convicted prisoners sit at the full-target end: victims with trapped exit, no arbitrage, no secondary channel of gain. Families of the executed are targets with slightly moderated d through their partial expressive stake. Punitive political coalitions derive low d (beneficiaries with mobile exit - the nearest the derivation reaches toward the subsidy end among real actors). Prosecutorial offices derive low d despite constrained exit because the leverage flow is unconditional. Retributivist constituencies derive low d with the caveat, routed through the retributive_satisfaction_valence omega, that their benefit is partly self-harming. Deterrence theorists combine beneficiary role with arbitrage exit - nearest the beneficiary pole of any seat. Execution teams are the genuinely dual-positioned seat: they administer the machinery (agenda-setting) while absorbing its documented psychological cost (paying); the secondary_role declaration carries this duality to the engine without a power-atom override, since an override keyed to 'moderate' would also distort capital_jurors, who are straightforward payers with high d. International bodies and the analytical seats sit near symmetry as observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested, and the classification machinery is what keeps that contest honest. The arrangement's defenders present it as coordination - justice delivery, deterrence signaling, closure provision - and a naive reading of the coordination function alone would license a rope-like verdict. The structural data block that move: the coordination story does not require the killing (expressive_substitutability omega), the enforcement ratchet peaked decades after any functional need it served, and the beneficiary set consists of actors who collect expressive, electoral, and leverage rents rather than parties who would suffer a coordination failure in its absence. Conversely, the analysis prevents the opposite mislabeling: the boundary-maintenance function is real and widely felt, which is why the arrangement retains a mass constituency and why abolition proceeds jurisdiction-by-jurisdiction rather than by collapse. If the founding problem resolves dead (deterrence null, desert satisfiable by life-without-parole, sovereignty display obsolete) while the arrangement persists, the status-dead x world-rearranges mismatch fires the zombie-mandate flag and the arrangement migrates toward inertial persistence maintained by identity and electoral habit rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_multiplicity,
    'Which reading of the state_killing_legitimacy kernel governs a given jurisdiction, and how would adopting a sibling reading restructure this constraint''s victim and beneficiary sets?',
    'Jurisdiction-level adoption events: constitutional entrenchments, treaty ratifications, and statutory repeals reveal which reading each polity has operationalized; cross-jurisdiction comparison separates the readings'' structural signatures.',
    'Under the retributive reading the condemned shifts from rights-bearer to forfeited-life-right holder and this story''s victim set dissolves; under the deterrence reading the condemned becomes an instrument of a signal and the beneficiary set expands to the protected population. This file''s classification holds only for the abolition reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'This constraint is one of three readings of one kernel; sibling adoptions restructure the stakeholder surface wholesale.').

omega_variable(
    dignity_categorical_vs_balanceable,
    'Is human dignity a categorical limit on state purposes, or a value balanceable against desert and utility claims?',
    'Conceptual: track whether any jurisdiction''s doctrine successfully balances dignity against consequentialist or desert grounds without collapsing into one of the sibling readings; the stability of the balancing attempt is the test.',
    'If dignity is balanceable, the categorical prohibition softens into a weighted consideration and this reading converges toward regulated-retention hybrids; if categorical, no desert or utility showing can reopen the question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_categorical_vs_balanceable, conceptual, 'Location of the core disagreement between this reading and both siblings.').

omega_variable(
    deterrence_empirical_status,
    'Does execution produce a marginal deterrent effect on homicide that lesser sanctions do not?',
    'Continued panel studies and abolition natural experiments; the US National Research Council''s 2012 committee found existing studies incapable of answering the question; new identification strategies or long-run cross-jurisdiction data could.',
    'Sustained null results strip the deterrence reading''s empirical grounding and strengthen this reading''s claim that no utility showing licenses killing; a robust positive effect would revive the strongest sibling challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Empirical status of the deterrence signal thesis.').

omega_variable(
    expressive_substitutability,
    'Can the arrangement''s expressive and boundary-maintenance function - collective condemnation of the worst acts - be delivered fully by life-without-parole?',
    'Abolition jurisdictions serve as natural experiments: measure whether condemnation expression, victim-family outcomes, and boundary signaling degrade after repeal.',
    'Full substitutability confirms the coordination story does not require death and the arrangement''s persistence rests on the killing itself; non-substitutability would credit part of the structure as genuine coordination and soften the computed classification for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expressive_substitutability, empirical, 'Whether the coordination function requires the killing or merely the condemnation.').

omega_variable(
    retributive_satisfaction_valence,
    'Does execution deliver net benefit to the constituencies whose desert demands it invokes, or does the process impose costs on them as well?',
    'Longitudinal study of murder-victim families through the capital process, including the substantial minority who oppose execution; valence is a value question the data can inform but not settle.',
    'If the process harms its own claimed beneficiaries, the beneficiary set shrinks and the support coalition narrows further; if satisfaction is real, those seats remain net beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retributive_satisfaction_valence, preference, 'Valence of retributive satisfaction for the arrangement''s expressive beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 52).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__abolition_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__abolition_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__abolition_reading, theater_ratio, 24, 0.47).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__abolition_reading, theater_ratio, 32, 0.53).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__abolition_reading, theater_ratio, 40, 0.57).
narrative_ontology:measurement(stat_tr_t46, state_killing_legitimacy__abolition_reading, theater_ratio, 46, 0.59).
narrative_ontology:measurement(stat_tr_t52, state_killing_legitimacy__abolition_reading, theater_ratio, 52, 0.6).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__abolition_reading, base_extractiveness, 8, 0.91).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__abolition_reading, base_extractiveness, 16, 0.92).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__abolition_reading, base_extractiveness, 24, 0.93).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__abolition_reading, base_extractiveness, 32, 0.94).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__abolition_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(stat_be_t46, state_killing_legitimacy__abolition_reading, base_extractiveness, 46, 0.95).
narrative_ontology:measurement(stat_be_t52, state_killing_legitimacy__abolition_reading, base_extractiveness, 52, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__abolition_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__abolition_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__abolition_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__abolition_reading, suppression_requirement, 32, 0.84).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__abolition_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(stat_su_t46, state_killing_legitimacy__abolition_reading, suppression_requirement, 46, 0.75).
narrative_ontology:measurement(stat_su_t52, state_killing_legitimacy__abolition_reading, suppression_requirement, 52, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, identity_coordination).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel, three epsilon-invariant readings. The retributive and deterrence readings are historically upstream - their doctrines are cited to sustain the standing arrangement - while this abolition reading is the downstream challenger that denies both licensing grounds. Each reading authors its own epsilon over the same referent (the standing arrangement of state killing): this reading assesses it near the ceiling because it counts the taking of life itself as the transferred good; a sibling reading assesses the same arrangement as justified delivery. Cross-links route contamination and displacement analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
