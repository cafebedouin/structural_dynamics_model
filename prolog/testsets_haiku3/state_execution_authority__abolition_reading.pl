% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: criminal_justice/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the ABOLITION READING of the contested
 *   kernel state_execution_authority. The abolition reading treats state
 *   execution as categorically impermissible regardless of crime severity,
 *   procedural safeguards, or claimed legitimizing purposes (retribution,
 *   deterrence, closure). From this reading's epistemic seat, all executed
 *   persons and death-sentenced persons are victims; no legitimate
 *   beneficiaries exist; the constraint persists through institutional
 *   inertia, victim-family advocacy, and retributive narrative despite being
 *   foundationally illegitimate. This reading is one of three:
 *   retributive_reading (proportionate punishment is justice) and
 *   deterrence_reading (execution prevents future murders) are the sibling
 *   readings; they share the kernel but read it differently and instantiate
 *   different constraints with different beneficiary/victim structures and ε
 *   values. The claim/metric divergence is authored deliberately:
 *   claimed_type is snare (pure extraction, no legitimate coordination
 *   function) while the abolition reading measures very high extractiveness
 *   (0.92) because execution is a categorical violation that admits no
 *   substitution—the state could imprison instead, but abolition rejects
 *   imprisonment-with-execution-threat as a coordination solution to the
 *   problem of dangerous offenders. The constraint is not a mountain: it is
 *   constructed (law, procedure, institutional choice) and benefits
 *   identifiable parties (retributive apparatus, victim-family advocacy
 *   narratives) at the cost of condemned persons.
 *
 * KEY AGENTS:
 *   - executed_persons: fully powerless, trapped, ultimate payers (their life is extracted)
 *   - capital_defendants: powerless, identity-locked in state legal machinery, face extraction of life or long imprisonment
 *   - state_execution_apparatus: institutional agenda-setter (laws, courts, execution procedures), arbitrage exit (could abolish but chooses to maintain)
 *   - victims_families_of_murdered: organized, positioned as beneficiaries of retributive narrative, secondary payers (perpetuate killing-as-justice framework)
 *   - abolition_movement: organized, excluded from governing justifications, constrained exit (democratic reform only)
 *   - wrongfully_convicted_exonerees: living proof of systemic illegitimacy; executed exonerees have no voice
 *   - execution_workers: moderate power, constrained exit, payers (forced to participate in illegitimate killing; moral injury common)
 *   - legislatures_and_courts: institutional agenda-setters, arbitrage exit (could abolish by statute/amendment), choose to maintain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.92).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.78).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority (Abolition Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '7b42c040-97ba-41df-8238-48f5ff6db398').
narrative_ontology:cs_kernel_codification('7b42c040-97ba-41df-8238-48f5ff6db398', fixed_text).
narrative_ontology:cs_authority_grounding('7b42c040-97ba-41df-8238-48f5ff6db398', extraction).
narrative_ontology:cs_interpretation_layer_present('7b42c040-97ba-41df-8238-48f5ff6db398').
narrative_ontology:cs_reading_relation('7b42c040-97ba-41df-8238-48f5ff6db398', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('7b42c040-97ba-41df-8238-48f5ff6db398', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('7b42c040-97ba-41df-8238-48f5ff6db398', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7b42c040-97ba-41df-8238-48f5ff6db398', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('7b42c040-97ba-41df-8238-48f5ff6db398', foundational, human_dignity_inalienable_and_absolute).
narrative_ontology:cs_axiom_status(human_dignity_inalienable_and_absolute, holdable).
narrative_ontology:cs_axiom_grounding('7b42c040-97ba-41df-8238-48f5ff6db398', human_dignity_inalienable_and_absolute, deontological).
narrative_ontology:cs_reference_frame('7b42c040-97ba-41df-8238-48f5ff6db398', categorical_human_dignity_inalienable).
narrative_ontology:cs_drift_state('7b42c040-97ba-41df-8238-48f5ff6db398', contemporary_post_exoneration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7b42c040-97ba-41df-8238-48f5ff6db398', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, capital_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, victims_families_of_murdered).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, victims_families_of_murdered).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, execution_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the ultimate extraction: their life is taken by state authority as punishment or supposed deterrent. Under the abolition reading, they are victims regardless of guilt or crime severity because the state has no categorical authority to execute. Their powerlessness is structural: no appeal, no exit, no negotiation. The constraint operates through their elimination.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, national).

% Face potential execution through capital trials; live under the threat and uncertainty of death sentences. Their situation combines powerlessness (limited legal resources, evidentiary disadvantages, appellate exhaustion) with identity-locked dependence on state legal machinery (must participate in trials, appeals, clemency processes that may be performative). The suppression is dual: external (state enforcement machinery, execution procedures) and internalized (learned helplessness, procedural compliance despite existential threat).
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, capital_defendants, payer,
    powerless, biographical, trapped, national).

% Administers capital punishment through law, trial procedure, appellate review, and execution protocol. Justifies the system as retributive (proportionate punishment for heinous crimes) or deterrent (raising the cost of capital crimes). Under the abolition reading, these justifications are rejected; what remains is institutional extraction of state authority over life and death, defended by procedural theater (clemency boards, appeals, mental competency reviews) that create the appearance of careful judgment while the categorical power to execute persists.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Experience loss from homicides and often advocate for capital punishment as justice or closure. Under the abolition reading, they are positioned as beneficiaries of the retributive narrative (the state acts in their name to execute the murderer) but are also payers in that the constraint perpetuates the moral framework of killing-as-justice rather than breaking the cycle. Their organizational power (victim advocacy groups, testimony) is real but structurally asymmetric to the execution apparatus's institutional power.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, victims_families_of_murdered, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__abolition_reading, victims_families_of_murdered, payer).

% Organized advocates who reject capital punishment on human-rights, categorical, or utilitarian grounds. They are excluded from the governing justification framework of retribution and deterrence; their objections are treated as minority preference rather than as evidence that the constraint's foundational claims are false. Their access to courts, legislatures, and clemency processes is limited; their primary channel is democratic reform, which is slow and locally contingent.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolition_movement, excluded,
    organized, generational, constrained, global).

% Were death-sentenced and later exonerated by DNA evidence or innocence projects. Those executed cannot testify; those surviving carry trauma and legal disability (barriers to compensation, employment, social reintegration). Under the abolition reading, they are the proof of systemic illegitimacy: a system that kills innocents has no categorical authority to kill anyone. Their exclusion is structural — once executed, they have no voice; before exoneration, their objections are treated as standard appeals rather than as evidence the system is broken.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_exonerees, excluded,
    powerless, biographical, trapped, national).

% Carry out executions: guards, medical personnel, technicians operating lethal injection protocols. Under the abolition reading, they are also payers—forced to participate in what the reading frames as illegitimate killing. Their exit is constrained by employment (state employment, limited alternative opportunities), legal duty (participating in lawful orders), and identity-lock (role as corrections officer or medical provider becomes inseparable from the execution function). Trauma and moral injury are common; the apparatus treats their participation as professional duty, not as extraction.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, execution_workers, payer,
    moderate, biographical, constrained, national).

% Maintain capital punishment law, appellate procedures, and constitutional doctrine permitting execution. They could abolish the constraint by statute or constitutional amendment; they choose to maintain it. Under the abolition reading, their role is not merely administering a legitimate state function but actively perpetuating a categorical violation of human dignity. Their arbitrage option (change the law) is real but politically costly; inertia and the power of the retributive narrative keep them in place.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% International human rights standards (European Convention on Human Rights, UN Mandela Rules, abolitionist majority among UN member states) treat state execution as a violation of human dignity. The norm entity does not collect or pay; it is a reference frame against which the executing jurisdiction's claims are measured. From the abolition reading's perspective, the executing state positions itself outside a global consensus on categorical prohibition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, comparative_global_norm, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(state_execution_authority__abolition_reading, comparative_global_norm).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, state_execution_apparatus).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Capital punishment claims to coordinate a state function: proportionate punishment (retribution), deterrence of future murders, and closure for victim families. The abolition reading rejects these claimed functions as legitimate justifications and treats the underlying coordination claim (that the state has authority to decide who may live) as itself the contested object.
% TRANSFER_FUNCTION: Moves legal authority to kill from private citizens (vigilante killing is illegal) to the state, in exchange for procedural regularity and the promise of justice. Under the abolition reading, this transfer is a one-way extraction: condemned persons lose their right to life; the state gains power of life and death. The only transfer TO the condemned is a death sentence.
% ABSENT_VOICES: Wrongfully convicted exonerees (who would testify that the system kills innocents); execution workers (whose moral objections are overridden by duty); abolition movements (treated as minority preference rather than evidence the constraint is illegitimate); and the executed dead (who cannot testify to the experience of state killing). The constraint's foundational justifications (retribution, deterrence) are authored by the beneficiary apparatus and victims' families; objecting voices are structurally excluded from the framework that defines legitimate punishment.
% DISAPPEARANCE_RATIONALE: If state execution disappeared overnight (abolished), criminal justice systems would reorganize around life imprisonment without parole, alternative victim restitution frameworks, and different narratives of legitimate punishment. The loss of the death penalty would not collapse law-and-order function; it would shift the moral justification from retribution/deterrence (which the abolition reading rejects) to incapacitation, rehabilitation, and dignity-respecting accountability. Victim families would grieve differently; execution workers would cease. The constraint is not a natural limit on punishment—it is a discrete institutional choice that, if removed, leaves law-and-order function intact but transforms its legitimacy story.
% FOUNDING_PROBLEM: Capital punishment originated in premodern societies with limited incarceration capacity and state authority organized around visible punishment and deterrence signaling (torture, execution, display). The founding problem was: how to deter future crime and satisfy a public sense of proportionate justice when resources were scarce? Execution was one solution among several (mutilation, branding, enslavement).
% FOUNDING_PROBLEM_CORROBORATION: Modern penology (incarceration, long-term imprisonment) solves the original capacity problem without execution. Empirical criminology (from outside the retributive apparatus and from comparative jurisdictions that abolished execution) attests that murder rates do not depend on capital punishment—deterrence claims are not supported by evidence. Life imprisonment provides incapacitation (the murderer cannot kill again in society) without categorical killing. The founding problem (scarce caging, need for visible deterrence) has been solved by alternative means; the constraint persists through institutional inertia, retributive narrative, and victim-family advocacy. Corroboration from abolitionist jurisdictions: 143 UN member states have abolished capital punishment; murder rates in abolitionist countries are not higher than in retentionist states; the founding problem has demonstrably ceased to drive the constraint's persistence.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   EXTRACTIVENESS (0.92): The abolition reading measures execution as categorical extraction—no legitimate purpose (retribution and deterrence are rejected as justifications), no substitutable alternative (life imprisonment preserves the legitimate penalty for heinous crime without the ultimate extraction of life), and no exit from the death-sentence mechanism (appeals are procedural theater that preserve the authority to kill while creating appearance of care). The measurement series shows stable high extraction (0.85–0.92) across the interval: as international abolitionist consensus grows and wrongful-execution evidence accumulates, the reading's case strengthens, but the state apparatus sustains the constraint through institutional inertia. SUPPRESSION (0.78): Active enforcement is required—state must actively exclude alternative framings (abolition, human-rights readings), suppress objections from condemned and their advocates, and maintain execution procedures against growing public opposition. Suppression is dual: structural (legal barriers to clemency, appellate exhaustion, execution protocols) and internalized (victim families accept retributive narrative; jurors internalize duty to condemn; execution workers internalize obedience to law). The measurement series is flat (0.76–0.78): suppression does not rise because international pressure is real but non-coercive; domestic suppression suffices. THEATER (0.42): Procedural machinery creates appearance of careful judgment—clemency boards, appellate review, mental competency examinations, automatic stays for constitutional review. Under the abolition reading, these procedures are theater that preserves the categorical authority to execute while creating the illusion that killing is the outcome of rigorous deliberation. Theater ratio is flat (0.38–0.42) because the apparatus must maintain this performance to sustain legitimacy against objections; as evidence of wrongful execution accumulates (exonerations, DNA testing), the theatrical function intensifies (more procedural safeguards, more stays, more claims of refinement) without changing the underlying extraction. ACCESSIBILITY_COLLAPSE (0.81): Once a death sentence is imposed, alternatives collapse almost completely—clemency is rare, pardon is rarer, appellate reversal is low-probability. The condemned person's exit from the death sentence becomes identity-locked (bound to state legal machinery, appeals, clemency processes that preserve the authority to kill). The accessibility collapse is slightly lower than a mountain (not 0.85+) because appellate process itself creates some friction and occasional exonerations show the system is not absolutely closed; but the collapse is substantial. RESISTANCE (0.73): Capital punishment meets real resistance from abolition movements, human-rights organizations, victim families (Murder Victims' Families for Reconciliation), exonerees and their lawyers, and growing portions of the public. This is higher than a pure mountain (which meets near-zero resistance) and reflects that the constraint is constructed and contestable. The measurement series are all authored on one shared time grid (every metric at every time point) so temporal alignment is preserved and the compiler will not inject end-state values at earlier times.
 *
 * PERSPECTIVAL GAP:
 *   THE PAYER SEAT (capital defendants, executed persons): experiences the constraint as categorical illegitimate extraction. From the condemned person's position, appeals and clemency are procedural theater that preserve the state's authority to kill while creating appearance of deliberation. Exit is impossible. Suppression is overwhelming and internalized (legal powerlessness, familial abandonment, loss of status). The constraint is experienced as pure coercion. THE AGENDA-SETTER SEAT (state apparatus, courts, legislatures): experiences the constraint as a legitimate legal/moral function. From this seat, capital punishment is retributive justice (proportionate punishment for heinous crime) or deterrent (preventing future murders). Procedures (trial, appeal, clemency) are genuine safeguards, not theater. The constraint is experienced as coordination around legitimate state authority. The abolition reading rejects the agenda-setter's justifications and computes its classification from the payer's structural position. The engine will compute per-seat types: from the payer seat, snare (pure extraction, high suppression, victims); from the agenda-setter seat, the retributive/deterrence readings would compute as rope or tangled_rope (coordination + legitimate punishment). This divergence is structural asymmetry the reading acknowledges: execution is experienced as fundamentally different depending on whether you are the one being executed or the one ordering it.
 *
 * DIRECTIONALITY LOGIC:
 *   BENEFICIARIES (REJECTED BY THIS READING): Retributive apparatus, victim-family closure narratives. The abolition reading does not deny that these entities/narratives exist or that victim families genuinely grieve; it rejects that state execution is a legitimate response to their grief or that retribution is a categorical good. Therefore, the abolition reading does NOT list these as beneficiaries in the classical sense (collecting legitimate benefit from a coordination function). Instead, the reading treats them as ideological justifications that sustain a snare. VICTIMS (CORE READING): All executed persons (d = 1.0, fully targeted), capital defendants (d = 0.95, nearly fully targeted, slight uncertainty from innocence project exonerations that create hope), execution workers (d = 0.85, forced participation, constrained exit). DIRECTIONALITY DERIVATION: Executed persons are structurally trapped (no exit), face immediate extraction (death), belong to powerless population (no political influence over their fate). From base power (powerless) + time_horizon (immediate) + exit_options (trapped), directionality is 1.0 (full target). Capital defendants are powerless, identity-locked in state legal machinery (appellate exhaustion), face biographical extraction (waiting for execution, life imprisonment, or release). Directionality is 0.95 (nearly full target, slightly modulated down because appellate process creates fractional hope). Execution workers are moderate power, constrained exit (employment, legal duty), biographical horizon. Directionality is 0.85 (substantial target, modulated by moderate power and occupational constraint rather than powerlessness). Victim families are organized power, biographical horizon, constrained exit (cultural/relational ties to victims, justice narratives). Under the abolition reading, they are secondary beneficiaries of the retributive narrative (the state acts in their name) but also secondary payers (perpetuate killing-as-justice framework). Their directionality is 0.55–0.60 (symmetric to slightly targeted, depending on whether they actively advocate for execution or passively accept state action).
 *
 * MANDATROPHY ANALYSIS:
 *   FOUNDING PROBLEM (premodern): State execution solved a real coordination problem in societies with limited incarceration capacity—how to deter future crime and satisfy public justice without building expensive prisons? Execution was visible, memorable, and low-cost. FOUNDING PROBLEM STATUS (abolished): Modern incarceration, long-term imprisonment, and specialized prisons solve the original problem without execution. The constraining resource (secure confinement capacity) is no longer scarce. Comparative data: 143 UN member states have abolished capital punishment; those states maintain functional criminal justice systems without execution. The founding problem (scarcity of secure confinement) has been solved by alternative means. MANDATROPHY: The abolition reading identifies this as a MANDATROPHY case—the founding problem is dead, yet the constraint persists through institutional inertia, victim-family advocacy, retributive narrative, and legislative resistance to change. The constraint exhibits theater_ratio of 0.42, indicating a growing share of procedural activity (clemency boards, appellate stays, competency examinations, post-conviction DNA testing) that creates appearance of careful deliberation while preserving the underlying authority to execute. As evidence of wrongful execution accumulates (exonerations), the apparatus responds by adding more procedural safeguards (more theater) rather than questioning the categorical authority. This is textbook mandatrophy: the original coordination function is dead (no resource scarcity), the constraint persists through institutional maintenance and narrative cover, and the apparatus responds to evidence of failure by intensifying procedural theater. RESOLUTION (abolition reading): The constraint should be abolished because (a) the founding problem is demonstrably solved by alternative means, (b) empirical evidence refutes deterrence claims (the deterrence reading's empirical grounding has been overridden), (c) wrongful execution evidence proves the system kills innocents (abolition reading's core proof), and (d) international human rights consensus treats execution as a violation of human dignity (global-norm influences_downstream pressure). The abolition reading's mandatrophy analysis predicts that without abolition, the constraint will persist indefinitely through institutional inertia and victim-family advocacy, acquiring more procedural theater and more wrongful executions, until either political pressure or constitutional amendment forces change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is state execution a legitimate instrument of criminal justice (retributive/deterrent readings) or a categorical violation of human dignity (abolition reading)?',
    'This is a committer-axis contest at the kernel level: the three readings (abolition, retributive, deterrence) each instantiate different constraints with different beneficiary/victim structures and different ε values. No empirical fact alone resolves the contest because it is a disagreement about the scope of state authority and the moral status of the condemned. The resolution is normative/constitutional: which reading does the jurisdiction adopt as its governing framework?',
    'This omega documents that the constraint classification (snare, high extraction, victim set includes all executed persons) is READING-DEPENDENT. A retributive reading would classify the same institutional practice as rope or tangled rope (coordination around justice + legitimate punishment). A deterrence reading would classify it as rope (coordination around crime prevention) conditional on empirical claims about deterrent effect. This reading (abolition) classifies it as snare because it rejects both legitimizing narratives and treats all execution as illegitimate extraction of state power over life.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The constraint is one reading of a contested kernel; sibling readings produce different classifications.').

omega_variable(
    wrongful_execution_epistemology,
    'How many wrongful executions is consistent with a system claiming legitimate authority to execute?',
    'Empirical: DNA exonerations, innocence project work, post-conviction DNA testing databases provide a floor estimate. The abolition reading treats ANY wrongful execution as proof of systemic illegitimacy (the state cannot reliably distinguish guilty from innocent, therefore has no authority to kill at all). Retributive/deterrence readings treat wrongful execution as a regrettable error within an otherwise legitimate system—improve procedures, raise evidentiary standards, but maintain the authority. The readings differ on how evidence of error maps to normative conclusions.',
    'If the system is estimated to have executed 4-6% innocent people (Gross et al. 2014), the abolition reading invokes this as the core proof: you have already killed innocents, you will kill more, therefore you have no categorical authority. Retributive readings respond by demanding procedural reform, not abolition. The empirical fact (wrongful execution rate) is the same across readings; the normative interpretation differs structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_epistemology, empirical, 'Whether wrongful execution disproves the system''s legitimacy or motivates procedural reform.').

omega_variable(
    deterrence_empirical_contingency,
    'Does capital punishment deter murder more than life imprisonment?',
    'Criminology: comparative studies (abolitionist vs. retentionist jurisdictions), time-series analysis of murder rates after abolition, meta-analyses of deterrence studies. The National Research Council (2012) found no credible evidence that execution deters murder; murder rates in abolitionist countries are comparable to or lower than retentionist states.',
    'This omega documents a key structural difference between the abolition reading and the deterrence reading: the deterrence reading''s legitimacy DEPENDS on an empirical claim (execution deters murder) that is now contested and largely refuted. The abolition reading is INDEPENDENT of deterrence empirics—it rejects deterrence as a justification regardless of whether it works. If deterrence doesn''t work, the deterrence reading collapses; the abolition reading is unaffected. This is an axiom_overriding mechanism for the deterrence reading (empirically_contingent grounding type).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_contingency, empirical, 'Whether execution deters murder; the deterrence reading''s legitimacy depends on this; the abolition reading does not.').

omega_variable(
    victim_family_identity_lock,
    'Are victim families structurally dependent on capital punishment for closure and justice, or would alternative accountability frameworks (truth commissions, restorative justice, life imprisonment certainty) satisfy their legitimate need for acknowledgment?',
    'Sociological/comparative: documented cases where victim families support abolition (Murder Victims'' Families for Reconciliation); comparative study of victim-family outcomes in abolitionist vs. retentionist jurisdictions; victim-advocacy group membership and position on capital punishment.',
    'The abolished reading positions victim families as beneficiaries of the retributive narrative (execution in their name) but also as payers (perpetuating a framework of killing-as-justice). If victim families are not actually dependent on execution for closure—if other accountability forms satisfy them—then their role shifts from structural beneficiary to secondary-role (beneficiary + payer) with lower net directionality benefit. This affects the reading''s claim about who genuinely benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_family_identity_lock, empirical, 'Whether victim families are structurally dependent on capital punishment or would accept alternative accountability.').

omega_variable(
    internationalization_pressure,
    'As 143 UN member states move toward abolition, does the global norm create structural pressure on retentionist jurisdictions to follow, or does it entrench domestic opposition through sovereignty claims?',
    'Political science: documented cases of norm diffusion (European abolition movements), cases of entrenchment (U.S. political resistance to international pressure), treaty accessions and denunciations, legislative debates citing international standards.',
    'The abolition reading invokes international human-rights consensus as corroboration (the executing state is an outlier). If the norm creates genuine structural pressure toward abolition (through EU membership conditions, treaty obligations, soft-law reputational cost), the constraint faces influences_downstream pressure from the international system. If sovereignty claims entrench retentionism, the constraint is self-defending and normalization is slower. This affects the trajectory of the constraint''s persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internationalization_pressure, empirical, 'Whether international abolition norms create structural pressure toward abolition or entrench domestic resistance.').

omega_variable(
    execution_apparatus_identity_fusion,
    'Is execution-worker participation in capital punishment structural (occupational duty, payroll dependency) or identity-fused (corrections officer professional identity inseparable from execution)?',
    'Qualitative: interviews with execution workers, documentation of moral injury and trauma, cases where workers refuse participation or seek alternative employment, comparative study of how corrections systems organize execution roles in retentionist states.',
    'Under the abolition reading, execution workers are payers (forced to participate in illegitimate killing). If participation is purely structural (external pressure, employment dependence), abolition removes the constraint and they exit. If participation is identity-fused (their professional identity is constituted through their role in the system), abolition creates identity dislocation and requires reorientation—post-abolition trauma and resistance may persist. This affects the read of suppression_mechanism ambiguity (internalized vs. structural).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(execution_apparatus_identity_fusion, empirical, 'The degree to which execution workers are identity-locked into participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(stat_tr_t5, state_execution_authority__abolition_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(stat_tr_t35, state_execution_authority__abolition_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(stat_be_t5, state_execution_authority__abolition_reading, base_extractiveness, 5, 0.87).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.88).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.9).
narrative_ontology:measurement(stat_be_t35, state_execution_authority__abolition_reading, base_extractiveness, 35, 0.91).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement(stat_su_t5, state_execution_authority__abolition_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(stat_su_t35, state_execution_authority__abolition_reading, suppression_requirement, 35, 0.78).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__abolition_reading, 0.05).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% state_execution_authority is a contested kernel with three structural readings: abolition_reading (this constraint—categorical prohibition, snare classification, all executed as victims), retributive_reading (proportionate punishment—rope/tangled_rope classification, legitimate authority), deterrence_reading (prevents future murders—rope classification conditional on empirical deterrence claims). Each reading instantiates a different constraint with different beneficiary/victim structures and ε values. The readings are linked by network.affects_constraints and documented in corresponding cs_structure blocks. Abolition influences retributive and deterrence by creating normative pressure (if execution is categorically impermissible, retributive and deterrence justifications are overridden); deterrence reading is empirically_contingent (refuted deterrence claims trigger axiom_overriding drift for that reading, leaving abolition unaffected). Three separate JSON files, one per reading, comprise the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__abolition_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
