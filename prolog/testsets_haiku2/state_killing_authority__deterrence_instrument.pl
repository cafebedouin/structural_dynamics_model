% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the deterrence reading of the contested
 *   kernel: state killing authority. The deterrence reading justifies capital
 *   punishment if and only if it causally prevents future murders at an
 *   acceptable cost-benefit ratio. Under this reading, the condemned person
 *   is an instrumental cost (not a rights-bearer or desert-subject), and the
 *   beneficiary set includes unnamed future potential murder victims whose
 *   lives would be saved by the deterrent effect. The constraint is presented
 *   as conditional on an empirical claim (deterrence works); if the empirical
 *   claim fails, the justification collapses entirely. This reading coexists
 *   with the retributive reading (punishment is justified as desert,
 *   independent of consequences) and the categorical abolitionist reading
 *   (state killing is inherently impermissible regardless of efficacy). The
 *   three readings share the same kernel (state authority to execute) but
 *   ground that authority in different normative commitments and empirical
 *   assumptions.
 *
 * KEY AGENTS:
 *   - state_prosecutorial_authority: agenda-setter (institutional power) — administers and enforces the constraint, has institutional interest in preserving capital punishment authority
 *   - condemned_persons: victims/payers (powerless, trapped) — face execution as the ultimate cost, instrumentalized as means to future prevention
 *   - future_potential_murder_victims: beneficiaries (powerless, unnamed) — ostensibly protected by deterrent effect but cannot organize or participate; benefit is counterfactual
 *   - empirical_criminology_research: observer (non-agent) — the deterrent claim hinges entirely on whether research establishes deterrence works; the research is contested
 *   - abolitionist_advocates: excluded (organized, mobile) — contest the constraint's legitimacy through legal and legislative challenge; their exclusion reflects the reading's epistemic claim to authority over the empirical question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.68).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.72).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, 'a987eee3-b080-4b55-8e2e-ad0867444ccb').
narrative_ontology:cs_kernel_codification('a987eee3-b080-4b55-8e2e-ad0867444ccb', formalized).
narrative_ontology:cs_authority_grounding('a987eee3-b080-4b55-8e2e-ad0867444ccb', extraction).
narrative_ontology:cs_interpretation_layer_present('a987eee3-b080-4b55-8e2e-ad0867444ccb').
narrative_ontology:cs_reading_relation('a987eee3-b080-4b55-8e2e-ad0867444ccb', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('a987eee3-b080-4b55-8e2e-ad0867444ccb', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('a987eee3-b080-4b55-8e2e-ad0867444ccb', foundational, execution_justified_if_deters_murder).
narrative_ontology:cs_axiom_status(execution_justified_if_deters_murder, holdable).
narrative_ontology:cs_axiom_grounding('a987eee3-b080-4b55-8e2e-ad0867444ccb', execution_justified_if_deters_murder, empirically_contingent).
narrative_ontology:cs_axiom('a987eee3-b080-4b55-8e2e-ad0867444ccb', foundational, future_victim_safety_consequentialist_metric).
narrative_ontology:cs_axiom_status(future_victim_safety_consequentialist_metric, holdable).
narrative_ontology:cs_axiom_grounding('a987eee3-b080-4b55-8e2e-ad0867444ccb', future_victim_safety_consequentialist_metric, instrumental).
narrative_ontology:cs_reference_frame('a987eee3-b080-4b55-8e2e-ad0867444ccb', deterrence_efficacy_justifies_execution).
narrative_ontology:cs_drift_state('a987eee3-b080-4b55-8e2e-ad0867444ccb', contemporary_post_meta_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a987eee3-b080-4b55-8e2e-ad0867444ccb', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, state_prosecutorial_authority).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, families_of_condemned).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, victims_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are members of the future population whose lives would ostensibly be protected by the deterrent effect of capital punishment. They are unnamed, unorganized, and their benefit is conditional on the empirical claim that executions reduce homicides. They cannot organize or participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_murder_victims, beneficiary,
    powerless, immediate, trapped, national).

% Administers and enforces capital punishment law. Possesses the authority to seek and execute sentences. Has institutional interest in preserving the legal framework that authorizes its most extreme enforcement power, and in sustaining the claim that this power serves a crime-prevention function rather than other motives.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_prosecutorial_authority, agenda_setter,
    institutional, generational, analytical, national).

% Face execution as the ultimate cost. Under the deterrence reading, they are instrumental — their death is justified as a means to future prevention, not as a desert or rights-respecting outcome. They bear the cost with no exit and cannot participate in the decision whether the deterrent claim is empirically true.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, national).

% Experience the loss of a family member and the state's claim that this loss serves future prevention. They may organize to oppose execution or contest the deterrent claim, but their legal standing is limited and their exit from the kinship relationship that grounds their stake is identity-locked.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, families_of_condemned, payer,
    moderate, biographical, constrained, national).

% The body of research on whether capital punishment causally reduces homicides. Included here as a non-agent entity because the deterrence reading's entire justification pivots on an empirical claim: whether execution prevents future murders. The research community produces competing findings; the constraint's legitimacy within this reading depends on that research verdict.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, empirical_criminology_research, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(state_killing_authority__deterrence_instrument, empirical_criminology_research).

% Advocate for the rights of murder victims and bereaved families. They often support capital punishment as a form of justice or closure for past crimes, though the deterrence reading justifies execution not as desert for past wrongs but as prevention of future ones. This creates a structural gap: they may support the constraint for reasons other than deterrence, even as the constraint's justification within this reading is purely consequentialist.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, victims_rights_advocates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, victims_rights_advocates, observer).

% Oppose capital punishment on grounds of categorical rights (death penalty violates inherent human dignity) or empirical insufficiency (deterrence claim is false or the cost exceeds any benefit). They are excluded from the constraint's administration but contest its legitimacy through legal challenge, legislative advocacy, and public discourse. Their excluded position within this reading reflects the deterrence reading's claim to epistemic authority over the empirical question.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Past victims of homicide whose deaths ground the criminal cases. They are deceased and non-agents. The deterrence reading instrumentalizes future prevention in their name but may not honor their own deaths as anything other than data in the deterrence calculation.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, murder_victims, observer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(state_killing_authority__deterrence_instrument, murder_victims).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, state_prosecutorial_authority).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal mechanism for matching the ultimate penalty (execution) to the ultimate crime (premeditated murder) with the justification that the certainty and severity of this penalty deters future potential murderers from committing capital crimes. Solves a coordination problem: what penalty is proportionate to premeditated murder, and how should the state exercise ultimate authority?
% TRANSFER_FUNCTION: Transfers the condemned person's life to the state as payment for a future deterrent benefit accruing to unnamed future potential murder victims. The state gains authority to execute as an instrument of crime prevention.
% ABSENT_VOICES: The empirical criminology research community is divided on whether deterrence is real; the research record is contested, and the deterrent benefit (future victims protected) is counterfactual and cannot organize or testify. Abolitionist scholars and death-penalty reform advocates argue the deterrent claim is empirically false or that the cost exceeds any benefit, but the deterrence reading excludes them by asserting the empirical question is settled (deterrence works). Their exclusion is the exclusion of the epistemic challenge itself.
% DISAPPEARANCE_RATIONALE: Abolitionists argue the world would rearrange only marginally: homicide rates would stay stable (deterrence is false) and imprisonment would substitute for execution (alternative penalty remains), and the state would lose a tool that provides no marginal safety benefit. Deterrence proponents argue eliminating executions would lose the deterrent effect, allowing future murders that executions would have prevented — the world would rearrange into a higher-homicide scenario. The disappearance verdict hinges on the empirical claim.
% FOUNDING_PROBLEM: Premeditated murder (lex talionis): the problem of what penalty befits the taking of a life. Secondarily, how to prevent future murders through severe penalties.
% FOUNDING_PROBLEM_CORROBORATION: The retributive reading (sibling constraint) attests the founding problem remains live as a justice question: the taking of a life requires an equivalent penalty as a matter of desert. The deterrence reading reframes the founding problem as purely preventive: the original problem is now 'how to prevent murders' and retribution is beside the point. The empirical criminology research community is the external corroborating witness: meta-analyses show no credible evidence that capital punishment has a deterrent effect beyond incapacitation (Nagin & Pepper 2012, National Research Council 2012), suggesting the founding problem (murder prevention via deterrence) is not actually solved by executions. Deterrence proponents point to statistical studies they argue show deterrent effects (Ehrlich 1975 and successors), but these findings are contested within the research community itself and have not achieved consensus.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, contested).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers a condemned person's life to the state on the justification of a counterfactual, future-conditional benefit that cannot be directly observed or verified at the moment of execution. The condemned person bears a certain cost; future victims receive a probabilistic benefit. The asymmetry is structural: the condemned person's death is certain and immediate; the deterrent benefit is uncertain and distributed across unnamed future agents. Suppression is also high (0.72) because sustaining the constraint requires suppressing the empirical challenge itself — the research record shows no credible evidence of deterrent effect, and the constraint's legitimacy depends on treating that research question as settled or as supporting deterrence despite the contested consensus. Theater is moderate (0.41) because execution is performed as ritual justice (formal trial, solemnity, claim to rational procedure) while the actual justification has become increasingly decoupled from its empirical premise. The measurement series show extractiveness rising from 0.58 to a peak of 0.71 at time-point 40 (reflecting intensifying use of execution and hardening of the legal framework), then declining to 0.68 by time-point 50 (reflecting growing judicial and legislative retreat from capital punishment in the U.S. and globally). Theater rises steadily, indicating increasing performative emphasis as empirical grounds weaken. Suppression follows a similar trajectory. The reading's claim/metric divergence is deliberate: the constraint is CLAIMED as a justified preventive instrument (the deterrence reading's own framing) while the metrics describe highly extractive, suppression-dependent operation — the engine measures this gap as the distance between the reading's normative claim and its structural reality.
 *
 * PERSPECTIVAL GAP:
 *   The state prosecutorial authority (agenda-setter seat) experiences the constraint as a justified instrument: capital punishment is a tool that works (deterrence premise accepted) and should be preserved. The condemned person (payer seat) experiences the constraint as instrumental extraction: their life is transferred as payment for a benefit they did not receive and cannot verify. The families of the condemned (payer seat, moderate power) experience the constraint as both loss and contestation: they bear the kinship cost and can question whether the deterrent benefit was real. Future potential victims (beneficiary seat, powerless) cannot experience the constraint at all — they are unnamed, unorganized, and their benefit is counterfactual. The empirical research community (observer seat) sits outside the institutional constraint: its role is to testify whether the deterrent claim is true, but that testimony is excluded from the constraint's self-justification if it contradicts the deterrence premise. The engine computes different type classifications for each seat from the structural data: from the agenda-setter's position, the constraint appears as a rope (coordination + justification); from the condemned person's position, it appears as a snare (extraction + suppression of the empirical challenge).
 *
 * DIRECTIONALITY LOGIC:
 *   Future potential murder victims are declared as beneficiaries because the deterrence reading's entire justification rests on the claim that they benefit from deterrence. However, they are powerless and trapped — they cannot organize, negotiate, or exit. Their benefit is not a revealed preference but a policy claim. State prosecutorial authority is the agenda-setter: it administers the constraint, has institutional power, and has demonstrable incentive to preserve the capital punishment framework (bureaucratic, legal, symbolic). Condemned persons and their families are victims and payers: they bear the cost (death, loss) and have trapped or identity-locked exit (the condemned cannot exit by ceasing to exist; families cannot exit the kinship relationship). The directionality computation should show: agenda-setter at low d (beneficiary/beneficiary of institutional preservation), condemned persons at high d (targets), families at high d with some moderate modulation (they can theoretically exit through legislative advocacy). The powerlessness of future potential murder victims creates a structural asymmetry: they are nominal beneficiaries but have zero agency in the system. This is a red flag for false-beneficiary classification (the benefit is fictitious or imposed) — handled by omega variable on the empirical claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading originally solved a genuine coordination problem: what authority and procedure should govern execution? The state needed a rule that would be seen as rational and justified, not arbitrary. The deterrence frame provided that: 'execution is justified if it prevents murders.' This was live coordination. Over 50+ years, empirical research has not established that deterrence works (meta-analyses find no credible effect); legal scholarship has accumulated showing alternatives (life imprisonment without parole) accomplish incapacitation without execution; and public opinion in developed democracies has shifted toward abolition. The founding problem (what penalty justifies taking a life, how to prevent murder) persists, but the deterrence solution has increasingly failed to deliver. However, the state apparatus continues to invoke the deterrence justification even as it abandons its empirical premise — the constraint persists through performative maintenance (ritual procedure, repeated invocation of deterrence in judicial opinions and policy) despite the mandate being dead. This is mandatrophy: the founding problem that justified the constraint has been solved (or is insoluble within the deterrence frame), but the constraint persists because the institutional actors who administer it have interests in preserving it (institutional authority, symbolic power, organizational continuity). The theater_ratio rise (to 0.41) indicates increasing performative activity relative to functional activity — the constraint is increasingly theater. The extraction remains high (0.68) because the cost to the condemned person is not reduced; only the justification is hollow. Mandatrophy is not yet fully resolved (the constraint still operates in some U.S. jurisdictions and globally), but the mismatch between founding problem (solved or abandoned) and persistence (institutional inertia) is the defining signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_claim,
    'Does capital punishment have a measurable deterrent effect on homicide rates beyond incapacitation (i.e., beyond preventing the specific condemned person from committing future murders)?',
    'Continued empirical criminology research with improved causal identification methods. The National Academies panel (2012) found no credible evidence; ongoing studies attempt to resolve the question. Resolution would require meta-analysis consensus or a well-designed natural experiment (abolition in a comparable jurisdiction).',
    'If deterrence is false or negligible, the entire justification collapses: the constraint extracts a certain death from the condemned person in exchange for zero future benefit. The constraint would shift from Tangled Rope (coordination on justified prevention + extraction as cost of prevention) to pure Snare (extraction with false justification). If deterrence is real and substantial, the constraint remains a justified Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_claim, empirical, 'The core empirical premise of the deterrence reading: whether execution prevents future murders.').

omega_variable(
    acceptable_cost_definition,
    'What cost (in terms of execution of innocent persons, system error, erosion of trust in state authority, brutalization effects, or alternative-penalty efficacy) is acceptable in exchange for the deterrent benefit, assuming deterrence is real?',
    'Public deliberation, legislative decision, and empirical measurement of system error rates and collateral harms. The cost question is partly empirical (how many innocents are executed, what is the empirical effect on public trust) and partly normative (what cost is politically acceptable).',
    'If the cost exceeds the benefit (even if deterrence exists), the constraint loses its conditional justification. If cost is established as unacceptable, the constraint becomes unjustified even if deterrence works.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_definition, empirical, 'The cost-benefit threshold: acceptable cost for the deterrent benefit.').

omega_variable(
    future_potential_victim_agency,
    'Are unnamed future potential murder victims a legitimate beneficiary class under the deterrence reading, given that they cannot organize, consent, or participate in the constraint''s operation?',
    'Normative debate on whether consequentialist benefits to abstract future populations can justify constraints that extract from specific present persons without their participation. Comparison to other constraints that benefit future parties (environmental regulations, public health mandates).',
    'If future potential victims are not a legitimate beneficiary class (because they lack agency, cannot consent, or benefit only hypothetically), then the constraint has no real beneficiary class — it becomes pure Snare rather than Tangled Rope. The constraint would be unjustified even if deterrence works.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_potential_victim_agency, conceptual, 'Whether unnamed future victims constitute a valid beneficiary for a constraint that requires present extraction.').

omega_variable(
    committer_frame_kernel_contestation,
    'Which reading of the state_killing_authority kernel will ultimately prevail in the long term: deterrence, retribution, or categorical abolition?',
    'Historical observation across legislative, judicial, and international policy domains. The deterrence reading appears to be losing ground globally (declining executions, rising abolition); retributive and abolition readings may be displacing it. The contest cannot be decided within the deterrence reading''s own framework — it is a cross-framework question.',
    'If categorical abolition prevails, the deterrence reading becomes a dead letter (superseded constraint). If retribution solidifies, the deterrence justification becomes unnecessary (retribution alone justifies execution). The deterrence reading''s persistence depends on institutional actors continuing to invoke it despite empirical weakness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_contestation, conceptual, 'Long-term displacement of the deterrence reading by sibling readings within the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.32).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__deterrence_instrument, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__deterrence_instrument, theater_ratio, 20, 0.38).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__deterrence_instrument, theater_ratio, 30, 0.41).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.43).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__deterrence_instrument, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__deterrence_instrument, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__deterrence_instrument, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__deterrence_instrument, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__deterrence_instrument, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__deterrence_instrument, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__deterrence_instrument, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__deterrence_instrument, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__deterrence_instrument, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__deterrence_instrument, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel decomposes into three constraint stories corresponding to three distinct readings: deterrence_instrument (THIS story — consequentialist, empirically contingent), retributive_desert (deontological, grounded in desert and proportionality, independent of consequences), and categorical_abolition (grounded in inalienable rights, denies any state authority to execute). Each reading produces a different constraint structure: deterrence is a Tangled Rope conditional on an empirical claim; retribution is a different Tangled Rope grounded in desert; abolition would produce no constraint (instead would produce a prohibition). The three stories are linked as coexisting readings of the same kernel — they do not logically foreclose each other at the global level (different parties hold each), but within any single framework, accepting one reading's core premise creates pressure against the others. The deterrence reading influences (creates downstream pressure on) the retributive reading by shifting justification from desert to consequence — if deterrence works, retribution becomes unnecessary. The categorical abolition reading forecloses both deterrence and retribution within its framework by denying the state any authority to execute regardless of purpose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__deterrence_instrument, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
