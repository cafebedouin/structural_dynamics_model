% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   This constraint is ONE READING of the contested kernel
 *   state_killing_authority. The deterrence reading grounds the state's
 *   authority to execute in empirical efficacy: capital punishment is
 *   justified if and only if it prevents future murders at acceptable cost.
 *   This reading treats capital punishment as a crime-prevention mechanism,
 *   with future potential victims as the beneficiary set and condemned
 *   persons as instrumental costs. The structure generates a tangled_rope:
 *   genuine coordination function (crime reduction) paired with asymmetric
 *   extraction (condemned persons bear the entire cost). The constraint is
 *   read differently by retributive and abolitionist traditions, which authur
 *   different ε values, beneficiary structures, and victim sets from the same
 *   standing arrangement (the state's practice of executing murderers).
 *   Measurement data span 1972 (Furman v. Georgia moratorium + reinstatement)
 *   to 2024, tracking extractiveness accumulation as empirical challenges to
 *   deterrence mount while the practice persists, theater rising as
 *   justificatory rhetoric decouples from empirical foundation.
 *
 * KEY AGENTS:
 *   - future_potential_victims: Hypothetical, powerless, powerless, cannot voice preferences, treated as beneficiary constituency on behalf of whom condemned persons are killed.
 *   - condemned_persons: Powerless, trapped, no exit, instrumentalized as means to crime reduction.
 *   - state_execution_authority: Institutional, administers the arrangement, grounds authority in deterrence efficacy.
 *   - criminal_justice_empiricists: Observers, institutional, measure whether deterrence actually operates.
 *   - abolitionist_movements: Excluded, organized, object on grounds of inalienability or rights doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.68).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.72).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '3b23f50d-bf5f-4bbf-b23f-d263082f7dc1').
narrative_ontology:cs_kernel_codification('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', formalized).
narrative_ontology:cs_authority_grounding('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', extraction).
narrative_ontology:cs_interpretation_layer_present('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1').
narrative_ontology:cs_reading_relation('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', foundational, efficacy_justifies_state_killing).
narrative_ontology:cs_axiom_status(efficacy_justifies_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', efficacy_justifies_state_killing, empirically_contingent).
narrative_ontology:cs_axiom('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', foundational, future_crime_prevention_overrides_condemned_person_death).
narrative_ontology:cs_axiom_status(future_crime_prevention_overrides_condemned_person_death, holdable).
narrative_ontology:cs_axiom_grounding('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', future_crime_prevention_overrides_condemned_person_death, instrumental).
narrative_ontology:cs_reference_frame('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', state_monopoly_crime_prevention).
narrative_ontology:cs_drift_state('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', contemporary_empirical_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b23f50d-bf5f-4bbf-b23f-d263082f7dc1', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, society_crime_reduction).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hypothetical people who would be murdered if deterrence fails. They cannot voice preferences (do not yet exist as determinate actors), cannot negotiate terms, and cannot refuse the arrangement made on their behalf. They are the justificatory seat — the reading asserts their lives saved constitute the state's warrant for killing condemned persons.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_victims, beneficiary,
    powerless, immediate, trapped, national).

% A vindicated proposition (not an agent collecting rents): the claim that capital punishment reduces homicide rates through deterrent effect. Listed here for narrative completeness; compiled to constraint_vindicates/2, not to beneficiary derivation or directionality.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, society_crime_reduction, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_killing_authority__deterrence_instrument, society_crime_reduction).

% Convicted murderers. Under this reading, they are instrumental means to crime reduction — their execution is justified by its hypothetical preventive effect on future murders, not by their desert or by recognition of their agency. They have no exit; they cannot refuse the arrangement or negotiate its terms. The reading treats their death as the cost of the coordination benefit (crime prevention).
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, national).

% The state apparatus that legislates, judges, and executes capital punishment. Under this reading, the state's authority to kill is grounded in efficacy — crime prevention outcome — rather than in desert doctrine or inalienable-rights prohibition. The state administers the arrangement and sustains enforcement by justifying each execution as a necessary cost of deterrence.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_execution_authority, agenda_setter,
    institutional, generational, constrained, national).

% The actual people who would have been murdered if deterrence operates. They are excluded from the conversation that justifies capital punishment — their actual preferences about the trade-off (their lives saved vs. condemned persons killed on their behalf) are not solicited. They exist only as a counterfactual premise in the reading's justification.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, victims_of_murders_prevented, excluded,
    powerless, immediate, trapped, national).

% Researchers and statisticians who measure whether capital punishment actually deters homicide. This reading's entire justificatory force depends on empirical facts they produce. Their measurements can refute the reading's core premise; they occupy the analytical seat where the truth-conditions for the deterrence claim are adjudicated.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminal_justice_empiricists, observer,
    institutional, generational, analytical, national).

% Organized groups that reject capital punishment on grounds of inherent impermissibility or rights doctrine. Under this reading, their objections are treated as preferences about values (inalienability, inherent dignity) that do not override the empirical justification (deterrence efficacy). They are excluded from the conversation not by force but by the reading's structural prioritization of consequentialist over deontological grounds.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_movements, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, state_execution_authority).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes individuals deemed too dangerous or deterrent-effective to leave alive from the population, purportedly preventing future murders by eliminating high-risk persons and creating fear of death in other potential offenders. The reading frames this as solving the collective action problem of crime prevention through a state monopoly on lethal enforcement.
% TRANSFER_FUNCTION: Moves the life of a condemned person (converted to a prevented-murder deterrence benefit) from the condemned person to society and future potential victims. The condemned person's death is the transfer mechanism; the beneficiary is the statistical reduction in future homicides.
% ABSENT_VOICES: Future potential victims cannot voice their actual preferences about the trade-off (their lives saved via deterrence vs. condemned persons killed on their behalf); victims of prevented murders exist only as counterfactual actors; abolitionist moral frameworks that reject the instrumental framing are structurally excluded by the reading's prioritization of empirical efficacy over deontological constraint.
% DISAPPEARANCE_RATIONALE: Abolitionist and retributive readings contest this. Under the deterrence reading: if capital punishment disappeared, homicide rates would rise by some magnitude (contested empirically), imposing a cost on society and future potential victims. Under retributive and abolitionist readings: if capital punishment disappeared, justice would be served more completely (proportional punishment for desert, or recognition of inalienable rights), and homicide rates would not substantially shift (empirically contested).
% FOUNDING_PROBLEM: How can the state prevent future murders when murderers cannot be reliably rehabilitated or incapacitated by incarceration alone? The deterrence reading locates the founding problem in the failure of non-lethal crime control: imprisonment does not stop murders because potential offenders discount the risk or the state's incapacitation capacity is finite.
% FOUNDING_PROBLEM_CORROBORATION: Deterrence proponents (certain criminologists, some prosecutors) attest the founding problem is live: even life sentences do not prevent future murders in society or inside prisons. Abolitionists and empiricists outside deterrence advocacy cite decades of criminological research showing no reliable deterrent effect measurable above noise; they attest the founding problem has been empirically superseded (the deterrent premise fails) and the arrangement persists for non-empirical reasons (retributive desert, state capacity display, or political-economy inertia).
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, contested).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness at 0.68 reflects sustained killing of condemned persons whose death serves a hypothetical coordination benefit (future crime prevention). The reading claims the coordination is real; the metrics treat the actual extraction as substantive: condemned persons have no voice in the arrangement, cannot refuse, and bear the entire cost. Suppression at 0.72 reflects active enforcement: the state must prevent both (a) condemned persons' escape and (b) abolitionist movements' disruption of the practice. Theater at 0.44 reflects the gap between the stated justification (deterrence) and actual operation: executions proceed regardless of empirical falsification, and procedural legitimacy (appeals, clemency review) performs compliance while actual grounds shift (desert doctrine, retributive narrative, or pure state capacity display emerge as the deterrence premise weakens). Accessibility collapse at 0.58 reflects the reading's own logic: alternatives (life imprisonment, incapacitation, rehabilitation) are not foreclosed by the structure; the reading asserts deterrence superiority empirically, not logical necessity. Resistance at 0.79 is high because the reading meets sustained, organized opposition (abolitionist movements, criminal justice research challenging deterrence, victims' families opposing execution). The measurement series track extractiveness rising slowly (from 0.55 to 0.68) as the empirical case for deterrence erodes while executions continue — a classic mandatrophy signal (the founding problem no longer justifies the arrangement, but suppression remains).
 *
 * PERSPECTIVAL GAP:
 *   The state_execution_authority and future_potential_victims seats should compute as perceiving genuine coordination (crime prevention justified by deterrence efficacy). The condemned_persons seat should compute as perceiving pure extraction (instrumentalization without consent or benefit). The empiricists seat should compute as analytical observer whose measurements can refute the entire reading. The abolitionist_movements seat should compute as excluded from the coordination function but targeted by suppression. The engine's per-seat classification will diverge sharply: state authority sees rope (coordination), condemned persons see snare (extraction), empiricists see contingent classification (dependent on deterrence data), abolitionists see snare with exclusion. The authored claim (tangled_rope) sits between these seats' divergent computations; the divergence IS the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Future potential victims are beneficiaries under this reading (lives saved via deterrence), so they derive d near 0.0 (beneficiary end) — though their d is paradoxical because they have no voice, no exit, and no determinate identity. Condemned persons are victims (instrumental cost of deterrence), powerless and trapped, so they derive high d near 1.0 (target end). State execution authority is the agenda_setter (administers the arrangement, sustains enforcement), institutional power, constrained exit (cannot unilaterally abandon the practice), so d sits in the middle-to-beneficiary region (around 0.3–0.4): the state collects the crime-reduction coordination benefit and exercises monopoly authority. Criminal justice empiricists are analytical observers (d = 0.5 by definition) whose measurements determine whether the arrangement's empirical premise holds. Abolitionist movements are excluded from the coordination function but pay a cost (suppression of their speech and advocacy), so they would derive toward the target end (high d, around 0.7–0.85) IF they were counted as parties — but their exclusion means they are not integrated into the constraint's structure by the reading's own logic. The beneficiary/victim declarations (future_potential_victims, society_crime_reduction as beneficiaries; condemned_persons as victims) feed this directionality chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The rising theater ratio (from 0.32 to 0.44) and modest extractiveness growth (0.55 to 0.68) despite stable suppression (0.58 to 0.72) suggests mandatrophy in motion. The founding_problem_status is 'contested': deterrence proponents say crime prevention remains a live problem and executions remain justified; empiricists and abolitionists say the empirical premise has failed (no measurable deterrent effect) and the arrangement now persists for non-empirical reasons (desert doctrine, state authority display, political economy). If the empirical premise is indeed falsified (omega resolution: deterrence_empirical_claim = empirical effect is zero or noise), then founding_problem_status = dead and the arrangement is zombie-classified: it persists despite its justification collapsing. The theater ratio rising suggests increasing performance of legitimacy (procedural review, clemency consideration, careful sentencing) to maintain the arrangement while its ground erodes. Mandatrophy_resolved is not authored because the omega remains open: depending on omega resolution, the constraint may or may not be classified as a zombie (dead founding problem + continued operation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_claim,
    'Does capital punishment have a measurable deterrent effect on future homicides that exceeds the background reduction from incapacitation and that is statistically distinguishable from noise?',
    'Meta-analysis of criminological studies controlling for confounds (economic cycle, policing intensity, incarceration rate, demography). The National Research Council, Pew research summaries, and the International Evidence-Based Crime and Justice Research Community represent independent scholarly adjudication outside deterrence advocacy.',
    'If deterrence effect is zero or statistically indistinguishable from noise, the reading''s core justification collapses: the arrangement persists but the ground for it shifts from empirical efficacy to desert, state authority display, or institutional inertia (moving toward piton or snare classification). If deterrence effect is robustly measurable and substantial, the reading''s empirical premise holds and the arrangement retains its consequentialist justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_claim, empirical, 'Whether the empirical premise of the deterrence reading — capital punishment prevents future murders — is true.').

omega_variable(
    future_victim_identity_status,
    'Can hypothetical future murder victims meaningfully be treated as a beneficiary constituency whose interests ground the state''s authority to execute, when they have no determinate identity, cannot voice preferences, and do not consent to the arrangement made on their behalf?',
    'Philosophical analysis of the conditions for constituency and standing; empirical investigation of how societies make decisions about counterfactual beneficiaries (e.g., environmental policy protecting unborn generations, nuclear-waste containment). The question probes whether the reading''s beneficiary structure is coherent or a cover for instrumental use of condemned persons.',
    'If hypothetical future victims cannot bear standing as a beneficiary constituency without consent-grounds or identity, the reading''s coordination framing collapses into pure extraction (condemned persons are sacrificed for state crime-control capacity, not for identifiable future beneficiaries). The arrangement would compute as snare, not tangled_rope. If future victims can bear standing as a consequentialist beneficiary set, the reading''s structure holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_victim_identity_status, conceptual, 'Whether future hypothetical victims constitute a coherent beneficiary constituency under the reading''s framework.').

omega_variable(
    suppression_internalization_interpersonal,
    'To what extent is the suppression measured in this reading — the structural silencing of abolitionist objections and victim-family preferences — internalized by condemned persons and their communities, versus externally maintained by state force?',
    'Post-execution trajectory analysis: do condemned persons, their families, and abolitionist movements continue to resist after legal exhaustion (indicating internalized suppression failure), or does resistance cease once state authority is invoked (indicating structural suppression holds)? Ethnographic and testimonial evidence from death-row prisoners and abolitionist organizers.',
    'If suppression is substantially internalized (condemned persons and their communities believe in the legitimacy of the arrangement even after exhausting legal recourse), the constraint''s effective suppression is lower than the structural measure; the arrangement operates more as accepted coordination. If suppression is entirely structural (resistance persists, consent is never achieved), the effective suppression is higher; the arrangement is more extractive and requires continuous force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal, empirical, 'The mechanism of suppression: structural (external force) or internalized (belief in legitimacy).').

omega_variable(
    kernel_reading_contest__deterrence_vs_retributive,
    'Is the deterrence reading''s core premise (capital punishment justified by crime prevention efficacy) logically compatible with the retributive reading''s core premise (capital punishment justified by proportional desert for murder), or do they foreclose each other?',
    'Philosophical analysis of whether a single state authority can simultaneously ground capital punishment in both deterrence AND desert doctrine, or whether adopting one creates structural pressure against the other. Case study: jurisdictions that formally justify executions using both grounds in capital sentencing statutes.',
    'If the readings foreclose each other (adopting deterrence logically rules out retributive justification in the same framework), the relation is ''forecloses''. If both readings coexist in different jurisdictions or judicial philosophies without logical necessity that one rule out the other, the relation is ''coexists_with''. If deterrence grounds shift the legitimacy burden away from desert (making desert harder to defend without consequentialist supplement), the relation is ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__deterrence_vs_retributive, conceptual, 'The logical relationship between deterrence and retributive grounds for capital punishment: do they coexist, influence each other, or foreclose?').

omega_variable(
    kernel_reading_contest__deterrence_vs_abolition,
    'Does the categorical abolitionist reading''s core premise (life is inalienable; state killing is inherently impermissible) logically foreclose the deterrence reading''s core premise (capital punishment justified if it prevents future murders), or do they merely represent different value orderings that different parties can hold simultaneously?',
    'Philosophical analysis of whether ''life is inalienable'' entails ''capital punishment is always impermissible'' or whether it is compatible with instrumental exceptions under consequentialist logic. Examination of legal and philosophical frameworks that attempt to hold both premises (e.g., strict inalienability with torture exceptions, or life-rights with emergency override clauses).',
    'If inalienability entails absolute prohibition, the readings foreclose each other — adopting abolition rules out the deterrence ground. If inalienability is compatible with consequentialist override (e.g., killing in self-defense as an exception to the rule), the readings coexist but with friction. The outcome determines the cs_structure.reading_relations entry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest__deterrence_vs_abolition, conceptual, 'Whether categorical abolition and deterrence justify foreclose or coexist in any coherent framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 1972, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1972, state_killing_authority__deterrence_instrument, theater_ratio, 1972, 0.32).
narrative_ontology:measurement(stat_tr_t1985, state_killing_authority__deterrence_instrument, theater_ratio, 1985, 0.36).
narrative_ontology:measurement(stat_tr_t1995, state_killing_authority__deterrence_instrument, theater_ratio, 1995, 0.39).
narrative_ontology:measurement(stat_tr_t2005, state_killing_authority__deterrence_instrument, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(stat_tr_t2015, state_killing_authority__deterrence_instrument, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__deterrence_instrument, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(stat_be_t1972, state_killing_authority__deterrence_instrument, base_extractiveness, 1972, 0.55).
narrative_ontology:measurement(stat_be_t1985, state_killing_authority__deterrence_instrument, base_extractiveness, 1985, 0.61).
narrative_ontology:measurement(stat_be_t1995, state_killing_authority__deterrence_instrument, base_extractiveness, 1995, 0.64).
narrative_ontology:measurement(stat_be_t2005, state_killing_authority__deterrence_instrument, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(stat_be_t2015, state_killing_authority__deterrence_instrument, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__deterrence_instrument, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1972, state_killing_authority__deterrence_instrument, suppression_requirement, 1972, 0.58).
narrative_ontology:measurement(stat_su_t1985, state_killing_authority__deterrence_instrument, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement(stat_su_t1995, state_killing_authority__deterrence_instrument, suppression_requirement, 1995, 0.67).
narrative_ontology:measurement(stat_su_t2005, state_killing_authority__deterrence_instrument, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(stat_su_t2015, state_killing_authority__deterrence_instrument, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__deterrence_instrument, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__deterrence_instrument, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings (retributive_desert, categorical_abolition) are three readings of the contested kernel state_killing_authority. Each reading authores a different ε value, beneficiary/victim structure, and type classification of the SAME standing arrangement (state execution of murderers). They are linked via network.affects_constraints because the empirical claim of each reading (deterrence prevents crime; desert is proportional; life is inalienable) can refute the others' empirical premises. Decomposition is justified by ε-invariance: the three readings measure the same observable (whether state kills murderers) but via different valence frames (consequentialist efficacy, retributive principle, rights doctrine). Each reading instantiates a different constraint; the kernel frame routes the committer structure through omega variables and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
