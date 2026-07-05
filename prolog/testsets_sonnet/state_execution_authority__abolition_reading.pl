% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: State Execution Authority — Abolition Reading (Categorical Impermissibility)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the abolition reading of the
 *   state_execution_authority kernel: the claim that state killing of a
 *   convicted person is categorically impermissible, independent of crime
 *   severity or the elaborateness of procedural safeguards. On this reading
 *   every executed person — including the factually guilty — enters the
 *   victim set, because the categorical premise denies that guilt licenses
 *   lethal punishment once the state has already achieved incapacitation
 *   through incarceration. No legitimate beneficiary group exists:
 *   retribution and deterrence are rejected as justifications that could
 *   redeem the practice, so there is no coordination function to weigh
 *   against the harm. The sibling readings (retributive_reading,
 *   deterrence_reading) are separate constraint stories with their own ε and
 *   beneficiary/victim structures — they are not described further here per
 *   the ε-invariance and decomposition rules; this file evaluates only the
 *   abolitionist claim as its own structurally distinct constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.93).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority — Abolition Reading (Categorical Impermissibility)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '190d5737-7a51-4ba1-8b5d-37a3536cac6c').
narrative_ontology:cs_kernel_codification('190d5737-7a51-4ba1-8b5d-37a3536cac6c', distributed).
narrative_ontology:cs_authority_grounding('190d5737-7a51-4ba1-8b5d-37a3536cac6c', distributed).
narrative_ontology:cs_reading_relation('190d5737-7a51-4ba1-8b5d-37a3536cac6c', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('190d5737-7a51-4ba1-8b5d-37a3536cac6c', state_execution_authority__deterrence_reading, influences).
narrative_ontology:cs_axiom('190d5737-7a51-4ba1-8b5d-37a3536cac6c', foundational, state_killing_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_killing_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('190d5737-7a51-4ba1-8b5d-37a3536cac6c', state_killing_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('190d5737-7a51-4ba1-8b5d-37a3536cac6c', secondary, wrongful_execution_proves_systemic_illegitimacy).
narrative_ontology:cs_axiom_status(wrongful_execution_proves_systemic_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('190d5737-7a51-4ba1-8b5d-37a3536cac6c', wrongful_execution_proves_systemic_illegitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('190d5737-7a51-4ba1-8b5d-37a3536cac6c', categorical_prohibition_on_state_killing).
narrative_ontology:cs_drift_state('190d5737-7a51-4ba1-8b5d-37a3536cac6c', post_dna_exoneration_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('190d5737-7a51-4ba1-8b5d-37a3536cac6c', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons_guilty).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted_executed).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, death_row_populations).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_the_condemned).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, human_dignity_is_inalienable).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, state_violence_monopoly_has_limits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals convicted of capital crimes who are put to death by the state. On this reading, guilt does not remove them from the victim set: the categorical claim holds that no crime severity licenses the state to kill a person already rendered harmless by incarceration. They have no exit once sentencing concludes; the arrangement terminates their capacity to be a party to anything further.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons_guilty, payer,
    powerless, immediate, trapped, national).

% People executed for crimes they did not commit, discovered post-execution or never discovered at all. Their deaths are irreversible in a system that has documented exoneration rates among death-row populations; no procedural safeguard, however elaborate, can undo an execution once carried out. This reading treats each documented or suspected wrongful execution as direct empirical proof the entire authority is illegitimate, not as an unfortunate error rate to be minimized.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_executed, payer,
    powerless, immediate, trapped, national).

% Currently condemned individuals awaiting execution, living under the constraint's active threat for years or decades. Their exit options are appeals, clemency petitions, and litigation — all mediated by the same state apparatus that holds the execution power. They cannot exit the jurisdiction or the sentence structure once condemned.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_row_populations, payer,
    powerless, biographical, trapped, national).

% Relatives who bear the ongoing harm of watching a family member held under sentence of death, and the permanent harm if the execution proceeds. They have no standing to halt the process and often exhaust resources on appeals; their exit is bounded by the same legal channels available to the condemned.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_the_condemned, payer,
    powerless, generational, trapped, national).

% The prosecutorial, judicial, and correctional infrastructure that sentences, upholds, and carries out executions. On this reading it administers a categorically illegitimate power regardless of how carefully it is proceduralized; it can revise its own procedures (add safeguards, narrow eligibility) but this reading holds that no revision cures the underlying claim, so the apparatus's continued operation is itself the extraction — it maintains a power it should not hold at all.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Survivors and family members of murder victims, who are frequently invoked by retributive and deterrence framings as the constraint's intended beneficiaries but who, on the abolition reading, do not legitimately benefit from execution as a moral matter — execution does not restore what was lost and this reading denies it should be treated as compensation. Their grief and demand for accountability are real but are read here as directed toward a punishment mechanism (incarceration, restitution processes) other than execution; they are largely excluded from the abolitionist framework's own deliberation about what execution accomplishes.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, victims_of_the_underlying_crime, excluded,
    powerless, biographical, trapped, local).

% Treaty bodies, courts, and monitoring organizations that document execution practices, exoneration data, and procedural failures across jurisdictions, and that generally hold the abolitionist position as the emerging international norm. They have no direct enforcement power over any single state's practice but shape legitimacy discourse and diplomatic pressure.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized on this reading. Where retributive and deterrence readings claim execution coordinates a collective demand for proportionate justice or a public-safety deterrent effect, the abolition reading holds there is no coordination problem that execution — as opposed to permanent incarceration — actually solves; whatever public-safety or moral-closure function exists is achievable by non-lethal means.
% TRANSFER_FUNCTION: The arrangement transfers life itself from the condemned (guilty and wrongfully convicted alike) to the state's exercise of authority, with no compensating flow back to any legitimate party — no restored victim, no measurably deterred future crime attributable to execution specifically versus incarceration.
% ABSENT_VOICES: Wrongfully convicted persons who were executed before exoneration evidence emerged cannot testify to the apparatus's error rate. Victims'-rights advocates who reject execution as inadequate or beside the point of their own healing are marginalized within a public discourse that frames execution as inherently victim-serving.
% DISAPPEARANCE_RATIONALE: If state execution authority disappeared overnight, capital sentencing dockets would convert to maximum-incarceration outcomes, death-row populations would be resentenced, and the entire evidentiary and procedural machinery built around capital cases (bifurcated trials, special appellate tracks, execution protocols) would be dismantled or repurposed — a substantial institutional rearrangement, not a null change.
% FOUNDING_PROBLEM: State execution was historically built to provide a maximal, final sanction for the most severe crimes, understood as necessary for public order, retribution, and deterrence where lesser punishments were seen as insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Documented wrongful-execution and exoneration research from independent legal-innocence projects (outside both the prosecutorial apparatus and abolitionist advocacy groups), international human-rights treaty bodies, and comparative-jurisdiction studies showing no consistent deterrent effect superior to life imprisonment collectively attest that the practical problem execution was built to solve is not actually solved by execution specifically, and that the risk of irreversible error is empirically substantial. This corroboration comes from evidentiary and comparative research bodies, not from abolitionist advocacy organizations themselves.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.93, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near the ceiling (0.93-0.97) because the reading holds the harm is irreversible, categorical, and admits no substitution — unlike a fine or a prison term, an execution cannot be partially refunded, appealed post-fact, or corrected if new evidence emerges. Suppression is high and rising (0.70 to 0.88) because maintaining a capital sentencing and execution apparatus in the face of a strengthening abolitionist consensus, mounting exoneration data, and international pressure requires increasingly active procedural, political, and rhetorical defense. Theater ratio is moderate and rising (0.20 to 0.42): a growing share of the apparatus's activity (extended appellate processes, execution-protocol litigation, clemency review theater) functions to perform procedural legitimacy rather than to change the categorical outcome, on this reading's own terms. Accessibility collapse is authored moderate-low (0.35) rather than high, because the abolitionist reading holds that alternatives (life imprisonment) are not just theoretically available but already the norm in most jurisdictions worldwide — the collapse is partial and jurisdiction-specific, not global or complete. Resistance is high (0.78) reflecting substantial organized opposition (legal-innocence projects, treaty bodies, abolitionist litigation) actively contesting the constraint's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The apparatus experiences its own operation as bounded, safeguarded, and procedurally legitimate (extensive appeals, bifurcated trials, clemency review). The condemned and their families experience the identical structure as a categorical, irreversible threat that no amount of procedure converts into legitimacy. The engine should compute a sharp seat divergence here: an institutional agenda-setter seat with civilizational time horizon and analytical distance from the individual case, versus powerless, trapped, immediate-horizon payer seats for whom the constraint is terminal.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution apparatus is the agenda_setter with arbitrage-level exit (it can revise procedures, narrow eligibility criteria, or suspend executions administratively) — but per this reading, no such revision cures the underlying illegitimacy, so its structural position remains extractive regardless of procedural refinement. All executed and condemned persons are payers with trapped exit; there is no beneficiary seat authored at all, which is the central structural delta of this reading relative to its siblings — retributive and deterrence readings would name victims'-rights groups or the public as beneficiaries, but this reading denies that execution confers a legitimate benefit on anyone, so no beneficiaries array entry is authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a maximal sanction for the gravest crimes) is marked dead on this reading because comparative-jurisdiction and deterrence research corroborate that the practical function is not uniquely served by execution versus life imprisonment, while the practice persists via institutional inertia, political symbolism, and residual retributive sentiment. This is precisely the mismatch the R5 genealogy interview flags: founding_problem_status=dead paired with disappearance_verdict=world_rearranges signals a capture/zombie pattern rather than a resolved coordination function — the apparatus persists past the death of its stated justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_calibrated_wrongness,
    'Is the wrongness of state execution categorical (any execution, however procedurally safeguarded, is impermissible) or calibrated (wrongness scales with error rate, arbitrariness, or procedural quality, such that a sufficiently safeguarded system could be permissible)?',
    'This is not resolvable by further procedural data — it is a normative-framework question about whether the deontological premise (state may never take the life of an already-incapacitated person) can be defeated by empirical improvement in accuracy. Legal philosophy and constitutional doctrine remain split; no empirical study of error rates can settle a categorical claim.',
    'If categorical, ε remains maximal and stable regardless of any procedural reform data. If calibrated, the constraint''s classification should track empirical error-rate and arbitrariness measurements over time and could in principle be resolved toward a heavily safeguarded retributive_reading rather than staying a distinct abolition claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_calibrated_wrongness, preference, 'Whether abolitionist wrongness is categorical or could be defeated by procedural perfection.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the abolition, retributive, and deterrence readings genuinely incommensurable framings of the same underlying kernel (state execution authority), or does one reading''s empirical claims (e.g., deterrence_reading''s causal claim about crime prevention) actually settle the dispute if verified?',
    'If deterrence_reading''s empirical causal claim (execution measurably reduces capital crime beyond incarceration alone) were robustly confirmed, it would not by itself refute the abolition reading''s deontological premise (a deterrent effect does not license taking a life), but it would remove one plank of the abolitionist''s supporting case (that execution serves no distinguishable social function). Track deterrence research findings as a partial, non-dispositive input.',
    'Empirical resolution of the deterrence question would not force reclassification of THIS reading, because the abolition reading''s foundational axiom is deontological, not consequentialist — but it would affect the relative political weight of the sibling readings in the broader kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether empirical resolution of sibling claims can settle this reading''s categorical premise.').

omega_variable(
    victim_of_underlying_crime_exclusion,
    'Is it accurate to exclude victims of the underlying crime from any beneficiary role, given that some crime victims'' families explicitly support execution as a form of closure or justice they experience as real?',
    'Survey and qualitative research on crime-victim-family attitudes toward capital punishment across jurisdictions, distinguishing subjective reported closure from the abolitionist reading''s normative claim that such closure does not constitute legitimate justification.',
    'If a meaningful share of victims'' families experience genuine, durable benefit from an execution, the abolition reading''s zero-beneficiary structural claim understates the constraint''s perceived value to at least one real stakeholder group, even though the reading''s normative argument is that perceived benefit does not confer legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_of_underlying_crime_exclusion, empirical, 'Whether crime-victim families constitute an unacknowledged beneficiary group under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.97).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.96).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.95).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.935).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the state_execution_authority kernel. abolition_reading (this file) authors zero beneficiaries and a maximal, stable ε from categorical prohibition. retributive_reading and deterrence_reading are separate files with their own beneficiary sets (satisfied victims'-families / deterred-crime public respectively) and their own ε values, expected to be lower and more contested than this reading's. The three files are linked bidirectionally via affects_constraints because each reading's political and legal traction structurally affects the resourcing and legitimacy conditions of the others — a jurisdiction's move toward one reading changes the argumentative burden on the remaining two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
