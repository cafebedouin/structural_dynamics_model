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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Capital Punishment as Categorical Dignity Violation (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the
 *   state-killing-legitimacy kernel: the claim that state killing
 *   categorically violates human dignity regardless of desert (the
 *   retributive reading) or utility (the deterrence reading). Under this
 *   reading, the condemned person is a rights-bearer whose categorical
 *   dignity claim cannot be discharged by any showing of proportionality or
 *   crime-prevention benefit, and the state's retained killing power is
 *   itself the object under indictment — not merely its misuse. Since 1976
 *   (the U.S. resumption of executions post-Gregg v. Georgia, used here as
 *   the interval anchor), the theater component has risen as procedural
 *   safeguards (extended appeals, clemency review, execution protocol
 *   litigation) have proliferated without altering the categorical objection
 *   this reading presses, while enforcement (continued executions,
 *   death-qualified juries, habeas restriction) has intensified. This is one
 *   of three linked readings of the same kernel; the retributive and
 *   deterrence readings are separate constraint files with their own ε and
 *   structure, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Capital Punishment as Categorical Dignity Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '2aac4768-9a45-4b8e-b872-105c43778de4').
narrative_ontology:cs_kernel_codification('2aac4768-9a45-4b8e-b872-105c43778de4', distributed).
narrative_ontology:cs_authority_grounding('2aac4768-9a45-4b8e-b872-105c43778de4', distributed).
narrative_ontology:cs_reading_relation('2aac4768-9a45-4b8e-b872-105c43778de4', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('2aac4768-9a45-4b8e-b872-105c43778de4', state_killing_legitimacy__deterrence_reading, influences).
narrative_ontology:cs_axiom('2aac4768-9a45-4b8e-b872-105c43778de4', foundational, dignity_bars_killing_regardless_of_desert).
narrative_ontology:cs_axiom_status(dignity_bars_killing_regardless_of_desert, holdable).
narrative_ontology:cs_axiom_grounding('2aac4768-9a45-4b8e-b872-105c43778de4', dignity_bars_killing_regardless_of_desert, deontological).
narrative_ontology:cs_axiom('2aac4768-9a45-4b8e-b872-105c43778de4', foundational, utility_cannot_license_categorical_rights_violation).
narrative_ontology:cs_axiom_status(utility_cannot_license_categorical_rights_violation, holdable).
narrative_ontology:cs_axiom_grounding('2aac4768-9a45-4b8e-b872-105c43778de4', utility_cannot_license_categorical_rights_violation, deontological).
narrative_ontology:cs_reference_frame('2aac4768-9a45-4b8e-b872-105c43778de4', post_gregg_regulated_execution_regime).
narrative_ontology:cs_drift_state('2aac4768-9a45-4b8e-b872-105c43778de4', contemporary_abolitionist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2aac4768-9a45-4b8e-b872-105c43778de4', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, political_actors_campaigning_on_severity).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, death_row_condemned).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, wrongfully_convicted_executed).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, families_of_the_condemned).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, categorical_human_dignity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held under sentence of death, subject to a legal process that terminates in the state ending their life. Has no exit from the sentence except clemency, appellate reversal, or death by other means; the arrangement extracts the totality of the person's remaining life and future, irrespective of the underlying facts of guilt or the passage of time.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, death_row_condemned, payer,
    powerless, immediate, trapped, national).

% Persons executed under judgments later shown, or credibly arguable, to have been factually mistaken. Their harm is irreversible and cannot be corrected by later exoneration; they represent the limit case the abolitionist reading treats as decisive evidence that no institutional safeguard can render the practice legitimate.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, wrongfully_convicted_executed, payer,
    powerless, civilizational, trapped, national).

% Bear the ongoing loss and stigma attached to a relative's execution, with no standing to contest the sentence once appeals are exhausted. Their relationship to the constraint is entirely passive; they absorb consequences of a decision in which they had no voice.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, families_of_the_condemned, payer,
    powerless, generational, trapped, national).

% Charges capital cases, negotiates plea deals under the shadow of the death penalty, and administers the execution apparatus. Derives prosecutorial leverage (plea bargaining power, career advancement tied to conviction and sentencing outcomes) from the state's retained killing power, independent of whether any given execution occurs.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__abolition_reading, state_prosecutorial_apparatus, beneficiary).

% Use support for capital punishment as an electoral signal of toughness on crime. Collect political capital from the state's retention of the power to kill, without personally administering or bearing any cost of its exercise; can shift positions with electoral winds without structural consequence.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, political_actors_campaigning_on_severity, beneficiary,
    powerful, biographical, mobile, national).

% Survivors of homicide victims who may seek execution as closure or vindication. The abolitionist reading treats their felt need for finality as real but categorically insufficient to license the state's exercise of the killing power; they are structurally present in the sentencing process but their preference is not treated as dispositive under this reading.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, victims_families_seeking_closure, excluded,
    moderate, biographical, constrained, regional).

% Adjudicate Eighth Amendment and comparable dignity-based challenges to capital punishment across jurisdictions. Their doctrine can narrow, expand, or abolish the practice; they observe the operation of the constraint from a position that can alter its legal status but does not itself bear the extraction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination problem this reading recognizes the death penalty as solving. Any purported coordination function (deterrence, retribution, closure) is treated as insufficient to override the categorical dignity bar; the arrangement is read as extraction dressed in the vocabulary of justice.
% TRANSFER_FUNCTION: Moves the totality of a condemned person's remaining life, and any possibility of correction for error, to the state's exercise of penal authority — while generating diffuse political and institutional benefit (electoral capital, prosecutorial leverage) for actors who bear none of the risk of the transfer's irreversibility.
% ABSENT_VOICES: The condemned themselves have no standing to contest the categorical legitimacy of the practice once due process is formally satisfied — only its procedural application. International human rights bodies and comparative-law abolitionist scholarship are structurally outside domestic sentencing proceedings even where their arguments bear directly on the dignity claim.
% DISAPPEARANCE_RATIONALE: If capital punishment were abolished overnight, plea bargaining leverage in capital-eligible cases would restructure around lesser maximum sentences, a category of irreversible wrongful-conviction harm would be foreclosed going forward, and political actors would lose a specific severity signal — sentencing regimes, appellate dockets, and clemency processes would all reorganize around a life-imprisonment ceiling.
% FOUNDING_PROBLEM: The historical justification offered for state killing power was the need to punish the gravest crimes proportionately and/or deter future homicide through the ultimate sanction.
% FOUNDING_PROBLEM_CORROBORATION: Prosecutorial and political beneficiaries attest the founding problem (grave-crime punishment, deterrence) remains live. Exoneration-project data, meta-analyses finding no reliable deterrent effect over life-without-parole, and international human rights bodies outside the prosecuting jurisdictions attest that the founding problem is either unaddressed by the practice or better addressed without it — this reading treats that outside corroboration as decisive against the practice's continued legitimacy regardless of the founding problem's status.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.88, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (ε=0.88) is authored high because the reading treats the harm as categorical and irreversible — no institutional safeguard, however elaborate, can discharge it, and the wrongful-conviction cases are treated as decisive rather than anomalous. Suppression (0.72) reflects the active legal and political infrastructure required to sustain executions against sustained constitutional and international challenge — death-qualified jury selection, habeas restrictions, and execution-method litigation are all forms of active maintenance. Accessibility collapse is authored low-moderate (0.35) because life imprisonment is a readily available, functioning alternative already used in the large majority of homicide cases and in most abolitionist jurisdictions — this is not a mountain-grade collapse of alternatives. Resistance is high (0.7): the practice meets sustained organized opposition (innocence projects, defense bar, international bodies, some prosecutors) rather than passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned, the wrongfully executed, and their families sit at the full-target end of directionality: trapped exit, powerless position, and the extraction (loss of life or its irreversible risk) is total and non-recoverable. The state prosecutorial apparatus and severity-signaling political actors sit near the beneficiary end: they derive leverage or political capital from the state's retention of the killing power without bearing its risk. Victims'-families-seeking-closure are treated as excluded rather than beneficiary or payer under this reading specifically because the abolitionist framework denies that their preference for closure can legitimate the practice — their interest is real but, on this reading, not the kind of interest that can license categorical rights violation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proportionate punishment / deterrence of the gravest crimes) is treated by this reading as either never adequately solved by execution or now solved as well or better by life imprisonment without parole — making the persistence of the killing power an instance of mandatrophy: an arrangement whose original justificatory problem has been superseded by an available substitute, but which persists because concentrated political and institutional benefit is not offset by any comparably organized cost-bearer on the beneficiary side. This is precisely why the constraint is authored as snare rather than tangled_rope: the reading finds no genuine coordination function surviving scrutiny, only extraction wearing coordination's vocabulary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_scalar_dignity_claim,
    'Is human dignity a categorical bar to state killing (admitting no exceptions regardless of desert or utility), or a scalar value that can in principle be outweighed by sufficiently strong desert or deterrence claims?',
    'This is not empirically resolvable; it is the foundational normative disagreement dividing this reading from the retributive and deterrence readings. Resolution would require philosophical argument establishing or rejecting the categorical/scalar distinction for dignity claims, not new data.',
    'If dignity is scalar rather than categorical, this reading collapses into a strong-but-defeasible presumption against execution, structurally converging toward the retributive/deterrence readings'' framework (where sufficiently grave desert or sufficiently robust deterrence evidence could in principle license the practice) rather than remaining a distinct categorical-bar reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_scalar_dignity_claim, conceptual, 'Whether the dignity claim underlying this reading is categorical or defeasible-scalar.').

omega_variable(
    wrongful_conviction_rate_as_decisive_evidence,
    'Does the empirically documented rate of wrongful capital convictions (via exoneration data, DNA-based innocence findings) function as decisive evidence against the legitimacy of the practice, or as evidence only of a fixable procedural defect?',
    'Systematic tracking of exoneration rates and post-execution innocence claims across jurisdictions; comparison of error rates in capital vs. non-capital homicide prosecutions.',
    'If error rates are shown to be structurally irreducible (an inherent feature of any adversarial capital process) rather than incidentally fixable, that strengthens the categorical reading; if shown to be a contingent, correctable defect, the retributive/deterrence readings could accommodate the same evidence via procedural reform without conceding the categorical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_rate_as_decisive_evidence, empirical, 'Whether wrongful-conviction evidence supports categorical abolition or only procedural reform.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the three sibling readings of state_killing_legitimacy locate their disagreement — in the empirical premises (does execution deter; is error rate acceptable), or in the foundational normative premise (can any desert or utility claim ever license state killing)?',
    'Structural analysis of each reading''s axioms: this reading''s axioms are normative and largely insulated from empirical deterrence findings; the deterrence reading''s axioms are empirically contingent; the retributive reading''s axioms are deontological but oriented around desert rather than dignity.',
    'If the disagreement is purely empirical (deterrence_reading), new deterrence data could in principle resolve the kernel dispute. If foundational-normative (this reading vs. retributive_reading), no amount of empirical evidence resolves it — the three readings remain permanently coexisting positions held by different commitment frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Locating where the three kernel readings actually diverge — empirical premise vs. foundational normative premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_legitimacy__abolition_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(stat_tr_t1985, state_killing_legitimacy__abolition_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(stat_tr_t1995, state_killing_legitimacy__abolition_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(stat_tr_t2005, state_killing_legitimacy__abolition_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(stat_tr_t2015, state_killing_legitimacy__abolition_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__abolition_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_legitimacy__abolition_reading, base_extractiveness, 1976, 0.72).
narrative_ontology:measurement(stat_be_t1985, state_killing_legitimacy__abolition_reading, base_extractiveness, 1985, 0.76).
narrative_ontology:measurement(stat_be_t1995, state_killing_legitimacy__abolition_reading, base_extractiveness, 1995, 0.82).
narrative_ontology:measurement(stat_be_t2005, state_killing_legitimacy__abolition_reading, base_extractiveness, 2005, 0.85).
narrative_ontology:measurement(stat_be_t2015, state_killing_legitimacy__abolition_reading, base_extractiveness, 2015, 0.87).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__abolition_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_legitimacy__abolition_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(stat_su_t1985, state_killing_legitimacy__abolition_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(stat_su_t1995, state_killing_legitimacy__abolition_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(stat_su_t2005, state_killing_legitimacy__abolition_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(stat_su_t2015, state_killing_legitimacy__abolition_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__abolition_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, retributive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the state_killing_legitimacy kernel. abolition_reading, deterrence_reading, and retributive_reading each author their own ε and stakeholder structure from the same underlying contested practice (state execution). They are not the same constraint measured three ways; the ε-invariance principle requires each to be a separate file. This file's high ε (0.88) reflects the categorical-violation premise; the sibling files' ε values reflect their own distinct normative premises (empirical desert-forfeiture, empirical deterrence-efficacy) and should not be expected to match.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
