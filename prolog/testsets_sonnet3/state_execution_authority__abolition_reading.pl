% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority — Abolitionist Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the abolitionist reading of a single
 *   contested kernel: the state's claimed authority to execute condemned
 *   persons. Under this reading, the practice is categorically impermissible
 *   independent of crime severity, procedural rigor, or public support for
 *   any specific execution. The reading treats every executed person — guilty
 *   or innocent — as a victim of an illegitimate exercise of state power,
 *   because the irreversibility of death forecloses the correction available
 *   to every other criminal sanction. This is NOT a story about whether
 *   capital punishment deters crime or achieves proportionate retribution;
 *   those are the deterrence_reading and retributive_reading, authored as
 *   separate constraints with their own ε values, beneficiary structures, and
 *   classifications. Rising theater_ratio and suppression_requirement across
 *   the measurement interval reflect the increasing procedural apparatus
 *   (extended appeals, heightened scrutiny, execution-protocol litigation)
 *   built around a practice whose legitimacy is, under this reading,
 *   foreclosed from the start — procedural elaboration functions here as
 *   legitimating theater layered onto an act the reading holds cannot be
 *   legitimated by any procedure.
 *
 * KEY AGENTS:
 *   - executed_persons: primary target (powerless/trapped) — bears the irreversible harm the reading is organized around
 *   - wrongfully_convicted_persons: evidentiary anchor (powerless/trapped) — the population whose existence the reading treats as dispositive proof of systemic illegitimacy
 *   - state_prosecutorial_apparatus: agenda-setter (institutional/arbitrage) — administers the practice and could unilaterally decline capital charging
 *   - marginalized_defendant_populations: disparate-impact target (powerless/trapped) — bears documented sentencing disparity
 *   - abolitionist_legal_advocates: analytical observer (organized/analytical) — primary institutional carrier of this reading, contests from outside the apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.93).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.78).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.93).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority — Abolitionist Reading").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'd8c038f0-8bce-4a3e-8da8-f7c0ae8678b1').
narrative_ontology:cs_kernel_codification('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', distributed).
narrative_ontology:cs_authority_grounding('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', distributed).
narrative_ontology:cs_reading_relation('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', foundational, irreversibility_bars_categorical_state_punishment).
narrative_ontology:cs_axiom_status(irreversibility_bars_categorical_state_punishment, holdable).
narrative_ontology:cs_axiom_grounding('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', irreversibility_bars_categorical_state_punishment, deontological).
narrative_ontology:cs_reference_frame('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', abolitionist_deontological_prohibition).
narrative_ontology:cs_drift_state('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', contemporary_death_penalty_jurisprudence, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('d8c038f0-8bce-4a3e-8da8-f7c0ae8678b1', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, capital_defendants).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_the_condemned).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, marginalized_defendant_populations).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, state_monopoly_on_lethal_force_requires_absolute_limits).
narrative_ontology:constraint_vindicates(state_execution_authority__abolition_reading, irreversibility_bars_categorical_punishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Condemned by the state and put to death; from the abolitionist reading, this is true of every executed person regardless of factual guilt — the categorical prohibition treats guilt as irrelevant to the wrongness of the act itself. They have no exit: appeals exhaust, clemency is discretionary and rare, and the sentence is irreversible once carried out.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, national).

% A documented subset of the executed population later shown (or credibly suspected) to have been innocent. Procedural safeguards — appeals, DNA testing, post-conviction review — systematically fail to catch every wrongful conviction before execution. Once executed, no remedy exists; this population is the abolitionist reading's central evidentiary anchor for systemic illegitimacy.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_persons, payer,
    powerless, immediate, trapped, national).

% Face capital charges under a system that permits execution as an outcome. Even those ultimately not executed live under threat of an irreversible sentence for years, often decades, on death row awaiting appeals. Their bargaining position in plea negotiations is shaped entirely by the existence of the death penalty as a threat.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, capital_defendants, payer,
    powerless, biographical, trapped, national).

% Bear the secondary harm of a family member's execution — social stigma, grief without resolution, and in wrongful-conviction cases, decades-long fights for posthumous exoneration that the state has no obligation to pursue once the sentence is carried out.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_the_condemned, payer,
    powerless, generational, trapped, national).

% Racial and socioeconomic minorities are documented to receive capital sentences at disproportionate rates relative to case severity, and to have less access to adequate capital defense. The categorical prohibition treats this disparity as compounding evidence that no procedural safeguard can render the practice legitimate, since the safeguards themselves are unevenly applied.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, marginalized_defendant_populations, payer,
    powerless, generational, trapped, national).

% Sets capital charging policy, seeks death sentences, and administers the execution process through corrections agencies. Under the abolitionist reading, this apparatus is not a legitimate beneficiary but the instrument of an impermissible act — it holds the power to end the practice unilaterally (by declining to seek execution) but characteristically does not, because doing so would concede the illegitimacy of past executions.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Families of murder victims who experience the death penalty as a source of closure or justice. The abolitionist reading does not treat this experience as illegitimate but holds that it cannot ground a categorical exception — their preference is heard in sentencing and victim-impact proceedings but is structurally overridden by the abolitionist premise that no outcome, however desired, licenses state killing.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, victims_families_seeking_closure, excluded,
    powerless, biographical, constrained, local).

% Litigate against capital sentences, document wrongful convictions, and press for categorical abolition. They are the primary institutional carriers of this reading, but their advocacy does not itself administer the constraint — they observe and contest it from outside the apparatus that enforces it.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_legal_advocates, observer,
    organized, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading. The retributive and deterrence readings claim execution coordinates proportionate punishment or crime prevention; the abolitionist reading holds that no genuine coordination problem is solved by execution that could not be solved by life imprisonment without the irreversibility risk.
% TRANSFER_FUNCTION: Moves life itself — irreversibly — from the condemned to the state's exercise of authority. No compensating flow returns to the condemned or, under this reading, to society: any deterrent or retributive benefit claimed by the sibling readings is treated here as illusory or unable to justify an irreversible harm.
% ABSENT_VOICES: Wrongfully executed persons cannot testify to their innocence post-execution; this is the paradigmatic absent voice the reading is built around. Victims'-families seeking closure are heard but structurally overridden. International human rights bodies and comparative-law abolitionist jurisdictions are cited as external corroboration but hold no binding authority within the domestic system.
% DISAPPEARANCE_RATIONALE: If the state's execution authority were abolished, capital sentencing would end nationwide, death rows would be commuted to life sentences, prosecutorial charging strategy would lose its most severe leverage point in plea bargaining, and the wrongful-execution risk would be eliminated entirely rather than merely reduced. This is a live, contested policy question with active legislative and judicial movement in multiple jurisdictions, not a stable background fact.
% FOUNDING_PROBLEM: The kernel commitment (state authority to impose death as punishment) was originally built to solve retributive and deterrent problems: satisfying a societal demand for proportionate response to the gravest crimes and, historically, deterring capital offenses. The abolitionist reading denies both problems are actually solved by execution and asserts the practice generates a new problem — irreversible error — that no procedural safeguard can fully close.
% FOUNDING_PROBLEM_CORROBORATION: Empirical criminology on deterrence effects is genuinely contested among researchers outside both the abolitionist and retentionist advocacy communities — several peer-reviewed meta-analyses find no reliable deterrent effect, while others report mixed or inconclusive results. The Innocence Project and comparable post-conviction review organizations, operating independently of prosecutorial and abolitionist-advocacy funding structures, corroborate that wrongful capital convictions occur at a nontrivial and documented rate. No source outside the abolitionist movement itself corroborates the specific normative claim of categorical impermissibility; that premise is acknowledged here as a moral claim, not an empirically corroborated one.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored very high (0.93) because the abolitionist reading holds that no substitution exists for an irreversible harm — life imprisonment is a qualitatively different sanction, so any execution under this reading is total, uncompensated extraction from the condemned, without exception for guilt. Suppression is high (0.78) but not maximal: the practice persists through active prosecutorial choice and appellate exhaustion, not through totalizing control of all resistance — abolitionist advocacy, litigation, and legislative repeal remain live channels. Accessibility_collapse is authored moderate-low (0.35) precisely because this is a contested political and legal question with functioning alternative pathways (legislative abolition, judicial moratoria, gubernatorial commutation) rather than a foreclosed natural fact — the abolitionist reading argues for closure, but has not achieved it, and multiple jurisdictions have in fact abolished the practice through ordinary political process. Resistance is authored high (0.72): the death penalty is intensely and visibly contested, with organized advocacy on both sides, active litigation, and a documented multi-decade trend of jurisdictions abolishing or narrowing capital eligibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Every executed person, including the factually guilty, is placed in the victim set — this is the reading's signature structural move and distinguishes it sharply from the retributive and deterrence readings, which would place guilty executed persons outside any victim set (the retributive reading treats their execution as deserved; the deterrence reading treats it as instrumentally justified). No beneficiary group is authored: the reading rejects retribution and deterrence as legitimate justifications, so it declines to name any party as a legitimate beneficiary of the practice, even though it acknowledges (via the excluded stakeholder) that some victims'-families report subjective benefit. The state_prosecutorial_apparatus is authored as agenda_setter rather than beneficiary, because it administers and could halt the practice but collects no direct rent from doing so — this is a structural, not extractive, position, though its persistence in seeking capital sentences despite documented wrongful-conviction risk is precisely what the reading treats as illegitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem answer documents a genealogy where the original justification (retribution/deterrence) is contested on its own empirical and moral terms, and the practice's continuation is read by this reading as the arrangement persisting past any defensible mandate — mandatrophy in the strong sense. But the six_questions corroboration is careful to flag that the *categorical impermissibility* claim itself is a moral premise held by the abolitionist movement and its allied advocates, not an empirically corroborated external fact — unlike the wrongful-conviction rate, which independent post-conviction review organizations do corroborate. This keeps the reading's strongest empirical claim (documented wrongful executions) separate from its normative claim (that this makes ALL executions categorically impermissible), which is the actual site of contest with the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_case_specific_impermissibility,
    'Is the impermissibility of execution genuinely categorical (true of every case regardless of facts), or does the abolitionist position actually rest on case-specific empirical claims (error rates, disparate application) that could in principle be resolved by sufficiently rigorous procedure, collapsing this reading toward a stricter version of the retributive/deterrence readings rather than a truly independent kernel reading?',
    'Examine whether abolitionist legal advocacy, when offered a hypothetical zero-error, perfectly non-disparate execution regime, would still oppose it. If yes, the categorical claim is genuinely independent of procedural facts (a deontological axiom). If opposition weakens, the reading is partially empirical and closer to a demand for better safeguards than true abolition.',
    'If genuinely categorical, this reading''s extractiveness (0.93) is correctly authored as intrinsic to the practice itself, independent of any procedural measurement — the ε-invariance principle is satisfied. If actually contingent on error rates, the reading may be measuring a different, more empirically-grounded constraint and should be decomposed further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_case_specific_impermissibility, conceptual, 'Whether the abolitionist reading''s core claim is a deontological axiom independent of empirical safeguards or a strong empirical claim about irreducible error rates.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the abolition_reading''s core premise (execution is categorically impermissible) logically foreclose the retributive_reading and deterrence_reading within a single legal framework, or can a jurisdiction coherently hold elements of multiple readings simultaneously (e.g., retain capital eligibility for the deterrence rationale while conceding some retributive claims are overstated)?',
    'Comparative constitutional analysis: examine whether any jurisdiction''s legal framework has attempted to simultaneously endorse categorical impermissibility and case-specific retention, and whether that produced internal doctrinal contradiction requiring resolution by a supreme court or legislature.',
    'If foreclosure is genuine, cs_structure.reading_relations should mark ''forecloses'' rather than ''coexists_with'' for at least one sibling. This omega documents why ''forecloses'' was NOT chosen here despite the categorical language, since abolition and retention factually coexist across different jurisdictions and even within the same polity''s ongoing political contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the categorical language of the abolitionist premise entails logical foreclosure of sibling readings or merely political opposition to them.').

omega_variable(
    no_beneficiary_completeness,
    'Is it accurate that this reading identifies zero legitimate beneficiaries, or do certain institutional actors (private prison/execution-service contractors, certain prosecutorial career incentives) receive concrete material benefit from the practice''s continuation that the reading has not named?',
    'Audit of execution-related contracting (lethal injection drug procurement, execution facility staffing) and prosecutorial career-outcome data (conviction rates, electoral benefit from capital case wins) to determine whether a concrete, currently-unnamed beneficiary group exists.',
    'If a concrete material beneficiary is found, this constraint''s classification could shift from snare (diffuse extraction, no clear capturer) toward tangled_rope or a snare with a named gain_flow recipient, changing the receipt-surface analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(no_beneficiary_completeness, empirical, 'Whether the beneficiaries array is genuinely empty or omits a material institutional capturer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.87).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.89).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.9).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.93).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_execution_authority kernel, each authored as a structurally distinct constraint per the ε-invariance principle: abolition_reading (this file, ε=0.93, snare, no beneficiaries, all executed persons as victims), retributive_reading (separate file, expected lower ε, likely rope or tangled_rope, with a moral-balance beneficiary class), and deterrence_reading (separate file, ε contingent on empirical deterrence evidence, likely tangled_rope with public-safety beneficiaries and wrongfully-convicted victims). The three do not average into one ε — they are linked via affects_constraints because each reading's political success structurally pressures the others' legitimacy conditions and resource availability (e.g., high-profile wrongful-execution cases documented under this reading directly erode the evidentiary basis the deterrence_reading depends on).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
