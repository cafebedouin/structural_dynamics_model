% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority — Retributive Reading (Proportionate Desert)
 *   domain: criminal justice/political philosophy/constitutional law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the state_execution_authority
 *   kernel: the retributive reading, under which the state executes because
 *   proportionate desert for heinous crimes requires it and moral balance is
 *   thereby restored. Per the epsilon-referent rule, the extractiveness score
 *   below assesses the STANDING capital-punishment arrangement as the
 *   retributive reading itself sees it — the arrangement under contest — not
 *   the rights-respecting alternative any other reading would install. The
 *   reading's own lights make the offender's cost legitimate and the
 *   restoration genuine, yet the cost remains terminal, non-substitutable by
 *   imprisonment, and borne without exit; the wrongful-conviction class bears
 *   the arrangement's error in its purest form. KEY AGENTS (by structural
 *   relationship): victims_families_of_heinous_crimes — primary beneficiary
 *   (organized/trapped), holds the moral ledger the arrangement exists to
 *   discharge; executed_capital_offenders — primary target
 *   (powerless/trapped/immediate), pays terminally;
 *   wrongfully_convicted_defendants — error-bearing target
 *   (powerless/trapped), pays without having incurred the debt;
 *   capital_case_prosecutors — agenda-setter and secondary beneficiary
 *   (institutional/mobile), administers charging discretion and collects
 *   career capital; appellate_and_review_courts and capital_jurors —
 *   administering agenda-setters; retentionist_political_institutions —
 *   agenda-setter beneficiary supplying and drawing on expressive authority;
 *   categorical_opponents_of_execution — excluded voice;
 *   legal_scholars_of_punishment_theory — analytical observer. Family note:
 *   sibling readings (deterrence_reading, abolition_reading) are separate
 *   files linked via network.affects_constraints; their structures differ as
 *   documented in the dual-formulation note.
 *
 * KEY AGENTS:
 *   - victims_families_of_heinous_crimes: Primary beneficiary (organized/trapped) — holds the moral ledger; receives the restoration the arrangement promises
 *   - executed_capital_offenders: Primary target (powerless/trapped/immediate) — bears the terminal, non-substitutable cost
 *   - wrongfully_convicted_defendants: Error-bearing target (powerless/trapped) — bears irreversible cost without having incurred the debt
 *   - capital_case_prosecutors: Agenda-setter with secondary beneficiary position (institutional/mobile) — administers charging discretion, collects career capital
 *   - appellate_and_review_courts: Administering agenda-setter (institutional/generational) — writes the doctrines that set who may be executed
 *   - capital_jurors: Administering agenda-setter (moderate/immediate) — decides life or death from a pre-filtered panel
 *   - retentionist_political_institutions: Agenda-setter beneficiary (institutional/generational) — maintains statutes, signs warrants, grants clemency
 *   - categorical_opponents_of_execution: Excluded voice (organized/mobile) — objects from outside the deciding process
 *   - legal_scholars_of_punishment_theory: Analytical observer (analytical/global) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.78).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.74).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority — Retributive Reading (Proportionate Desert)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal justice/political philosophy/constitutional law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6').
narrative_ontology:cs_kernel_codification('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', fixed_text).
narrative_ontology:cs_authority_grounding('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', lineage).
narrative_ontology:cs_interpretation_layer_present('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6').
narrative_ontology:cs_reading_relation('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', foundational, proportionate_desert_requires_death).
narrative_ontology:cs_axiom_status(proportionate_desert_requires_death, holdable).
narrative_ontology:cs_axiom_grounding('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', proportionate_desert_requires_death, deontological).
narrative_ontology:cs_axiom('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', secondary, moral_restoration_non_substitutable).
narrative_ontology:cs_axiom_status(moral_restoration_non_substitutable, holdable).
narrative_ontology:cs_axiom_grounding('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', moral_restoration_non_substitutable, deontological).
narrative_ontology:cs_reference_frame('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', lex_talionis_proportional_desert).
narrative_ontology:cs_drift_state('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', contemporary_evolving_standards_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a4f97c67-ac04-4361-9cb4-92ef9bf7a6d6', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families_of_heinous_crimes).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, capital_case_prosecutors).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retentionist_political_institutions).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_capital_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted_defendants).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, lex_talionis_proportionality).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, just_deserts_theory).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_restoration_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lost kin to murder and were promised a public, final, proportionate answer in return. They testify at sentencing, witness executions, and organize politically for retention of the statutes. Nothing they choose removes them from the event that gave them standing in the process; their position is fixed by the crime itself.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families_of_heinous_crimes, beneficiary,
    organized, biographical, trapped, national).

% Convicted of capital crimes and, after final appeal, subjected to the state's ultimate sanction. There is no payment schedule, no substitution, and no purchase that discharges what the framework says they owe. Their account of the process effectively closes once the warrant is signed.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_capital_offenders, payer,
    powerless, immediate, trapped, national).

% Convicted of capital crimes they did not commit. Some are exonerated after decades; some after the sentence has been carried out. They bear the arrangement's error cost in its purest form, and their way out runs through luck — newly discovered evidence, volunteer lawyers — rather than through any right they hold.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted_defendants, payer,
    powerless, biographical, trapped, national).

% Decide whether to charge crimes capitally and what to offer in plea negotiations; the filing of a death notice reshapes every negotiation that follows. Successful capital prosecutions build reputations and careers. They can decline the death phase case by case, and can move to offices or practices that never seek it.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, capital_case_prosecutors, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, capital_case_prosecutors, beneficiary).

% Review proportionality, police the categorical lines around age and intellectual capacity, and write the doctrines that widen or narrow who may be executed. They cannot resign the question — every capital scheme eventually lands before them — but they reshape its reach decision by decision.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, appellate_and_review_courts, agenda_setter,
    institutional, generational, constrained, national).

% Weigh life against death after hearing victim impact and mitigation evidence. They arrive pre-filtered: anyone unwilling under any circumstances to consider the death option is removed during selection, so the panel that decides has already been stripped of categorical refusal. Their service ends with the verdict.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, capital_jurors, agenda_setter,
    moderate, immediate, constrained, local).

% Enact and maintain capital statutes, sign death warrants, and decide clemency petitions. They answer electorally for perceived leniency and ceremonially for firmness. Dismantling the arrangement would require spending the political capital the arrangement currently supplies them.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retentionist_political_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Campaign for repeal, litigate method challenges, and document exonerations. They hold a settled conclusion that the sentencing process never admits into evidence; their influence runs through legislatures, elections, and international bodies rather than through the courtroom that decides each individual case.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, categorical_opponents_of_execution, excluded,
    organized, generational, mobile, global).

% Map what the practice presupposes, compare punishment systems across jurisdictions, and trace the lineage from talionic proportionality to modern capital statutes. They collect no outcome and bear none of the costs; their analyses circulate among all the other seats.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, legal_scholars_of_punishment_theory, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, victims_families_of_heinous_crimes).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the community's demand for a proportionate response to heinous killing into a single state-administered procedure, replacing private vengeance and retaliatory cycles with a public, bounded scale of desert administered through trial and appeal.
% TRANSFER_FUNCTION: Moves the convicted offender's life — and, through error, the occasional innocent life — to satisfy the moral ledger held by victims' families and the community; moves expressive authority and electoral positioning to retentionist institutions; moves career capital to prosecutors who win capital cases.
% ABSENT_VOICES: The condemned, whose testimony about the process is structurally screened once the warrant issues; exonerees who survived the machinery, who speak only after the fact; and categorical opponents, who are filtered out of the deciding body itself by juror death-qualification before deliberation begins. The framework admits victim impact statements but does not admit the offender's claim that no balance requires his death.
% DISAPPEARANCE_RATIONALE: If the execution authority vanished overnight, capital statutes, death rows holding roughly two thousand people, execution protocols, victims'-family legal expectations, and prosecutorial charging structures would all reorganize; a mass resentencing wave would follow, and the demand the arrangement answers would lose its state-administered answer while the underlying losses remained.
% FOUNDING_PROBLEM: Private vengeance and blood feuds following heinous killings: unbounded retaliation, escalating clan violence, and the absence of any public, final, proportionate answer to the worst crimes.
% FOUNDING_PROBLEM_CORROBORATION: Criminal-law historians corroborate the blood-feud genealogy and the reality of the founding problem; criminological surveys and restorative-justice practitioners outside the benefiting parties attest that the demand for proportionate public response to heinous killing persists today. Whether execution uniquely answers that demand is attested only by the arrangement's beneficiaries themselves — no source outside the beneficiary set vouches for the uniqueness claim.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the retributive requirement makes the offender's cost terminal and non-substitutable: once the desert axiom operates, no payment schedule, commutation-as-bargain, or lesser sentence discharges the debt, and the wrongfully convicted bear an irreversible version of the same cost with exits running only through luck. Suppression (0.74) reflects the sealed alternative space: life-without-parole is ruled out by the reading's own axiom rather than by empirical comparison, clemency is narrow and discretionary, and juror death-qualification removes categorical refusal before deliberation begins. Theater ratio is moderate-low (0.28): trials, appeals, and executions perform their declared function within the framework's own terms, but a growing share of procedural activity — decades of layered review, ceremonial warrant-signing, protocol choreography — functions increasingly as legitimation display. Accessibility collapse (0.58) is partial: accepting the desert axiom collapses the imprisonment alternative analytically, yet half the jurisdictions and most peer democracies operate the alternative in practice. Resistance (0.60) is sustained and organized: repeal campaigns, innocence litigation, moratoria, and international pressure meet the arrangement continuously. The measurement series run on one shared time grid (points 0, 10, 20, 30, 40, 50) with every tracked metric authored at every point; the suppression_requirement series is included because the story specifically tracks enforcement hardening — statutory limitation of successive petitions and streamlined review built up across the interval — not merely static suppression.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the prosecutor's chair, the arrangement is ordered justice: a bounded, reviewed, discretionary process replacing chaos, in which the death notice is a negotiating instrument and the outcome a career credential. From the condemned prisoner's chair, the identical structure is a terminal extraction with sealed exits and a closing archive of his own account. From the wrongfully convicted defendant's chair, it is worse: the machinery's celebrated safeguards are the thing that failed him, and the framework's own error clause — tragic but non-invalidating — prices his life as an acceptable rounding error. The victims'-family seat experiences genuine receipt: whatever the outcome studies ultimately show, the promise of discharge is what the arrangement sells them, and many report receiving it. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families sit nearest the beneficiary end: the arrangement subsidizes their claim, they collect the restoration, and their trapped position reflects the crime, not the arrangement's design against them. Executed offenders and wrongfully convicted defendants sit at the full-target end: powerless, trapped, bearing the entire transferred cost, with the wrongfully convicted marginally further toward full target because their debt was never incurred. Prosecutors derive low-to-moderate directionality — they collect career capital and control charging, but they also carry the workload and political exposure of the machinery. Retentionist institutions derive low directionality as beneficiaries of expressive authority. Categorical opponents are excluded rather than coordinated: the arrangement's filtering of the jury pool is precisely what keeps their position out of the room where each case is decided.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unbounded private vengeance after heinous killing — remains live, so this is not a mandate outliving its function; the arrangement is not drifting toward piton. The classification work cuts both ways. Against mislabeling as pure snare: the arrangement does solve a real collective-action problem — channeling retaliatory demand into a bounded public procedure — and its beneficiaries receive something the framework genuinely aims to deliver, which is why the tangled-rope structure rather than snare is the honest claim. Against mislabeling as pure rope: the extraction is severe, asymmetric, and enforced — the coordinated parties are not the ones paying, the paying parties did not consent, and the alternative space is closed by axiom rather than by demonstration. The R5 interview records the founding problem as live with corroboration from criminal-law historians and criminologists outside the beneficiary set; the uniqueness claim — that only death discharges the debt — is corroborated by no one outside the beneficiaries, and that gap is carried by the substitutability omega rather than resolved by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the state_execution_authority kernel — the retributive reading, which places victims'' families in the beneficiary set, treats the executed offender as a legitimate cost, and holds the moral-restoration requirement non-substitutable. Would a sibling reading (deterrence or abolition) change the structural classification?',
    'Comparative analysis across the three reading files of the kernel: the deterrence reading is indifferent between execution and equally deterrent substitutes (its beneficiary set and epsilon differ), and the abolition reading empties the beneficiary set entirely and relocates every participant to the target side.',
    'If the abolition reading prevailed, this constraint''s beneficiaries vanish, the executed offender becomes a pure victim, and the classification collapses toward snare; if the deterrence reading prevailed, the non-substitutability that drives high epsilon dissolves because a cheaper substitute delivering the same prevention would be acceptable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame uncertainty: which reading of the execution-authority kernel governs determines the entire beneficiary/victim structure.').

omega_variable(
    wrongful_execution_threshold,
    'What is the actual rate of wrongful capital conviction, and above what error rate does the retributive framework''s treatment of wrongful execution as tragic-but-non-invalidating error become untenable?',
    'Post-conviction DNA exoneration studies, matched comparison of capital versus non-capital homicide conviction error rates, and death-row exoneration counts per executions carried out.',
    'Below a low threshold, the error-absorption premise holds and the arrangement retains its coordination character; above it, the arrangement imposes irreversible terminal costs on innocents whose exits are sealed, and the classification shifts decisively toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_threshold, empirical, 'Whether the documented wrongful-conviction rate stays within the bound the retributive framework can absorb as tragic error.').

omega_variable(
    victim_restoration_authenticity,
    'Does execution actually deliver the moral restoration that puts victims'' families in the beneficiary set, or is the restoration promised but rarely received?',
    'Longitudinal studies of victims'' family members comparing psychological outcomes after execution versus life-without-parole resolutions, including closure measures collected independent of advocacy organizations.',
    'If restoration reliably fails to materialize, the primary beneficiary''s benefit evaporates, the coordination function thins to expressive politics alone, and the arrangement drifts toward pure extraction maintained by enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_restoration_authenticity, empirical, 'Whether the declared benefit to victims'' families is actually delivered by the mechanism.').

omega_variable(
    substitutability_conceptual_status,
    'Is the claim that moral balance requires death — that imprisonment cannot discharge the debt — a conceptual truth about desert or a culturally contingent construction?',
    'Cross-jurisdictional and cross-cultural comparison of societies that abandoned capital punishment: if communities report equivalent moral closure through life-imprisonment regimes, the non-substitutability premise is contingent rather than necessary.',
    'If substitutability holds, the feature that makes the offender''s cost unavoidable disappears, effective extraction drops sharply, and the arrangement becomes assessable as an ordinary punishment-allocation policy rather than a non-negotiable moral requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitutability_conceptual_status, conceptual, 'Whether the non-substitutability driving high extraction is necessary or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__retributive_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(stat_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__retributive_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(stat_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__retributive_reading, suppression_requirement, 50, 0.74).
narrative_ontology:measurement_basis(stat_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, abolition_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the state_execution_authority kernel. The colloquial label 'capital punishment' covers three structurally distinct claims with different epsilon values, beneficiary sets, and failure modes: the retributive reading (this file — restoration through proportionate desert, victims' families as beneficiaries, non-substitutable cost), the deterrence reading (prevention through raised costs, indifferent to means, substitutability intact), and the abolition reading (categorical prohibition, empty beneficiary set). Each is authored as a separate file per the epsilon-invariance principle; this file links to both siblings. The retributive reading sits downstream of the shared constitutional text and upstream of neither sibling logically — it forecloses the abolition reading within any single normative framework and merely coexists with the deterrence reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
