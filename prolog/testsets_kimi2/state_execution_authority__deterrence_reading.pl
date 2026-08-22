% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority â Deterrence Justification
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the state execution authority as justified
 *   by deterrence theory: the state kills convicted murderers to raise the
 *   cost of homicide and thereby prevent future murders. The kernel is
 *   contested â the same legal apparatus is read by retributivists as moral
 *   balancing and by abolitionists as categorical wrong. This JSON
 *   instantiates ONLY the deterrence reading. The executed offender is
 *   treated as an instrumental cost in the utilitarian calculus; future
 *   potential victims enter as beneficiaries. Structural extraction is
 *   moderate because the coordination benefit (deterrence) is empirically
 *   contested and life-without-parole may be a perfect substitute, while the
 *   cost (life, plus wrongful executions) is definite and severe.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus: Agenda setter (institutional/constrained) â administers sentences, claims deterrence.
 *   - executed_offenders: Primary target (powerless/trapped) â bear the extraction of life.
 *   - wrongfully_convicted_defendants: Secondary target (powerless/trapped) â bear irremediable error cost.
 *   - potential_future_victims: Primary beneficiary (moderate/constrained) â diffuse statistical beneficiaries of claimed deterrence.
 *   - abolitionist_advocates: Observer/resistance (organized/constrained) â contest efficacy and morality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.58).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority â Deterrence Justification").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '37cc738d-eea9-420f-b876-dc445aef6bc9').
narrative_ontology:cs_kernel_codification('37cc738d-eea9-420f-b876-dc445aef6bc9', formalized).
narrative_ontology:cs_authority_grounding('37cc738d-eea9-420f-b876-dc445aef6bc9', lineage).
narrative_ontology:cs_interpretation_layer_present('37cc738d-eea9-420f-b876-dc445aef6bc9').
narrative_ontology:cs_reading_relation('37cc738d-eea9-420f-b876-dc445aef6bc9', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_reading_relation('37cc738d-eea9-420f-b876-dc445aef6bc9', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_axiom('37cc738d-eea9-420f-b876-dc445aef6bc9', foundational, marginal_deterrence_efficacy).
narrative_ontology:cs_axiom_status(marginal_deterrence_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('37cc738d-eea9-420f-b876-dc445aef6bc9', marginal_deterrence_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('37cc738d-eea9-420f-b876-dc445aef6bc9', foundational, instrumental_justice_permissible).
narrative_ontology:cs_axiom_status(instrumental_justice_permissible, holdable).
narrative_ontology:cs_axiom_grounding('37cc738d-eea9-420f-b876-dc445aef6bc9', instrumental_justice_permissible, instrumental).
narrative_ontology:cs_reference_frame('37cc738d-eea9-420f-b876-dc445aef6bc9', classical_deterrence_mandate).
narrative_ontology:cs_drift_state('37cc738d-eea9-420f-b876-dc445aef6bc9', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('37cc738d-eea9-420f-b876-dc445aef6bc9', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_defendants).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, deterrence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the legal and carceral machinery of capital punishment: prosecutors seek death sentences, courts affirm them, and correctional departments carry out executions. Justifies the practice by citing criminological studies claiming a deterrent effect on homicide.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Convicted of capital murder, sentenced to death, and confined on death row pending exhaustion of appeals. Subject to the ultimate state sanction; their life is the direct cost of the deterrent policy.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Convicted through erroneous testimony, prosecutorial misconduct, or flawed forensic evidence and sentenced to death. Bear the risk of irremediable execution before exoneration can occur.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_defendants, payer,
    powerless, immediate, trapped, local).

% Diffuse population of individuals who, according to the deterrence hypothesis, are spared homicide because the threat of execution deters potential murderers. They do not choose their beneficiary status and are unaware of any specific protective effect.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, potential_future_victims, beneficiary,
    moderate, biographical, constrained, national).

% Civil liberties organizations, capital defense attorneys, and moral philosophers who challenge the empirical deterrence claim and assert that execution is categorically wrong. They resist the constraint through litigation, legislative lobbying, and public advocacy.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_advocates, observer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents future murders by imposing the highest possible cost on the commission of capital crimes, thereby altering the incentive calculus of potential offenders and coordinating public safety through state violence.
% TRANSFER_FUNCTION: Transfers the ultimate cost (life) from executed offenders to the probabilistic safety of potential future victims; also transfers substantial state resources into the machinery of capital trials, appeals, and execution protocols.
% ABSENT_VOICES: The executed offender is procedurally silenced after sentencing; future victims are a statistical abstraction never present in the room; wrongfully convicted defendants are invisible until posthumous exoneration efforts. Abolitionist advocates are vocal in public debate but structurally excluded from the execution protocol itself.
% DISAPPEARANCE_RATIONALE: If the deterrence-justified execution authority vanished, jurisdictions would rely on life-without-parole sentences. Homicide rates might shift marginally if the deterrence claim is valid; prison populations would increase permanently; and the criminal justice system would lose its most severe sanction, reallocating prosecutorial and judicial resources.
% FOUNDING_PROBLEM: Homicide â the unlawful taking of innocent life â and the need for a sanction severe enough to deter it when lesser penalties fail.
% FOUNDING_PROBLEM_CORROBORATION: Criminologists and victim advocates attest that homicide remains a serious social problem. However, these same experts, along with innocence-project attorneys and abolitionist scholars outside the benefiting parties, contest whether execution provides unique deterrence. No neutral party corroborates the claim that execution is necessary for deterrence without qualification; the empirical literature is divided and hotly contested.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint imposes the ultimate cost on a small class (offenders) for a diffuse benefit (homicide reduction) whose magnitude is uncertain. Suppression is high (0.72) because the arrangement requires continuous active enforcement â legal appeals, incarceration, execution protocol â and suppresses the alternative of abolition or LWOP substitution. Theater ratio is moderate-low but rising (0.42 at interval end) because as executions become rarer, each instance carries more symbolic freight, shifting toward performance. Accessibility collapse is moderate (0.60): for the offender, alternatives collapse completely (trapped); for the policy, LWOP exists as a structural alternative but is legally suppressed by the retentionist framework. Resistance is moderate (0.55): persistent legal and moral challenge from abolitionists and defense bars.
 *
 * PERSPECTIVAL GAP:
 *   The state seat experiences the constraint as a legitimate coordination mechanism protecting future victims; the executed offender experiences it as total extraction with zero coordination benefit; the potential future victim is unaware of their beneficiary status. The engine will compute divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution apparatus sets the agenda and enforces but does not personally bear cost or benefit â directionality near the middle but with extraction authority. Executed offenders are full targets (directionality near 1.0) with trapped exit and powerless status. Potential future victims are beneficiaries (directionality near 0.0) if the deterrence effect is real, but their benefit is probabilistic and diffuse. Wrongfully convicted defendants are also full targets. The asymmetry between the concentrated, absolute cost on the offender and the diffuse, uncertain benefit on future victims drives the effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents the mandatrophy error of reading a genuine (if contested) coordination claim as pure extraction (snare), while also preventing the error of reading an actively enforced, life-taking arrangement as neutral coordination (rope). The deterrence reading must carry victims because the offender's life is irreducibly extracted; it must carry beneficiaries because the reading's own logic posits a protective function. If empirical research definitively showed zero deterrence margin over LWOP, the coordination function would atrophy and the constraint would drift toward snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy,
    'Does execution provide marginal deterrence over life-without-parole, or is the coordination benefit illusory?',
    'Panel studies comparing homicide trends in abolitionist versus retentionist jurisdictions, controlling for confounding variables; natural experiments from state-level moratoria.',
    'If no marginal deterrence exists, the coordination function collapses and the constraint shifts toward snare classification; if marginal deterrence is demonstrated, tangled_rope remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy, empirical, 'Empirical uncertainty about the deterrence effect of execution relative to LWOP').

omega_variable(
    wrongful_execution_rate,
    'What is the rate of wrongful execution, and does it render the constraint net-harmful under its own utilitarian calculus?',
    'Posthumous DNA testing, innocence commission reviews, and statistical estimation of false conviction rates in capital cases.',
    'A substantiated high error rate would increase extractiveness and expand the victim set, potentially shifting the constraint toward snare by invalidating the cost-benefit structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_rate, empirical, 'Rate of wrongful execution and its impact on utilitarian justification').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of abolitionist alternatives structural (legal prohibition) or internalized (public acceptance of state violence)?',
    'Referenda outcomes, jury behavior in abolitionist versus retentionist jurisdictions, and post-exit policy trajectories of emigrants from retentionist states.',
    'If suppression is primarily internalized, effective resistance is lower than structural measures suggest and the constraint is more deeply embedded; if structural, reform may be achieved through legal change alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of abolitionist alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sea_deter_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sea_deter_tr_t10, state_execution_authority__deterrence_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(sea_deter_tr_t20, state_execution_authority__deterrence_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(sea_deter_tr_t30, state_execution_authority__deterrence_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(sea_deter_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(sea_deter_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(sea_deter_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sea_deter_be_t10, state_execution_authority__deterrence_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sea_deter_be_t20, state_execution_authority__deterrence_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(sea_deter_be_t30, state_execution_authority__deterrence_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(sea_deter_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(sea_deter_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sea_deter_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sea_deter_su_t10, state_execution_authority__deterrence_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(sea_deter_su_t20, state_execution_authority__deterrence_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(sea_deter_su_t30, state_execution_authority__deterrence_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(sea_deter_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(sea_deter_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is the deterrence reading of the kernel state_execution_authority. It is structurally paired with retributive_reading and abolition_reading as sibling constraints generated from the same kernel via the epsilon-invariance decomposition rule. Each reading carries a distinct epsilon, beneficiary/victim structure, and axiomatic foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
