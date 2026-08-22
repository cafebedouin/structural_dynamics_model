% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: Capital Punishment as Deterrence Signal (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of the contested
 *   state-killing-legitimacy kernel: capital punishment justified not by
 *   desert (the retributive reading) or condemned outright (the abolition
 *   reading) but as a rational, forward-looking signal calibrated to reduce
 *   future homicides by altering the incentive structure of potential
 *   offenders. The offender is treated as a means to a social end — their
 *   death is an instrument of public communication, not (primarily) a
 *   proportional response to their own act. This reading's ε sits at a
 *   moderate 0.52 because the empirical support for a measurable deterrent
 *   effect is genuinely contested: decades of econometric studies produce
 *   conflicting results, and the National Research Council's 2012 review
 *   found the evidence insufficient to draw causal conclusions either way.
 *   The theater_ratio rises over time (0.25 to 0.44) as the persistence of
 *   the deterrence justification in political and prosecutorial rhetoric
 *   increasingly outpaces the strength of the empirical case supporting it —
 *   a growing gap between the stated function (crime prevention) and the
 *   demonstrated function (signal maintenance regardless of proof).
 *
 * KEY AGENTS:
 *   - condemned_offender: primary target (powerless/trapped) — instrumentalized as the signal-bearer
 *   - potential_future_victims: claimed beneficiary (powerless/analytical) — an unidentifiable statistical population
 *   - state_prosecutorial_apparatus: agenda_setter (institutional/arbitrage) — administers and defends the policy
 *   - deterrence_theory_advocates: beneficiary (organized/mobile) — careers and legitimacy tied to the claim
 *   - wrongfully_convicted_death_row_inmates: payer (powerless/trapped) — signal function persists independent of guilt
 *   - criminologists_and_statisticians: analytical observer (analytical/analytical) — measures the contested causal claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.52).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.68).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment as Deterrence Signal (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'a022c1cb-1e4c-43a5-b774-205ac8aab8af').
narrative_ontology:cs_kernel_codification('a022c1cb-1e4c-43a5-b774-205ac8aab8af', distributed).
narrative_ontology:cs_authority_grounding('a022c1cb-1e4c-43a5-b774-205ac8aab8af', distributed).
narrative_ontology:cs_reading_relation('a022c1cb-1e4c-43a5-b774-205ac8aab8af', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('a022c1cb-1e4c-43a5-b774-205ac8aab8af', state_killing_legitimacy__abolition_reading, influences).
narrative_ontology:cs_axiom('a022c1cb-1e4c-43a5-b774-205ac8aab8af', foundational, instrumentalization_permissible_for_net_life_savings).
narrative_ontology:cs_axiom_status(instrumentalization_permissible_for_net_life_savings, holdable).
narrative_ontology:cs_axiom_grounding('a022c1cb-1e4c-43a5-b774-205ac8aab8af', instrumentalization_permissible_for_net_life_savings, instrumental).
narrative_ontology:cs_axiom('a022c1cb-1e4c-43a5-b774-205ac8aab8af', secondary, deterrent_causal_efficacy_required_for_justification).
narrative_ontology:cs_axiom_status(deterrent_causal_efficacy_required_for_justification, holdable).
narrative_ontology:cs_axiom_grounding('a022c1cb-1e4c-43a5-b774-205ac8aab8af', deterrent_causal_efficacy_required_for_justification, empirically_contingent).
narrative_ontology:cs_reference_frame('a022c1cb-1e4c-43a5-b774-205ac8aab8af', utilitarian_crime_prevention_framework).
narrative_ontology:cs_drift_state('a022c1cb-1e4c-43a5-b774-205ac8aab8af', post_nrc_2012_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a022c1cb-1e4c-43a5-b774-205ac8aab8af', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_prosecutorial_apparatus).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, deterrence_theory_advocates).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offender).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, offender_family_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death and executed not primarily as proportional response to their own act (the retributive frame) but as an instrumental signal broadcast to the population of would-be future offenders. Under this reading their death is a means calibrated to a social end beyond themselves; they have no exit once sentence is finalized and appeals are exhausted.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offender, payer,
    powerless, immediate, trapped, national).

% An unidentified, statistically-constructed population whose lives are claimed to be saved by the deterrent signal the execution sends. They benefit only if the deterrence mechanism actually works — a contested empirical claim — and cannot be named, consulted, or shown to exist as specific individuals.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Sets and enforces capital sentencing policy, selects which cases are charged capitally, and defends the deterrence rationale in courts and legislatures. Bears no personal cost from the execution and can revise or retain the policy at will; the deterrence justification gives prosecutorial and legislative action a scientific-sounding legitimacy independent of proof.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Policy researchers, prosecutors' associations, and political figures whose careers, funding, and platforms are built on the deterrence claim. They benefit from the claim's continued plausibility regardless of whether the underlying causal mechanism is empirically established.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, deterrence_theory_advocates, beneficiary,
    organized, generational, mobile, national).

% Executed or awaiting execution despite actual innocence or seriously compromised process. Under the deterrence frame their deaths still function as signal-events for the broader population regardless of factual guilt, which the reading does not structurally distinguish from a guilty offender's death for signaling purposes.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, national).

% Bear grief, stigma, and material loss from the execution without having been party to the crime. The deterrence framing offers them no proportionality argument (as retribution would) — their loss is simply collateral to a broadcast aimed at strangers.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, offender_family_members, payer,
    powerless, biographical, trapped, local).

% Conduct empirical studies attempting to measure whether executions actually reduce future homicide rates. Findings are contested and methodologically fraught (confounding variables, small sample sizes of executing jurisdictions, publication selection effects), which is precisely what keeps this reading's ε moderate rather than settled.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminologists_and_statisticians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: If the deterrent mechanism is real, the arrangement solves a genuine collective problem: reducing the incidence of future murders by raising the credible cost of committing one, coordinating the population's expectations around a severe, publicized consequence.
% TRANSFER_FUNCTION: Moves the offender's life from the offender to an instrumental use: their death is converted into a public signal intended to alter the behavior of a diffuse population of potential future offenders, for the claimed benefit of potential future victims.
% ABSENT_VOICES: The potential future victims who would supposedly benefit cannot be identified, consulted, or shown to exist in numbers greater than zero; they are a statistical hypothesis, not a party who can testify to the arrangement's value. Wrongfully convicted inmates are structurally silenced by the same instrumental logic — their factual innocence does not disrupt their usefulness as a signal-event.
% DISAPPEARANCE_RATIONALE: If the deterrence justification vanished, capital punishment in jurisdictions relying primarily on it would face intensified pressure to abolish or re-ground itself in retribution instead; prosecutors and death-penalty advocacy organizations dispute whether removing the deterrence rationale would actually reduce executions (since retributive framings often substitute) or whether the practice would collapse without a forward-looking justification that appeals to public safety rather than backward-looking desert.
% FOUNDING_PROBLEM: The stated problem is recidivism and general crime prevention: the claim that visible, severe punishment for murder measurably reduces the future incidence of murder by altering the cost-benefit calculation of would-be offenders.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutorial associations and some criminologists (e.g., studies using panel-data econometric methods) attest the deterrent effect is real and the problem remains live. The National Research Council's 2012 review and a substantial majority of surveyed criminologists (outside prosecutorial and advocacy interests) conclude the empirical evidence does not support a measurable deterrent effect distinguishable from life imprisonment, meaning the founding problem this reading claims to solve may not exist in the form claimed.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate rather than high because IF the deterrent mechanism is real, there is a genuine coordination function (reducing future murders) that partially offsets the cost imposed on the offender — this is what distinguishes the deterrence reading structurally from a pure snare. Suppression (0.68) is substantial because carrying out an execution requires an extensive coercive and procedural apparatus (courts, corrections systems, appeals suppression, execution protocols) regardless of whether the deterrent claim is true. Accessibility_collapse is moderate (0.42) — alternatives (life imprisonment, restorative approaches) remain visibly available and are actively debated, unlike a genuine mountain where alternatives have collapsed entirely. Resistance is substantial (0.58), reflecting robust ongoing legal and political challenge to the deterrence rationale specifically (as distinct from opposition to capital punishment generally).
 *
 * PERSPECTIVAL GAP:
 *   From the state_prosecutorial_apparatus seat, the arrangement looks like functioning coordination: a public-safety mechanism whose costs (a small number of executions) are justified by a large diffuse benefit (murders prevented). From the condemned_offender's seat, and especially from the wrongfully_convicted_death_row_inmates' seat, the same structure is pure instrumentalization — their deaths serve a purpose entirely external to their own culpability or personhood, converting them into a message rather than treating them as an end. The engine should compute a payer-seat classification closer to snare/tangled_rope and an agenda-setter-seat classification closer to rope, and that divergence is the analytically important fact this reading is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned_offender and wrongfully_convicted_death_row_inmates carry victim status and trapped exit, pushing directionality toward the full-target end — the constraint's cost lands entirely and irreversibly on them. potential_future_victims are declared beneficiaries but their exit_options are marked 'analytical' rather than any real-world option, reflecting that this beneficiary class is a theoretical construct whose existence and size are exactly the contested empirical question — this is a structurally unusual beneficiary group precisely because it cannot be named or consulted (see the absent_voices answer). state_prosecutorial_apparatus and deterrence_theory_advocates sit at the clear beneficiary end with mobile/arbitrage exit, since their institutional standing and careers do not depend on the deterrence claim being empirically vindicated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem framing prevents this reading from being flattened into either a pure snare (which would ignore the possibility that a genuine, if contested, coordination function exists) or a pure rope (which would ignore that the offender and especially wrongfully convicted inmates bear irreversible, non-consensual costs regardless of whether the claimed function is real). The founding_problem_status is marked contested rather than dead specifically because the NRC review and the majority of criminological literature outside prosecutorial interests suggest the problem as originally framed (measurable deterrence beyond life imprisonment) may not exist, while the practice persists at full institutional force — this is the mismatch the R5 corroboration answer is built to surface for the mandatrophy consumer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_reading_within_kernel_contest,
    'Given that state_killing_legitimacy is a kernel with three live readings (deterrence, retributive, abolition), does this deterrence reading''s moderate ε (driven by contested empirical evidence) understate or overstate the constraint''s true extraction relative to how a settled empirical finding would move it?',
    'A structurally rigorous natural experiment (e.g., matched-jurisdiction panel study with credible causal identification) resolving whether capital punishment produces a measurable marginal deterrent effect beyond life imprisonment. If resolved negatively, this reading''s coordination-function premise collapses and ε should rise toward the abolition reading''s implicit framing; if resolved positively, ε could fall as the coordination function is empirically vindicated.',
    'Reclassification risk: if the deterrent effect is conclusively falsified, the deterrence_reading''s tangled_rope classification (coordination + extraction) loses its coordination leg entirely and the constraint should be re-authored as a snare — the offender instrumentalized for a benefit that does not exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_reading_within_kernel_contest, empirical, 'Whether the contested deterrence effect is real, which determines whether this reading retains any coordination function distinct from its sibling readings.').

omega_variable(
    wrongful_conviction_signal_equivalence,
    'Does the deterrence rationale, taken on its own instrumental logic, structurally distinguish between executing a guilty offender and executing a wrongfully convicted one, given that both function identically as public signals?',
    'Doctrinal and philosophical analysis of whether deterrence theory as actually argued by its proponents contains an internal desert-constraint (i.e., smuggles in retributive reasoning to exclude wrongful executions) or is genuinely indifferent to factual guilt.',
    'If the deterrence rationale is genuinely guilt-indifferent, this substantially increases the reading''s authored extractiveness and moves the wrongfully_convicted_death_row_inmates seat''s classification further toward pure snare, since their instrumentalization would carry none of the offsetting justification even the reading''s own proponents claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_signal_equivalence, conceptual, 'Whether deterrence theory has an implicit desert-constraint or is purely consequentialist regardless of guilt.').

omega_variable(
    beneficiary_population_existence,
    'Does the beneficiary class ''potential_future_victims'' refer to any real, non-zero population, or is it a theoretical construct that exists only within the deterrence argument''s own premises?',
    'Requires resolution of the underlying empirical deterrence question (see deterrence_reading_within_kernel_contest); additionally requires philosophical clarification of what counts as ''benefiting'' from a statistically hypothesized but individually unidentifiable prevented harm.',
    'If no real beneficiary population exists, the tangled_rope classification''s coordination leg (requiring a genuine beneficiary) fails entirely, and the constraint should be re-evaluated as a snare with a purely rhetorical beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_population_existence, conceptual, 'Whether the claimed beneficiary class has any real referent or is purely rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__deterrence_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__deterrence_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__deterrence_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__deterrence_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__deterrence_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__deterrence_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__deterrence_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__deterrence_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__deterrence_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__deterrence_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__deterrence_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__deterrence_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_killing_legitimacy kernel. deterrence_reading instrumentalizes the offender as means to social end (moderate ε from contested empirical deterrent evidence); retributive_reading treats the offender's death as proportional desert (lex talionis, ε driven by proportionality contestation, not empirical uncertainty); abolition_reading treats any execution as categorically impermissible regardless of desert or utility (ε near-maximal by the abolitionist's own lights, since the abolition reading's referent is the standing execution arrangement, not its endorsed alternative). The deterrence reading INFLUENCES the abolition reading by supplying (and, if empirically falsified, undermining) one of the two major utilitarian counter-arguments abolitionists must address; it COEXISTS WITH the retributive reading because both readings currently justify the same practice in different jurisdictions and legislative records without either being logically foreclosed by the other — a single legal system's death penalty statute often invokes both rationales simultaneously in different provisions or judicial opinions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
