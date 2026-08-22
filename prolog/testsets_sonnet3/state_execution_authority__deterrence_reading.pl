% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: Capital Punishment Authority — Deterrence Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of the
 *   state_execution_authority kernel: the claim that executing convicted
 *   capital offenders reduces the future incidence of capital crime by
 *   raising its expected cost. This is a distinct constraint from the
 *   retributive reading (which grounds legitimacy in proportionate moral
 *   desert, independent of any measurable future effect) and the abolition
 *   reading (which holds execution categorically impermissible regardless of
 *   efficacy or procedure). Under this reading, unlike the retributive
 *   reading, the executed offender's death is instrumental rather than an end
 *   in itself, future potential victims enter the beneficiary set as a
 *   genuine (if unnamed and unverifiable) party, and the entire justification
 *   is conditional on an empirical claim that can in principle fail — which
 *   is exactly what much of the contested criminology literature suggests it
 *   does. Rising theater_ratio over the interval reflects that deterrence
 *   rhetoric in charging and sentencing has become increasingly decoupled
 *   from the weakening empirical support for the deterrent-effect claim.
 *
 * KEY AGENTS:
 *   - state_prosecutorial_apparatus: institutional agenda-setter, invokes deterrence to justify charging/sentencing policy
 *   - future_potential_murder_victims: unnamed, unverifiable statistical beneficiary class the entire reading is built to protect
 *   - condemned_offenders: instrumental cost under this reading's own logic
 *   - wrongfully_convicted_death_row_prisoners: pure utilitarian loss with no offsetting deterrent value
 *   - criminology_deterrence_researchers: analytical seat producing the contested evidence the reading depends on
 *   - life_without_parole_advocates: excluded substitution argument that would test whether execution specifically (versus severe punishment generally) is doing any deterrent work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.52).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.71).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "Capital Punishment Authority — Deterrence Reading").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, 'f54822b7-edd9-49b3-894e-e227d014cef9').
narrative_ontology:cs_kernel_codification('f54822b7-edd9-49b3-894e-e227d014cef9', formalized).
narrative_ontology:cs_authority_grounding('f54822b7-edd9-49b3-894e-e227d014cef9', extraction).
narrative_ontology:cs_interpretation_layer_present('f54822b7-edd9-49b3-894e-e227d014cef9').
narrative_ontology:cs_reading_relation('f54822b7-edd9-49b3-894e-e227d014cef9', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('f54822b7-edd9-49b3-894e-e227d014cef9', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('f54822b7-edd9-49b3-894e-e227d014cef9', foundational, execution_produces_marginal_deterrent_effect).
narrative_ontology:cs_axiom_status(execution_produces_marginal_deterrent_effect, holdable).
narrative_ontology:cs_axiom_grounding('f54822b7-edd9-49b3-894e-e227d014cef9', execution_produces_marginal_deterrent_effect, empirically_contingent).
narrative_ontology:cs_axiom('f54822b7-edd9-49b3-894e-e227d014cef9', foundational, offender_death_is_instrumentally_justified_by_third_party_protection).
narrative_ontology:cs_axiom_status(offender_death_is_instrumentally_justified_by_third_party_protection, holdable).
narrative_ontology:cs_axiom_grounding('f54822b7-edd9-49b3-894e-e227d014cef9', offender_death_is_instrumentally_justified_by_third_party_protection, instrumental).
narrative_ontology:cs_reference_frame('f54822b7-edd9-49b3-894e-e227d014cef9', utilitarian_crime_prevention_calculus).
narrative_ontology:cs_drift_state('f54822b7-edd9-49b3-894e-e227d014cef9', post_nrc_2012_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f54822b7-edd9-49b3-894e-e227d014cef9', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_murder_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, general_public_safety_interest).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_death_row_prisoners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, jurors_in_capital_cases).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, capital_punishment_deters_capital_crime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks and secures death sentences on the stated theory that the credible threat of execution suppresses the capital-crime rate. Controls charging decisions, plea leverage, and the appellate defense of the sentencing regime. Bears no direct cost if the deterrence claim is empirically wrong, since the apparatus's function and funding do not depend on measured deterrence outcomes.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% An unidentifiable statistical class whose claimed protection is the entire justification for this reading: if the deterrence mechanism functions as theorized, some unknown number of people who would otherwise be murdered are not. They cannot be named, cannot testify, and cannot register whether the mechanism actually worked in any individual case.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_murder_victims, beneficiary,
    powerless, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_execution_authority__deterrence_reading, future_potential_murder_victims).

% Are executed as the instrumental price of the deterrence signal — the theory treats the individual offender's death as a public message rather than as an end in itself. Have no exit once sentenced; appeals contest process, not the underlying logic that their death is being used to influence third parties' future behavior.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, condemned_offenders, payer,
    powerless, immediate, trapped, local).

% Bear the full weight of an irreversible error the deterrence framework has no mechanism to correct after the fact. Their execution or near-execution is a pure utilitarian loss under this reading's own logic — it produces no deterrent value (the wrong signal, sent about the wrong person) while consuming a life and destroying confidence in the system's error rate.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_death_row_prisoners, payer,
    powerless, immediate, trapped, local).

% Study homicide-rate correlations with capital punishment regimes across states and countries. The empirical literature is genuinely mixed and contested; some studies find modest deterrent effects, most find none distinguishable from execution's effect via incapacitation, and several find no measurable effect at all. Their findings are the load-bearing evidence this entire reading depends on and cannot itself generate.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminology_deterrence_researchers, observer,
    analytical, generational, analytical, national).

% Argue that life-without-parole achieves the same incapacitation and near-equivalent deterrent signal at a fraction of the cost and with full reversibility in the event of error. Their substitution argument, if empirically sound, would eliminate the deterrence reading's entire justification without requiring abolition on moral grounds — but this argument gets little institutional airtime because it threatens the prosecutorial apparatus's charging leverage in plea negotiations.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, life_without_parole_advocates, excluded,
    organized, biographical, constrained, national).

% Are asked to weigh deterrence as an aggravating consideration in sentencing instructions in many jurisdictions, but are rarely given the contested empirical literature on deterrence efficacy — they are handed the deterrence rationale as settled premise rather than as an open empirical question they might weigh differently if fully informed.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, jurors_in_capital_cases, beneficiary,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, jurors_in_capital_cases, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a credible, publicly visible cost signal attached to capital crimes, intended to shift the expected-cost calculation of a potential murderer before the act, thereby reducing the incidence of murder across the population the state protects.
% TRANSFER_FUNCTION: Moves the risk of irreversible state violence from the diffuse future population of potential murder victims onto the specific, identified population of convicted (and sometimes wrongfully convicted) capital offenders, in exchange for a claimed reduction in aggregate future harm.
% ABSENT_VOICES: Life-without-parole advocates whose substitution argument would test whether the deterrent signal survives without a death requirement; exonerated death-row survivors and the families of the wrongfully executed, whose experience is the direct empirical falsification case for this reading's error-rate premise; jurors are given the rationale but rarely the contested evidence base behind it.
% DISAPPEARANCE_RATIONALE: If capital punishment were abolished, the retributive and abolitionist camps would say the world barely changes at the level that matters to them (murder rates track back to detection, poverty, and enforcement certainty, not sentence severity, per most contested studies) — but the prosecutorial apparatus's plea-leverage and the deterrence reading's own predicted world would say murder rates should measurably rise, which is exactly the empirical claim in dispute and the reason the verdict is contested rather than settled.
% FOUNDING_PROBLEM: The problem this reading was built to address: capital crime is undersupplied with credible deterrent cost, so absent a maximal penalty, the marginal potential murderer is insufficiently deterred and future victims bear the resulting excess harm.
% FOUNDING_PROBLEM_CORROBORATION: The prosecutorial apparatus and legislative sponsors of capital statutes attest the deterrence problem is live and the execution regime addresses it. Independent criminology research — the National Research Council's 2012 review and subsequent meta-analyses, produced by researchers with no stake in prosecutorial outcomes — finds the deterrent effect is not reliably distinguishable from zero across the available natural experiments, directly undercutting the founding claim from outside the benefiting institutional seat.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is set moderate (0.52) rather than high because the deterrence reading, unlike the retributive reading, in principle ties its legitimacy to a testable causal claim — if life-without-parole achieves equivalent deterrence, this reading's own logic would counsel substitution, which caps how purely extractive the arrangement can be characterized as being (it retains a genuine, if contested, coordination function). Suppression is high (0.71) because the arrangement is enforced through the full machinery of capital prosecution, appeals foreclosure, and execution logistics — none of that machinery is optional or informally maintained. Theater ratio is moderate-to-rising (0.42 by interval end) because deterrence rhetoric persists in charging decisions and political messaging substantially past the point where the empirical consensus (NRC 2012 and subsequent reviews) stopped supporting a measurable deterrent effect distinguishable from zero — the gap between claimed function and demonstrated function is exactly what the theater_ratio metric is tracking. accessibility_collapse is moderate (0.4): the life-without-parole alternative remains visible and argued, it has simply not displaced the death-penalty apparatus institutionally. Resistance is substantial (0.6): abolitionist advocacy, wrongful-conviction exoneration organizations, and substitution advocates actively contest the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The prosecutorial apparatus and the condemned-offender seats compute this constraint very differently even under the shared deterrence framework: from the apparatus's seat, execution is a coordination mechanism producing a real public good (fewer future murders); from the condemned offender's seat, the same act is being used instrumentally as a signal to third parties who are not present and cannot be identified — their death is a means to someone else's safety, which is precisely the utilitarian structure that makes wrongful execution catastrophic under this reading's own terms, not just under the abolitionist's.
 *
 * DIRECTIONALITY LOGIC:
 *   future_potential_murder_victims are coded as beneficiary but marked agent:false because they are a statistical class, not an identifiable actor capable of registering benefit or objecting — this keeps them out of any directionality computation that would otherwise treat an unfalsifiable class as if it were exerting agency. condemned_offenders and wrongfully_convicted_death_row_prisoners are full targets: trapped exit, powerless, bearing the entire realized cost of a claim whose benefit (if real) accrues to people who will never know it was for them. jurors carry a dual beneficiary/excluded role: they benefit from a safer society if deterrence is real, but are systematically excluded from the contested evidence base that would let them evaluate that claim when sentencing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem × disappearance_verdict mismatch is the diagnostic core of this reading: founding_problem_status is contested precisely because the prosecutorial apparatus attests the deterrence problem remains live while independent criminology research corroborating from outside the benefiting institutional seat finds no reliably measurable deterrent effect. This is the classic mandatrophy signature — an arrangement whose stated justification may have gone empirically dead while the enforcement apparatus (rising suppression_requirement, rising theater_ratio) continues to intensify. The classification records this as tangled_rope rather than snare because the coordination function (a genuine, falsifiable deterrence claim) has not been definitively falsified — only substantially undercut — leaving open the possibility that the arrangement is still, in part, doing what it claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_substitutability,
    'Does execution produce a measurable marginal deterrent effect beyond what life-without-parole sentencing achieves through incapacitation and severe punishment alone?',
    'Comparative panel-data analysis across jurisdictions that have abolished capital punishment versus retained it, controlling for clearance rates, sentencing severity generally, and socioeconomic covariates; the NRC 2012 review and subsequent replications are the most direct existing evidence.',
    'If no measurable marginal effect exists beyond life-without-parole, this reading''s own internal logic collapses into the retributive reading wearing deterrence language as cover — the coordination function would be nullified and the classification should move toward snare. If a genuine marginal effect is found, the tangled_rope classification''s coordination component is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_substitutability, empirical, 'Whether execution deters beyond life-without-parole — the load-bearing empirical claim of this reading.').

omega_variable(
    wrongful_execution_error_rate,
    'What is the actual rate of wrongful capital conviction, and does it exceed the threshold at which the deterrence reading''s own utilitarian calculus would counsel abolition even on its own terms?',
    'DNA-exoneration base rates extrapolated to pre-DNA-era executions, combined with innocence-project audit data on death-row exoneration rates.',
    'A sufficiently high error rate makes the expected utilitarian cost of wrongful executions exceed the claimed deterrent benefit even accepting the reading''s own framework, without needing to invoke the abolitionist''s categorical premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_error_rate, empirical, 'Whether the error rate alone defeats the deterrence reading on its own utilitarian terms.').

omega_variable(
    reading_framing_boundary,
    'Is the coordination-function framing (a falsifiable deterrence claim) the correct structural lens for this reading, or does the prosecutorial apparatus''s institutional interest in charging leverage mean the deterrence rationale is itself functioning as post-hoc justification for retributive or institutional-power motives that predate and outlast the empirical claim?',
    'Track whether prosecutorial charging and public advocacy patterns shift in response to adverse deterrence-efficacy findings; if the rhetoric persists unchanged despite contrary evidence, that supports the post-hoc-justification framing over the genuine-coordination framing.',
    'If deterrence rhetoric is confirmed as post-hoc rationalization rather than a genuinely load-bearing claim, this reading''s classification should be revisited toward snare; the rising theater_ratio trend already authored is consistent with, but does not settle, this possibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_boundary, conceptual, 'Whether the deterrence framing is a genuine falsifiable coordination claim or institutional cover that would persist regardless of evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__deterrence_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__deterrence_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__deterrence_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__deterrence_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__deterrence_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__deterrence_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the state_execution_authority kernel. state_execution_authority__retributive_reading grounds legitimacy in proportionate desert independent of future effects (ε reflects moral-balance claims, not empirical contingency). state_execution_authority__abolition_reading treats execution as categorically impermissible (ε reflects the standing execution arrangement as the abolitionist sees it, not the abolitionist's endorsed zero-execution alternative). This deterrence reading is distinguished by making legitimacy conditional on a testable causal claim, which the other two readings do not share — it is the only reading of the three whose own internal logic could, in principle, collapse under empirical disconfirmation without requiring adoption of a different reading's premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
