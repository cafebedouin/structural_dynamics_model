% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: Capital Punishment Justified as Deterrent Signal
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of the
 *   state-killing-legitimacy kernel: execution is justified not by what the
 *   offender deserves (the retributive reading) or condemned outright
 *   regardless of outcome (the abolition reading), but as a rational signal
 *   calibrated to reduce future homicides by raising the expected cost of
 *   killing. The offender is instrumentalized as a means to a social end —
 *   their death is valuable to the state not for its own sake but for its
 *   communicative and deterrent effect on an unidentifiable population of
 *   potential future offenders and victims. The empirical support for the
 *   deterrent mechanism is genuinely contested in the criminology literature
 *   (the NRC 2012 review found existing studies uninformative), which is the
 *   structural source of this reading's moderate rather than extreme
 *   extractiveness — the coordination story may be partly real, but it also
 *   may be a fully unverifiable justification riding on the machinery of
 *   execution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.58).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment Justified as Deterrent Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '396d5509-8730-4024-b570-9b3cf2d71395').
narrative_ontology:cs_kernel_codification('396d5509-8730-4024-b570-9b3cf2d71395', formalized).
narrative_ontology:cs_authority_grounding('396d5509-8730-4024-b570-9b3cf2d71395', extraction).
narrative_ontology:cs_interpretation_layer_present('396d5509-8730-4024-b570-9b3cf2d71395').
narrative_ontology:cs_reading_relation('396d5509-8730-4024-b570-9b3cf2d71395', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('396d5509-8730-4024-b570-9b3cf2d71395', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('396d5509-8730-4024-b570-9b3cf2d71395', foundational, offender_instrumentalization_for_aggregate_welfare).
narrative_ontology:cs_axiom_status(offender_instrumentalization_for_aggregate_welfare, holdable).
narrative_ontology:cs_axiom_grounding('396d5509-8730-4024-b570-9b3cf2d71395', offender_instrumentalization_for_aggregate_welfare, instrumental).
narrative_ontology:cs_axiom('396d5509-8730-4024-b570-9b3cf2d71395', foundational, deterrent_signal_causally_reduces_future_homicides).
narrative_ontology:cs_axiom_status(deterrent_signal_causally_reduces_future_homicides, holdable).
narrative_ontology:cs_axiom_grounding('396d5509-8730-4024-b570-9b3cf2d71395', deterrent_signal_causally_reduces_future_homicides, empirically_contingent).
narrative_ontology:cs_reference_frame('396d5509-8730-4024-b570-9b3cf2d71395', utilitarian_penal_calculus).
narrative_ontology:cs_drift_state('396d5509-8730-4024-b570-9b3cf2d71395', post_nrc_2012_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('396d5509-8730-4024-b570-9b3cf2d71395', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_deterrence_apparatus).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, elected_prosecutors_and_officials).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, offender_families).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_actor_deterrence_theory).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, state_capacity_to_calibrate_punishment_to_social_utility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death not primarily because of what they deserve as individuals but as an instrument of a broader signal the state sends to future would-be killers. Their execution is scheduled, appealed, and carried out by a state apparatus that treats their death as a communicative act aimed at third parties, not at them. They have no exit from the sentence once affirmed on appeal.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offenders, payer,
    powerless, immediate, trapped, national).

% A subset of the condemned population who did not commit the crime but were convicted through eyewitness error, prosecutorial misconduct, or inadequate defense. The deterrence rationale requires irreversible execution to work as a signal, which forecloses correction once carried out; they bear the full cost of any error in the system with no path to remedy after execution.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, national).

% Carry social stigma, financial cost of appeals, and the loss of a family member as a side effect of the state's demonstration project. They have no standing in the deterrence calculus at all — they are neither offenders nor beneficiaries, simply uncompensated collateral.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, offender_families, payer,
    powerless, biographical, trapped, local).

% An unidentifiable, statistical population who would be saved from future murder if the deterrent effect is real. They cannot be named, cannot consent to or contest the arrangement made in their name, and the entire justification rests on an empirical claim about their existence that cannot be verified for any specific individual.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Prosecutes capital cases, carries out sentences, and produces the public narrative that executions reduce future homicide rates. Selects which studies and statistics to cite, controls execution scheduling and publicity, and bears no direct cost if the deterrence claim turns out to be empirically false — the mechanism persists regardless of what the evidence shows.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_deterrence_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain political capital from being 'tough on crime' by seeking and obtaining death sentences, using the deterrence rationale as public justification. Their careers benefit from visible severity regardless of whether the underlying deterrent mechanism functions as claimed; they can move to other offices or private practice regardless of outcome.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, elected_prosecutors_and_officials, beneficiary,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, elected_prosecutors_and_officials, agenda_setter).

% Study whether execution rates causally affect homicide rates using panel data and natural experiments. The empirical literature is genuinely contested — some studies find deterrent effects, most find none or find effects too small to distinguish from noise. Their findings are selectively cited by both sides of the political debate.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminology_deterrence_researchers, observer,
    analytical, generational, analytical, national).

% Vote for capital-punishment-supporting officials and serve on capital juries based on a belief in deterrence that is rarely tested against the actual empirical record presented to them. Their consent to the arrangement is real but built on contested and often outdated claims about what the evidence shows.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, general_public_as_jurors_and_voters, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism, in principle, for reducing future murders by raising the expected cost of killing to a level that rational potential offenders will weigh against the benefit of the act, protecting an unidentifiable class of future victims.
% TRANSFER_FUNCTION: Moves the offender's life from the offender to an instrumental role in a public signal, and moves whatever protective value the signal produces (if any) to potential future victims and to the political capital of officials who campaign on capital punishment's toughness.
% ABSENT_VOICES: Potential future victims cannot speak for themselves — they are a statistical abstraction invoked on their behalf without their consent or corroboration. Wrongfully convicted inmates, once executed, are permanently excluded from any subsequent exoneration process that could correct the record.
% DISAPPEARANCE_RATIONALE: If capital punishment were abolished, states that already show no measurable deterrent effect in the empirical record would see no change in homicide rates by the deterrence reading's own logic (arguing the mechanism was theater). Officials and segments of the public who believe in the signal would contest this, insisting the world would rearrange toward more killing. The disagreement is precisely about whether the coordination function is real or a cover story, which is why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: The claimed problem is unconstrained future homicide: without a maximal, irreversible penalty communicating the state's seriousness, rational potential killers would not be sufficiently deterred by lesser penalties.
% FOUNDING_PROBLEM_CORROBORATION: Prosecutors and elected officials who campaign on capital punishment attest the deterrent problem is live and execution addresses it. Outside the benefiting parties, the National Research Council's 2012 review of deterrence studies and the majority of published criminology panel-data research attest that existing studies are too methodologically flawed to establish a deterrent effect either way — meaning the founding problem's solution by this specific mechanism is empirically uncorroborated by the research community that would be positioned to confirm it.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) is moderate rather than severe because the reading retains a genuine, if contested, coordination claim: if deterrence is real, potential future victims are a real beneficiary class, distinguishing this from a pure snare. Suppression (0.72) is high because the arrangement requires an active state apparatus — courts, executioners, appellate review — to carry out irreversible punishment, and it forecloses any correction for the wrongfully convicted. Theater ratio (0.44, rising over the interval) reflects the growing gap between the strength of the empirical deterrence claim (weakening as sounder panel-data studies fail to find robust effects) and the persistence of the rationale in political and prosecutorial discourse — the signal is increasingly maintained as performance for the voting public rather than as data-supported policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned offenders and especially wrongfully convicted inmates sit at the full-target end: trapped, powerless, irreversibly harmed, with no capacity to benefit from or exit the arrangement. Potential future victims are named as the structural beneficiary class per the reading's own logic, but they are unidentifiable and cannot corroborate their own benefit, which is exactly the evidentiary gap the omega below addresses. Elected officials are a secondary beneficiary who profit politically regardless of whether the underlying mechanism functions, which is why their exit options (mobile) diverge sharply from the state apparatus's institutional arbitrage position despite sharing beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unconstrained homicide requiring maximal deterrent signal) is authored as contested rather than dead or live, because the underlying empirical claim was never robustly established even at founding — this is not a mandate that clearly solved its problem and then outlived it, but one whose problem-solving status has always been uncertain. Classifying this as tangled_rope rather than snare preserves the distinction between an arrangement with zero coordination function and one with a contested-but-possible one; classifying it as tangled_rope rather than rope acknowledges the asymmetric, irreversible cost borne by a class (wrongfully convicted inmates) whose harm cannot be coordination-justified even if deterrence is real for others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_effect_empirical_reality,
    'Does execution actually produce a measurable marginal deterrent effect on homicide rates beyond what long-term imprisonment produces, or is the deterrence claim empirically unsupported?',
    'Meta-analysis of natural experiments (moratoria, Furman-era suspension and reinstatement, cross-state panel data controlling for simultaneity) using methodologically rigorous causal inference, as attempted by the NRC 2012 panel.',
    'If no deterrent effect exists, the entire beneficiary class (potential_future_victims) is fictional and the constraint collapses toward a snare — irreversible extraction from condemned offenders with no offsetting coordination benefit. If a robust effect is found, the tangled_rope classification''s coordination leg is substantially strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_effect_empirical_reality, empirical, 'Whether the deterrent mechanism this reading depends on is empirically real.').

omega_variable(
    instrumentalization_vs_desert_distinctness,
    'Is treating the offender purely as a means to future-victim protection (rather than as receiving deserved punishment) a coherent moral basis for execution independent of retributive justification, or does the deterrence reading covertly smuggle in desert-based reasoning to justify why THIS offender (rather than a random person) is the one sacrificed for the signal?',
    'Philosophical analysis of whether deterrence theory can select a specific offender for execution without appeal to desert — e.g., whether a deterrence-only framework could justify executing a random innocent person if it produced an equivalent signal, and whether real-world capital sentencing ever actually operates this way.',
    'If deterrence cannot coherently select offenders without borrowing desert-based criteria, this reading may not be structurally independent of the retributive_reading, which would change the reading_relations declared in cs_structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalization_vs_desert_distinctness, conceptual, 'Whether pure deterrence reasoning can stand independent of retributive desert in offender selection.').

omega_variable(
    wrongful_conviction_irreversibility_asymmetry,
    'How does the deterrence reading''s own logic weigh the cost of executing an innocent person (which produces a false signal and destroys any future correction) against the projected marginal benefit to potential future victims?',
    'Formal cost-benefit modeling using documented exoneration rates for capital cases (DNA-era exoneration data) against the best-available deterrence effect-size estimates.',
    'If the wrongful-execution cost dominates the expected deterrent benefit under any plausible parameter range, the deterrence reading''s own utilitarian logic would counsel against retaining execution, which is a strong internal-consistency omega for this reading specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_irreversibility_asymmetry, empirical, 'Whether the deterrence reading''s utilitarian calculus is internally consistent given known wrongful conviction rates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__deterrence_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__deterrence_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__deterrence_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__deterrence_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__deterrence_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__deterrence_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__deterrence_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__deterrence_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__deterrence_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__deterrence_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__deterrence_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__deterrence_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the state_killing_legitimacy kernel, each authored as a separate constraint with its own ε: the retributive_reading (proportional desert, forfeiture of life-right independent of utility), the deterrence_reading (this story — instrumentalization for social signal, moderate contested ε), and the abolition_reading (categorical dignity violation, which would author execution's ε as near-maximal extraction with no coordination leg at all). The readings are linked here rather than merged because their beneficiary structures, victim sets, and empirical dependencies are structurally distinct — merging them would violate ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
