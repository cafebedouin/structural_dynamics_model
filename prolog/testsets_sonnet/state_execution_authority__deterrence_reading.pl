% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: State Execution Authority — Deterrence Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the deterrence reading of the
 *   state_execution_authority kernel: capital punishment is justified as a
 *   crime-control mechanism that raises the expected cost of capital crimes
 *   above what life imprisonment alone imposes, thereby producing a lower
 *   equilibrium rate of murder. This is structurally distinct from the
 *   retributive reading (moral desert, proportionate punishment for its own
 *   sake — not modeled here) and the abolition reading (categorical
 *   impermissibility regardless of consequences — also not modeled here).
 *   Under this reading, future potential victims are the beneficiary class,
 *   the executed offender is an instrumental cost rather than a deserving
 *   recipient of punishment, and the central empirical question is
 *   substitutability: if life-without-parole achieves equivalent deterrence
 *   at zero execution-error risk, the marginal extraction of this specific
 *   mechanism (as opposed to incapacitation generally) becomes unjustified on
 *   its own terms. Wrongful execution is treated here as pure utilitarian
 *   loss, not moral tragedy per se, which drives the requirement that the
 *   reading minimize error rate to sustain its own legitimacy.
 *
 * KEY AGENTS:
 *   - state_prosecutorial_apparatus: agenda_setter, administers capital charging and defends deterrence rationale
 *   - potential_future_victims: diffuse beneficiary class, exists only if the deterrence hypothesis is true
 *   - executed_offenders: instrumental cost-bearer under this reading's own logic
 *   - wrongfully_convicted_death_row_inmates: pure utilitarian loss with no offsetting benefit
 *   - criminology_researchers: analytical observers testing the deterrence hypothesis empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.42).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.55).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority — Deterrence Reading").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '4d53ec6f-729a-435e-bc90-4584ce67de2c').
narrative_ontology:cs_kernel_codification('4d53ec6f-729a-435e-bc90-4584ce67de2c', formalized).
narrative_ontology:cs_authority_grounding('4d53ec6f-729a-435e-bc90-4584ce67de2c', lineage).
narrative_ontology:cs_interpretation_layer_present('4d53ec6f-729a-435e-bc90-4584ce67de2c').
narrative_ontology:cs_reading_relation('4d53ec6f-729a-435e-bc90-4584ce67de2c', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d53ec6f-729a-435e-bc90-4584ce67de2c', state_execution_authority__abolition_reading, influences).
narrative_ontology:cs_axiom('4d53ec6f-729a-435e-bc90-4584ce67de2c', foundational, punishment_justified_by_future_consequences_only).
narrative_ontology:cs_axiom_status(punishment_justified_by_future_consequences_only, holdable).
narrative_ontology:cs_axiom_grounding('4d53ec6f-729a-435e-bc90-4584ce67de2c', punishment_justified_by_future_consequences_only, empirically_contingent).
narrative_ontology:cs_axiom('4d53ec6f-729a-435e-bc90-4584ce67de2c', secondary, offender_treatable_as_instrumental_means_to_population_safety).
narrative_ontology:cs_axiom_status(offender_treatable_as_instrumental_means_to_population_safety, holdable).
narrative_ontology:cs_axiom_grounding('4d53ec6f-729a-435e-bc90-4584ce67de2c', offender_treatable_as_instrumental_means_to_population_safety, instrumental).
narrative_ontology:cs_reference_frame('4d53ec6f-729a-435e-bc90-4584ce67de2c', utilitarian_crime_suppression_framework).
narrative_ontology:cs_drift_state('4d53ec6f-729a-435e-bc90-4584ce67de2c', post_meta_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d53ec6f-729a-435e-bc90-4584ce67de2c', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, general_public_safety_interest).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, capital_punishment_marginal_deterrence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks and secures capital sentences, justifying the practice on the ground that the credible threat of execution suppresses the incidence of capital crimes below what life imprisonment alone would achieve. Administers charging decisions, controls which cases become capital cases, and defends the deterrence rationale in appellate and legislative fora.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% An unidentifiable, diffuse population of people who would be murdered absent the deterrent effect, if the deterrent effect is real. They cannot be named, consulted, or organized; their benefit exists only counterfactually and only to the extent the deterrence hypothesis is empirically true, which is contested.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Bear the full and irreversible cost of the constraint's operation. Under this reading their execution is instrumentally justified as the price of suppressing future crime, not as deserved punishment in itself — they are treated as a means to a population-level statistical end. Have no exit once sentenced and appeals exhausted.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% A subset of executed or condemned offenders who did not commit the crime. Under a deterrence framework their deaths generate no offsetting deterrent benefit — they are pure utilitarian loss, and the reading requires the state to minimize this error rate to preserve its own justification. Innocence-project exonerations post-execution cannot be remedied.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, local).

% Study homicide rate differentials between capital and non-capital jurisdictions, and before/after abolition natural experiments, to test whether execution produces marginal deterrence beyond life imprisonment. Decades of panel-data studies report no robust, consistent deterrent effect distinguishable from life-without-parole regimes, though the literature remains methodologically contested.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminology_researchers, observer,
    analytical, generational, analytical, national).

% Litigate against capital sentences and would argue the deterrence rationale is empirically unsupported and that life-without-parole is a substitute achieving equivalent public-safety benefit at no execution risk. Their arguments are heard in courts but rarely displace the legislative and prosecutorial commitment to the deterrence justification itself.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, capital_defense_bar, excluded,
    organized, biographical, constrained, national).

% Named for completeness: the substitute incapacitation mechanism this reading's own logic says should be preferred if it achieves equivalent deterrence at lower error cost. Not an actor; represents the counterfactual policy alternative the deterrence claim must be measured against.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, life_without_parole_regime, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_execution_authority__deterrence_reading, life_without_parole_regime).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Publicly commits the state to imposing the maximum possible cost on capital crimes, intended to raise the expected cost of committing murder above what life imprisonment alone imposes, thereby coordinating a lower equilibrium rate of capital crime across the population.
% TRANSFER_FUNCTION: Moves the risk of the ultimate penalty from an unidentifiable set of future potential victims (who are protected, if the hypothesis is true) onto a small, identifiable set of convicted offenders — including, irreducibly, some innocent people convicted in error.
% ABSENT_VOICES: The capital defense bar and academic criminologists who find no robust marginal deterrent effect are present in litigation and publication but are structurally unable to force reconsideration of the underlying legislative commitment, which is renewed independent of the empirical record.
% DISAPPEARANCE_RATIONALE: If execution authority were abolished tomorrow, the state apparatus and much of the public would say a load-bearing crime-control mechanism has been removed; the criminological consensus would say homicide rates would track pre-existing trends unchanged, since no robust marginal deterrent effect above life-without-parole has been consistently demonstrated. The two camps genuinely dispute what would happen, not merely how to feel about it.
% FOUNDING_PROBLEM: Untrammeled violent crime, especially murder, imposing intolerable costs on the public; the state needed a maximal, credible threat to suppress the incidence of the most serious crimes.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutorial and legislative bodies attest the founding problem remains live and that execution authority continues to serve it. Independent criminological research spanning decades of comparative and panel-data studies — corroboration from OUTSIDE the prosecutorial apparatus and legislatures that maintain the practice — finds no consistent, methodologically robust marginal deterrent effect distinguishable from life-without-parole, directly contesting the founding-problem justification on its own empirical terms.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).
:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the deterrence reading's own internal logic constrains it: it can only justify the marginal cost of execution over life-without-parole to the extent it demonstrates a marginal deterrent effect, and the criminological record does not robustly support one. Suppression (0.55) reflects the coercive apparatus required to carry out capital sentencing and appeals exhaustion, which is real but bounded by extensive procedural safeguards. Theater ratio (0.4) captures a meaningful share of the deterrence justification functioning as legitimating narrative rather than demonstrated causal mechanism — the rationale is invoked in legislative and prosecutorial argument at a rate that exceeds what the empirical support would warrant, and this gap widens as more studies fail to find the effect while the practice persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential future victims are declared beneficiaries but are diffuse, unidentifiable, and their benefit is entirely conditional on the truth of the deterrence hypothesis — this is a weaker and more contingent beneficiary class than in the retributive reading (where the beneficiary is a more concrete moral-order interest) or a rope (where beneficiaries are identifiable participants). Executed offenders and wrongfully convicted inmates sit at the extreme target end: trapped, immediate horizon, no exit. The asymmetry between a diffuse, contingent, unfalsifiable-in-any-single-case beneficiary class and a concrete, irreversible victim class is the structural core of why this reading requires active empirical vindication (the deterrence hypothesis) to sustain its own coordination claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unchecked violent crime) may remain partially live as a general social problem, but the specific mechanism's continued justification depends on a substitutability question this reading itself raises: if life-without-parole produces equivalent public-safety benefit, the execution-specific apparatus has outlived its distinguishing function even if the general crime-control need remains. The founding_problem_status is authored as contested precisely because prosecutorial/legislative bodies and independent criminological corroboration disagree — this is the mismatch the mandatrophy consumer is built to detect: status=contested paired with disappearance_verdict=contested, rather than a clean live/dead read, because the two attesting camps hold genuinely opposed empirical claims about the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_deterrence_effect_reality,
    'Does execution produce a marginal deterrent effect on capital crime beyond what life-without-parole achieves, or is the deterrence rationale empirically unsupported cover for a mechanism actually sustained by other (retributive, political, institutional-inertia) forces?',
    'Meta-analysis of comparative-jurisdiction and natural-experiment homicide data controlling for confounds (economic conditions, policing intensity, sentencing reform timing); resolution would require methodological consensus the field has not reached in five decades.',
    'If no marginal effect exists, the deterrence reading''s own coordination-function claim collapses and the constraint reduces to pure extraction dressed in a coordination narrative — pushing the computed type toward snare. If a robust effect is found, the tangled_rope classification is more defensible as genuine (if costly) coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrence_effect_reality, empirical, 'Whether the deterrence hypothesis this reading depends on is empirically true.').

omega_variable(
    wrongful_execution_error_rate,
    'What is the true rate of wrongful capital convictions that proceed to execution, and does that rate exceed the threshold at which the utilitarian calculus underlying this reading turns net-negative?',
    'DNA-era exoneration studies and innocence-project audits of closed capital cases, extrapolated to pre-DNA-era executions using comparable error-rate models.',
    'A high wrongful-execution rate directly undermines this reading''s own stated requirement to minimize error, since each wrongful execution is pure loss with no offsetting deterrent benefit — this pushes toward reclassifying the constraint as more extractive than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_error_rate, empirical, 'Whether the error rate is low enough for the utilitarian justification to hold on its own terms.').

omega_variable(
    reading_selection_under_determination,
    'Is the deterrence framing the dominant public justification for execution authority, or is it a secondary rationalization layered onto an arrangement primarily sustained by retributive sentiment (the sibling reading)?',
    'Content analysis of legislative debate records, prosecutorial closing arguments, and public opinion survey instruments distinguishing deterrence-based from desert-based justificatory language over time.',
    'If deterrence is secondary rationalization rather than the operative justification, this story''s claimed_type and beneficiary structure describe a minority framing rather than the constraint''s actual sustaining logic — the retributive_reading would then carry more of the real explanatory weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Which reading actually does the justificatory work in practice versus in official rhetoric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__deterrence_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__deterrence_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__deterrence_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__deterrence_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__deterrence_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__deterrence_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__deterrence_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__deterrence_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__deterrence_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% Part of the state_execution_authority kernel family (3 readings). This file (deterrence_reading) models execution as an instrumental crime-control mechanism with a diffuse future-victim beneficiary class and moderate, substitution-sensitive ε. The sibling retributive_reading models execution as deserved proportionate punishment with a moral-order beneficiary and different ε profile. The sibling abolition_reading denies the legitimacy of the mechanism categorically and would model near-total suppression/extraction with no legitimate coordination function at all. All three share the same underlying kernel (state authority to execute) but instantiate structurally distinct constraints with distinct beneficiary/victim sets and distinct ε — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
