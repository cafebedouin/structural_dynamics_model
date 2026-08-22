% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: State Killing Authority — Deterrence-Instrumental Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the deterrence-instrumental reading of the
 *   state-killing-authority kernel: capital punishment is justified strictly
 *   conditional on its crime-prevention efficacy at acceptable cost. Since
 *   Gregg v. Georgia (1976) reauthorized capital sentencing in the US,
 *   deterrence has functioned as one of the standing public justifications
 *   alongside retribution and incapacitation, but this story isolates
 *   deterrence alone, as the reading's own framework demands — if the
 *   deterrent effect does not exist, this reading's justification collapses
 *   even if retributive or incapacitative justifications survive
 *   independently (those are separate constraints). The theater_ratio climbs
 *   over the interval because the empirical case for deterrence has weakened
 *   relative to the 1970s Ehrlich-era claims (which the NAS panel and later
 *   replications substantially undermined) even as invocation of deterrence
 *   in sentencing rhetoric, political campaigns, and appellate briefing has
 *   not correspondingly declined — the gap between asserted function and
 *   evidenced function widens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.58).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.62).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "State Killing Authority — Deterrence-Instrumental Reading").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '2f817f0c-b980-4302-9d4a-a2db98fb2917').
narrative_ontology:cs_kernel_codification('2f817f0c-b980-4302-9d4a-a2db98fb2917', distributed).
narrative_ontology:cs_authority_grounding('2f817f0c-b980-4302-9d4a-a2db98fb2917', distributed).
narrative_ontology:cs_reading_relation('2f817f0c-b980-4302-9d4a-a2db98fb2917', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('2f817f0c-b980-4302-9d4a-a2db98fb2917', state_killing_authority__categorical_abolition, influences).
narrative_ontology:cs_axiom('2f817f0c-b980-4302-9d4a-a2db98fb2917', foundational, punishment_legitimacy_conditional_on_prevention_efficacy).
narrative_ontology:cs_axiom_status(punishment_legitimacy_conditional_on_prevention_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('2f817f0c-b980-4302-9d4a-a2db98fb2917', punishment_legitimacy_conditional_on_prevention_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('2f817f0c-b980-4302-9d4a-a2db98fb2917', foundational, condemned_person_as_instrumental_means_to_collective_safety).
narrative_ontology:cs_axiom_status(condemned_person_as_instrumental_means_to_collective_safety, holdable).
narrative_ontology:cs_axiom_grounding('2f817f0c-b980-4302-9d4a-a2db98fb2917', condemned_person_as_instrumental_means_to_collective_safety, instrumental).
narrative_ontology:cs_reference_frame('2f817f0c-b980-4302-9d4a-a2db98fb2917', empirical_crime_prevention_efficacy_standard).
narrative_ontology:cs_drift_state('2f817f0c-b980-4302-9d4a-a2db98fb2917', post_nas_2012_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2f817f0c-b980-4302-9d4a-a2db98fb2917', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, prosecutorial_and_political_actors_running_on_deterrence).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, wrongfully_convicted_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, capital_defense_resource_pool).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, state_authority_to_kill_for_prevention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% An unidentifiable class whose members would, on the deterrence hypothesis, not be murdered because the threat of execution changed a would-be killer's calculus. They cannot be named, consulted, or shown to exist in any specific instance; their benefit is entirely inferred from aggregate statistical models, never observed as an individual saved life.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_murder_victims, beneficiary,
    powerless, generational, analytical, national).

% Sentenced to death and used, under this reading, as the mechanism by which the deterrent signal is transmitted to future would-be killers. Their death is not justified by what they did (desert) but by what it is claimed to prevent others from doing. They have no legal exit once sentence is final except exhausted appeals and clemency, both narrow and slow.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, national).

% A subset of the condemned who did not commit the crime. Under a deterrence justification their execution produces zero deterrent benefit (there is no correction mechanism once carried out) while incurring the full instrumental cost the theory is supposed to justify — they are pure loss even by the reading's own metric.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, wrongfully_convicted_persons, payer,
    powerless, immediate, trapped, national).

% District attorneys, legislators, and executives who invoke deterrence to justify seeking or maintaining capital statutes, gaining political capital from appearing tough on crime. They administer the charging and sentencing machinery and can decline to invoke it without personal cost; their careers benefit from the deterrence narrative regardless of whether the empirical deterrent effect is real.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, prosecutorial_and_political_actors_running_on_deterrence, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, prosecutorial_and_political_actors_running_on_deterrence, agenda_setter).

% Public defenders, appellate courts, and forensic-review systems absorb enormous resources litigating capital cases across decades of appeals, resources drawn away from other defendants and other crime-prevention spending, because the instrumental justification demands procedural rigor proportionate to an irreversible penalty.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, capital_defense_resource_pool, payer,
    moderate, biographical, constrained, national).

% Produce and contest the empirical studies the entire reading's legitimacy depends on. Decades of panel-data and natural-experiment research have failed to converge on a robust, reproducible deterrent effect distinguishable from incapacitation and general punishment severity effects.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminologists_and_deterrence_researchers, observer,
    analytical, generational, analytical, national).

% Families who supported capital sentencing believing it would prevent future crimes are rarely given the actual empirical status of the deterrence claim during proceedings; sentencing narratives assert the deterrent function without exposing them to the contested state of the evidence.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, murder_victims_families_seeking_prevention, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared societal mechanism, administered by the state, that purports to reduce the future incidence of murder by imposing the maximum credible threat on prospective killers — solving a genuine coordination problem (nobody can unilaterally deter murder; only a state-backed, credible, uniformly applied threat can, on this theory, shift incentives population-wide).
% TRANSFER_FUNCTION: Moves the certainty of death from an unascertainable future murderer (who is never identified or punished for a crime not yet committed) onto a specific, currently-held person, on the theory that the transfer produces a net reduction in deaths elsewhere in the population.
% ABSENT_VOICES: Families of prospective, never-realized victims cannot be identified or asked whether the trade was worth it, because a successfully deterred murder leaves no trace. Wrongfully convicted persons whose execution registered as false-positive proof of the theory are permanently unable to testify against it. Both classes are structurally silenced by the completed act.
% DISAPPEARANCE_RATIONALE: Abolitionists argue the world would barely change — most peer-nation comparisons and within-country moratorium studies show no consistent rise in murder rates after abolition or after de facto moratoria — meaning the arrangement's own justifying premise may already be empirically false. Proponents of this reading argue that removing capital punishment would remove a real, if hard-to-isolate, marginal deterrent and predict a measurable, if small, rise in specific categories of premeditated and contract killing. The dispute is precisely over whether the deterrent effect this reading requires actually exists at detectable magnitude.
% FOUNDING_PROBLEM: Murder rates that ordinary criminal sanctions (imprisonment) were believed insufficient to deter, particularly premeditated, contract, and recidivist killings where the marginal cost of an additional life sentence to an already-imprisoned or already-doomed offender is low.
% FOUNDING_PROBLEM_CORROBORATION: The National Academy of Sciences' 2012 deterrence and death penalty panel — a body with no stake in either abolition or retention — concluded that existing studies are not informative about whether capital punishment affects homicide rates, i.e., that the founding empirical problem this reading claims to solve has not been shown to be solved by capital punishment specifically, as opposed to certainty and severity of punishment generally (which do not require execution). This is corroboration from outside both the retentionist and abolitionist advocacy communities, and it does not support the reading's own founding claim.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, contested).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58) reflects that the state extracts an irreversible cost (the condemned person's life) on the strength of a contested, arguably unsupported empirical premise; it is not maximal because a genuine coordination function (public safety) is coherently claimed and pursued in good faith by many actors. Suppression (0.62) is high because appeal avenues are narrow, execution is final and forecloses correction, and the empirical debate itself is frequently excluded from jury sentencing instructions. accessibility_collapse (0.40) and resistance (0.55) are moderate rather than extreme: unlike a mountain, real alternatives (life without parole, alternative deterrence mechanisms) are visible and litigated, and organized resistance (abolition movements, defense bar, moratoria) is active and has had material successes (state-level abolitions, execution moratoria).
 *
 * DIRECTIONALITY LOGIC:
 *   Potential future murder victims are declared beneficiaries per the reading's own logic, but they are radically diffuse and unidentifiable — the engine should treat this beneficiary class as structurally weak evidence for a low-d coordination story, since no specific transfer to a specific person can ever be verified. Condemned persons and especially wrongfully convicted persons sit at the extreme target end: trapped, powerless, and bearing a cost that is total and irreversible. Prosecutorial and political actors are beneficiaries in the reputational/career sense even though they administer rather than personally profit financially — their exit option (declining to seek death) is cheap and always available, which sharpens rather than dilutes their beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unincentivized murderers requiring maximal threat) is authored as contested rather than resolved because credible, non-partisan review (NAS 2012) finds the empirical basis for the specific deterrent claim unestablished, while proponents maintain the effect exists but is difficult to isolate statistically. This is precisely the case mandatrophy analysis is built for: an arrangement whose founding justification may have quietly failed empirical testing while the coercive machinery (execution, capital trial procedure, appellate infrastructure) persists at nearly undiminished scale. The rising theater_ratio is the diagnostic signature — increasing performative invocation of deterrence in prosecutorial and political rhetoric against a flat-to-declining evidentiary base.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_effect_existence,
    'Does capital punishment produce a marginal deterrent effect on murder rates beyond that produced by long-term imprisonment, at a magnitude detectable above confounding variables (economic conditions, policing intensity, incarceration certainty)?',
    'Convergent, replicated natural-experiment and panel-data studies isolating capital-punishment-specific effects from general sentencing-severity and certainty effects; the NAS 2012 panel''s own recommended research design improvements would need to be executed and replicated across jurisdictions.',
    'If no detectable effect exists, this reading''s own stated justification condition (''if and only if it prevents future murders'') is unmet and the reading self-negates into pure extraction from its own framework''s terms — the constraint would then structurally resemble a snare wearing coordination language. If a robust effect is found, the beneficiary class (potential future victims) gains genuine empirical grounding rather than remaining a purely theoretical, unidentifiable class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_effect_existence, empirical, 'Whether the deterrent effect this entire reading depends on actually exists at a measurable magnitude.').

omega_variable(
    wrongful_execution_rate_ambiguity,
    'What is the true rate of wrongful capital convictions carried to execution, given that post-execution exoneration is structurally near-impossible (no living defendant to pursue DNA or witness-recantation relief)?',
    'Retrospective forensic re-analysis of closed capital cases (as attempted in the Cameron Todd Willingham and Carlos DeLuna cases) extrapolated via statistical models of wrongful conviction rates in non-capital cases with comparable evidence profiles.',
    'A higher-than-assumed wrongful execution rate directly increases the instrumental cost side of the reading''s own cost-benefit framework, independent of whether deterrence is real, and could flip the ''acceptable cost'' clause even under a stipulated positive deterrent effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_rate_ambiguity, empirical, 'The unobservable true rate of wrongful executions, which the instrumental framework must weigh against any deterrent benefit.').

omega_variable(
    kernel_reading_independence,
    'Is the deterrence-instrumental reading genuinely independent of the retributive reading in practice, or does retributive sentiment functionally sustain capital statutes even where deterrence rhetoric is the stated public justification (i.e., is deterrence a post-hoc rationalization layered over an underlying retributive commitment)?',
    'Discourse analysis of legislative debate records, jury instructions, and prosecutorial closing arguments across jurisdictions to identify whether deterrence or desert language predominates and whether statutes persist in jurisdictions where deterrence claims have been publicly discredited.',
    'If deterrence is chiefly rhetorical cover for an underlying retributive commitment, this story''s classification (tangled_rope contingent on a real, contestable coordination function) would be less apt than treating the observed practice under the retributive_desert reading instead — this would not change the retributive story''s own authored values, but it would suggest the two readings are less structurally separable in practice than the ε-invariance principle assumes for the purpose of predicting real-world statute persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_independence, conceptual, 'Whether the deterrence reading operates independently in practice or as a rhetorical overlay on retributive commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__deterrence_instrument, theater_ratio, 1976, 0.25).
narrative_ontology:measurement_basis(stat_tr_t1976, observed).
narrative_ontology:measurement(stat_tr_t1985, state_killing_authority__deterrence_instrument, theater_ratio, 1985, 0.3).
narrative_ontology:measurement_basis(stat_tr_t1985, observed).
narrative_ontology:measurement(stat_tr_t1995, state_killing_authority__deterrence_instrument, theater_ratio, 1995, 0.35).
narrative_ontology:measurement_basis(stat_tr_t1995, observed).
narrative_ontology:measurement(stat_tr_t2005, state_killing_authority__deterrence_instrument, theater_ratio, 2005, 0.42).
narrative_ontology:measurement_basis(stat_tr_t2005, observed).
narrative_ontology:measurement(stat_tr_t2015, state_killing_authority__deterrence_instrument, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(stat_tr_t2015, observed).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__deterrence_instrument, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(stat_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__deterrence_instrument, base_extractiveness, 1976, 0.45).
narrative_ontology:measurement_basis(stat_be_t1976, observed).
narrative_ontology:measurement(stat_be_t1985, state_killing_authority__deterrence_instrument, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement_basis(stat_be_t1985, observed).
narrative_ontology:measurement(stat_be_t1995, state_killing_authority__deterrence_instrument, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement_basis(stat_be_t1995, observed).
narrative_ontology:measurement(stat_be_t2005, state_killing_authority__deterrence_instrument, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement_basis(stat_be_t2005, observed).
narrative_ontology:measurement(stat_be_t2015, state_killing_authority__deterrence_instrument, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement_basis(stat_be_t2015, observed).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__deterrence_instrument, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(stat_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__deterrence_instrument, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement_basis(stat_su_t1976, observed).
narrative_ontology:measurement(stat_su_t1985, state_killing_authority__deterrence_instrument, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement_basis(stat_su_t1985, observed).
narrative_ontology:measurement(stat_su_t1995, state_killing_authority__deterrence_instrument, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement_basis(stat_su_t1995, observed).
narrative_ontology:measurement(stat_su_t2005, state_killing_authority__deterrence_instrument, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(stat_su_t2005, observed).
narrative_ontology:measurement(stat_su_t2015, state_killing_authority__deterrence_instrument, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement_basis(stat_su_t2015, observed).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__deterrence_instrument, suppression_requirement, 2024, 0.62).
narrative_ontology:measurement_basis(stat_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_killing_authority kernel (retributive_desert, deterrence_instrument [this story], categorical_abolition). Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle: this reading's ε (0.58) is conditioned on the contested empirical deterrence claim and its beneficiary set includes an unidentifiable future-victim class, which the retributive reading does not need and the abolitionist reading rejects as a category. Network edges reflect that legitimacy pressure on this reading (via deterrence-evidence erosion) has structural spillover — political and legal actors often invoke retribution as fallback justification when deterrence claims weaken, and abolition arguments frequently cite deterrence-evidence failure as their strongest empirical lever.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
