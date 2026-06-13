% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: State Killing Authority as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel
 *   'state killing authority': the deterrence-instrument reading, which
 *   grounds capital punishment justification in crime-prevention efficacy.
 *   The constraint claims that execution is justified if and only if it
 *   prevents future murders at acceptable cost. Future potential victims
 *   (lives saved) enter the beneficiary set; condemned persons become
 *   instrumental costs in a prevention calculus. This is structurally
 *   distinct from the retributive reading (desert and proportionality) and
 *   the categorical abolition reading (life is inalienable). The kernel
 *   itself — state killing authority — is the persisting commitment; the
 *   three readings offer competing interpretations of when and why that
 *   commitment is legitimate. This story models the deterrence reading as a
 *   tangled_rope: a genuine coordination function (reducing homicide through
 *   deterrence) paired with asymmetric extraction (condemning persons bear
 *   the cost, unknown future potential victims reap the benefit). The
 *   constraint's persistence depends on empirical claims about deterrence
 *   that remain contested across 60+ years of research.
 *
 * KEY AGENTS:
 *   - potential_future_murder_victims — Beneficiary (lives saved via deterrence); powerless, trapped (they cannot organize or refuse the benefit)
 *   - condemned_persons — Payer (death); powerless, trapped (execution is non-negotiable)
 *   - state_criminal_justice_apparatus — Agenda-setter (administers execution and adjudication); institutional, mobile (can change policy if empirical evidence shifts)
 *   - victims_families_of_murdered_persons — Fractured seat (some seek execution for closure, others oppose perpetuation of violence); moderate power, constrained exit
 *   - criminal_law_scholars_empiricists — Observer (arbiters of deterrence evidence); institutional, analytical (no stakes in moral/political dispute)
 *   - death_penalty_abolition_movement — Excluded (categorical opposition to state killing); organized, constrained
 *   - retributive_justice_advocates — Excluded (ground in desert, not deterrence); moderate, constrained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.68).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.72).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "State Killing Authority as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '518c1ede-b8cc-41af-bbe4-153a17a17f45').
narrative_ontology:cs_kernel_codification('518c1ede-b8cc-41af-bbe4-153a17a17f45', distributed).
narrative_ontology:cs_authority_grounding('518c1ede-b8cc-41af-bbe4-153a17a17f45', extraction).
narrative_ontology:cs_reading_relation('518c1ede-b8cc-41af-bbe4-153a17a17f45', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_reading_relation('518c1ede-b8cc-41af-bbe4-153a17a17f45', state_killing_authority__retributive_desert, influences).
narrative_ontology:cs_axiom('518c1ede-b8cc-41af-bbe4-153a17a17f45', foundational, prevention_efficacy_sufficient_justification).
narrative_ontology:cs_axiom_status(prevention_efficacy_sufficient_justification, holdable).
narrative_ontology:cs_axiom_grounding('518c1ede-b8cc-41af-bbe4-153a17a17f45', prevention_efficacy_sufficient_justification, empirically_contingent).
narrative_ontology:cs_axiom('518c1ede-b8cc-41af-bbe4-153a17a17f45', secondary, state_instrumentality_of_condemned_life_permissible).
narrative_ontology:cs_axiom_status(state_instrumentality_of_condemned_life_permissible, holdable).
narrative_ontology:cs_axiom_grounding('518c1ede-b8cc-41af-bbe4-153a17a17f45', state_instrumentality_of_condemned_life_permissible, deontological).
narrative_ontology:cs_reference_frame('518c1ede-b8cc-41af-bbe4-153a17a17f45', crime_prevention_efficacy_standard).
narrative_ontology:cs_drift_state('518c1ede-b8cc-41af-bbe4-153a17a17f45', contemporary_meta_analytic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('518c1ede-b8cc-41af-bbe4-153a17a17f45', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_murder_victims).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, death_row_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, victims_families_of_murdered_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, victims_families_of_murdered_persons).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, consequentialist_crime_prevention_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, state_instrumentality_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who will not be murdered because of deterrence effect of executions they do not know occurred. Their benefit is purely counterfactual, dependent on the empirical deterrence claim. They cannot organize, consent, or withdraw from the arrangement.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_murder_victims, beneficiary,
    powerless, biographical, trapped, national).

% Death row inmates sentenced to execution. Under this reading, their death is instrumentalized for prevention benefit to others, not for desert or rights restoration. They are typically poor, black, or mentally ill; they have exhausted appeals and face certain execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Courts, prosecutors, legislatures, and executioners who operate the capital punishment system. They set sentencing policy, carry out sentences, and can change policy if empirical evidence shifts. Their direct benefit is limited; the stated benefit accrues to potential future victims.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_criminal_justice_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Families of murder victims. Some find closure in execution independent of deterrence (beneficiary function). Others see execution as perpetuating violence and reject it (payer function). The deterrence reading instrumentalizes their loss as evidence of deterrence failure, not as a basis for healing.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, victims_families_of_murdered_persons, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, victims_families_of_murdered_persons, beneficiary).

% Academics and researchers studying deterrence effects. They are the primary arbiters of whether the empirical premise of this reading is true. Recent meta-analyses (National Research Council, Donohue & Wolfers) find no reliable deterrent effect.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminal_law_scholars_empiricists, observer,
    institutional, generational, analytical, national).

% Organized activists and parties opposing capital punishment on categorical grounds (life is inalienable; state killing is inherently impermissible). Their voice — that some things are not legitimately traded for prevention — is structurally excluded from the deterrence reading's framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, death_penalty_abolition_movement, excluded,
    organized, generational, constrained, national).

% Parties who ground capital punishment in desert and proportionality (lex talionis) rather than prevention. They would argue execution is justified because the murderer forfeited their right to life, independent of deterrence consequences. The deterrence reading brackets their argument.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, retributive_justice_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, state_criminal_justice_apparatus).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deters potential murderers from committing murder by raising the expected cost (execution) above the expected benefit, thus reducing homicide incidence at the national level.
% TRANSFER_FUNCTION: Transfers the condemned person's life from them to a prevention calculus: their death is valued as a deterrent signal to potential future murderers, not as punishment they deserved or restoration to victims' families.
% ABSENT_VOICES: Death penalty abolitionists (who argue life is inalienable and execution is inherently impermissible) and retributivists (who argue desert, not deterrence, is the legitimate ground for execution) are excluded from the seat arrangement. Victims' families are partially included but fractured — some support execution for closure, others oppose it as perpetuating violence.
% DISAPPEARANCE_RATIONALE: If the deterrence justification and its enforcement vanished, states would either abolish capital punishment (as most developed democracies have) or would require alternative grounds (desert, incapacitation). Homicide rates would depend on whether deterrence was empirically real: if real, removal would increase murders; if false (preponderant evidence suggests this), removal would have no effect on murder rates.
% FOUNDING_PROBLEM: Murderers continue to kill because the expected penalty (imprisonment) is insufficient to deter the marginal actor; raising the penalty to execution reduces the marginal murderer's incentive to kill, thus saving lives of future potential victims.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutors and justice officials assert that deterrence is live and that murder rates would rise if capital punishment were abolished. Empirical scholars (National Research Council 2012, Donohue & Wolfers 2005) find no reliable evidence of deterrent effect; abolitionist jurisdictions (EU, Canada, Australia) show no spike in homicide after abolition. The founding problem's empirical premise is contested by the most rigorous external evidence.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).

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
 *   Extractiveness measures 0.68 at interval end because the constraint transfers the condemned person's life to a prevention calculus independent of desert, consent, or proportionality — a substantial asymmetry. Suppression is high (0.72) because the constraint's persistence depends on: (1) silencing the condemned person's objection that they are being killed not for what they did but for what the state hopes it will prevent others from doing; (2) excluding retributivists and abolitionists from the seat arrangement; (3) managing victims' families whose interests fractionalizes — some use execution as closure, others see it as perpetuation. Theater ratio rises from 0.28 to 0.41 over 40 time units because: the constraint's empirical premise (deterrence effect) has become increasingly contested while the political and prosecutorial practice of execution continues; the measured performance of the constraint's stated function (deterrence) diverges from its actual operation (signaling state power, managing victims' trauma, incapacitating offenders). The trajectory flatlines after time-point 25, reflecting a steady state where execution practice persists despite weakening empirical justification — a piton-ward dynamic, though the active suppression required to maintain the deterrence frame keeps it from full piton classification. The shared time grid ensures every metric is authored at every examined point; no metric is missing from any row.
 *
 * PERSPECTIVAL GAP:
 *   The state criminal justice apparatus sees the constraint as legitimate coordination when deterrence evidence is positive or inconclusive; condemned persons see themselves as sacrificial to an unproven causal claim about future prevention; potential future victims (the stated beneficiaries) are not present to dispute their inclusion or to verify the benefit actually accrues to them; empiricists see the deterrence premise as increasingly indefensible; abolitionists reject the entire frame (state killing is impermissible regardless of consequences); retributivists accept execution but for desert reasons independent of deterrence. The engine computes directionality per-seat from this structural data: condemned persons sit at high d (targeted, powerless, trapped, instrumental cost); potential future victims sit at low d (stated beneficiaries, though their actual benefit is counterfactual and unverifiable); the state apparatus sits near 0.5 (it executes the constraint and could change it, but does not benefit from executions directly — benefit accrues to absent parties).
 *
 * DIRECTIONALITY LOGIC:
 *   Condemnned persons are targets: they are powerless (capital defendants are typically poor, black, or mentally ill), trapped (no exit from a death sentence), and their lives are instrumentalized for a state goal. Their directionality d approaches 1.0 (full target). Potential future murder victims are nominal beneficiaries: the constraint's justification posits their lives as saved via deterrence. However, they are powerless (cannot organize), trapped (cannot refuse the benefit), and most critically, their benefit is COUNTERFACTUAL — it depends on empirical claims about deterrence that remain disputed. If deterrence is false (the preponderance of scholarship suggests it is), their inclusion as beneficiaries is a false-summit fraud. This ambiguity is the core omega. The state apparatus sits near d=0.5 (symmetric): it administers the constraint and could change it, but the direct benefit (deterrence) accrues to the stated beneficiaries, not to the state. The actual benefit to the state is political (signaling commitment to 'tough on crime'), which is secondary. Victims' families bifurcate: those who support execution get some closure benefit (low d within beneficiary camp), those who oppose get further trauma (high d within payer camp). No directionality override is needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading sits on a collapsing empirical premise. The National Research Council meta-analysis (2012) and Donohue & Wolfers' comprehensive review (2005) find no reliable deterrent effect of capital punishment — the two highest-quality evidence syntheses in the field. Yet the practice persists. This is the mandatrophy signature: the founding problem (murderers aren't deterred enough by imprisonment) was addressed by a policy (execution) whose efficacy is now scientifically indefensible, yet the policy continues because: (1) politicians can say they are 'tough on crime' without bearing the empirical burden; (2) victims' families find closure in execution independent of deterrence; (3) the state's reputation for enforcement depends on carrying out sentences. The constraint has become a Snare wearing deterrence language: it extracts the condemned person's life for signaling value, not for crime prevention. The theater ratio's rise to 0.41 reflects this degradation — the constraint's stated function (deterrence) is increasingly performative, while its actual function (incapacitation, signaling, victims' closure) operates below the stated justification. Abolition jurisdiction comparisons (EU, Canada, Australia post-1973) show no spike in homicide after capital punishment removal, directly contradicting the deterrence premise's foundational claim. Yet states that retain capital punishment do so by treating the deterrence empirics as background noise — suppression operates here to manage the cognitive dissonance between stated justification and empirical reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_falsity,
    'Does capital punishment actually deter murder? Does the empirical evidence support the deterrence premise on which this reading''s justification rests?',
    'Meta-analysis of econometric and criminological studies controlling for confounders; natural experiment from abolition jurisdictions (EU, Canada, Australia) comparing pre- and post-abolition homicide rates against matched non-abolition jurisdictions.',
    'If deterrence is empirically false, this reading loses its justification entirely — the condemned person is killed for a prevention benefit that does not accrue. The constraint would reclassify from tangled_rope (coordination + extraction) to pure snare (extraction with false coordination narrative). If deterrence is true, the constraint remains justified at its current extraction level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_empirical_falsity, empirical, 'Whether the deterrence empirical premise is true or false. The preponderance of contemporary evidence suggests false (National Research Council 2012, Donohue & Wolfers 2005), yet the policy persists.').

omega_variable(
    future_victim_beneficiary_status,
    'Can potential future murder victims coherently be the beneficiaries of a constraint? Are they present enough to count as beneficiaries, or is their inclusion a placeholder for ''public safety'' that obscures the actual extraction from condemned persons?',
    'Philosophical analysis of beneficiary standing (must beneficiaries be identifiable? present? capable of consent?). Empirical check: do actual homicide rates shift when execution occurs, or is the benefit purely rhetorical?',
    'If potential future victims are incoherent as beneficiaries (non-identifiable, non-present, non-consenting), then the constraint has NO genuine beneficiary set and should reclassify as pure snare. If they are coherent beneficiaries, the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_victim_beneficiary_status, conceptual, 'Whether future potential victims can be counted as beneficiaries of a constraint that kills present people.').

omega_variable(
    instrumental_vs_desert_grounding,
    'Is the deterrence reading''s grounding of state authority in crime-prevention efficacy the legitimate basis, or is desert (retributive_desert reading) or rights (categorical_abolition reading) the deeper justification that the state is actually using?',
    'Examination of sentencing patterns, clemency decisions, and prosecutorial charging choices: do they follow deterrence logic (target high-profile cases for signaling) or desert logic (proportional to crime severity)? If patterns show desert logic, the reading is descriptively false.',
    'If the state is actually grounding in desert while claiming deterrence, the deterrence reading is a false-summit fraud: a retributive constraint wearing deterrence language. If grounded in deterrence, the reading is structurally honest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_vs_desert_grounding, empirical, 'Whether the state''s actual authority grounding is deterrence or desert.').

omega_variable(
    suppression_mechanism_internalized_or_structural,
    'Is the suppression measured in this constraint (0.72) structural (legal barriers to expressing abolitionist views, exclusion of certain voices from courts) or internalized (prosecutors and judges who have absorbed the deterrence frame as legitimate, victims'' families who suppress their own ambivalence about closure)?',
    'Post-abolition trajectory: if suppression persists in jurisdictions that have abolished capital punishment, it indicates internalization. If suppression recedes, it indicates structural origin.',
    'If suppression is internalized, the constraint carries more effective extractiveness than the scalar suggests — the targeted persons carry the suppression with them even after exit. If structural, removal of legal barriers would reduce suppression directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_or_structural, empirical, 'Whether the suppression of abolitionist voices and condemned persons'' objections is structural or internalized.').

omega_variable(
    reading_foreclosure_relationship,
    'Does the deterrence reading foreclose the categorical_abolition reading within a single framework, or do they coexist as live positions?',
    'Logical analysis: the deterrence reading asserts state killing is permissible if prevention conditions are met; abolition asserts it is impermissible regardless. These are contradictory IF grounded in the same framework (e.g., utilitarian welfare maximization). But they can coexist if grounded in different frameworks (utilitarian vs. rights-based). Empirically: both readings are held by different contemporary parties.',
    'If foreclosed, the relationship should be marked forecloses. If coexisting, coexists_with is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_relationship, conceptual, 'Whether the deterrence and abolition readings are logically contradictory or merely politically opposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, state_killing_authority__deterrence_instrument, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__deterrence_instrument, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, state_killing_authority__deterrence_instrument, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__deterrence_instrument, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t25, state_killing_authority__deterrence_instrument, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__deterrence_instrument, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, state_killing_authority__deterrence_instrument, base_extractiveness, 5, 0.59).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__deterrence_instrument, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, state_killing_authority__deterrence_instrument, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__deterrence_instrument, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t25, state_killing_authority__deterrence_instrument, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__deterrence_instrument, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t5, state_killing_authority__deterrence_instrument, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__deterrence_instrument, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t15, state_killing_authority__deterrence_instrument, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__deterrence_instrument, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t25, state_killing_authority__deterrence_instrument, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__deterrence_instrument, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__deterrence_instrument, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).

% DUAL FORMULATION NOTE:
% The kernel state_killing_authority has three structurally distinct readings: (1) deterrence_instrument (this constraint) — justified by crime prevention; (2) retributive_desert — justified by proportional desert; (3) categorical_abolition — inherently unjustified regardless of consequence. Each reading produces a different constraint with different beneficiary/victim structures, different extracted values, and different grounding of state authority. The three are linked as siblings of the same kernel. The deterrence reading influences both siblings: it reframes the question from 'is execution deserved?' (retribution) to 'does it prevent harm?' (deterrence), shifting the empirical basis and stakes. The abolition reading forecloses the deterrence reading if deterrence is empirically false, because the permissibility condition (prevention of future murders) would be unmet.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
