% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment Justified as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the deterrence-instrument reading of the
 *   state-killing-authority kernel: capital punishment is justified strictly
 *   conditionally, if and only if it prevents future murders at acceptable
 *   cost. This is structurally distinct from the retributive reading
 *   (desert-based, backward-looking, indifferent to future crime rates) and
 *   the abolitionist reading (life is inalienable regardless of
 *   consequences). Under THIS reading, future potential victims enter the
 *   beneficiary set as the people whose lives are claimed to be saved; the
 *   condemned person becomes an instrumental cost rather than a wrongdoer
 *   receiving deserved punishment; and state authority to kill is grounded in
 *   claimed crime-prevention efficacy rather than in desert or in an inherent
 *   state prerogative. As the theater_ratio measurements show, over time the
 *   rising gap between the empirical weakness of deterrence findings and the
 *   persistence of the practice suggests the deterrence justification
 *   increasingly functions as post-hoc legitimation for an arrangement
 *   sustained by other forces (political capital, institutional inertia,
 *   retributive sentiment relabeled).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.62).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.58).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment Justified as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '0a63eee9-0c2e-4612-aaf3-c0cedf8591c2').
narrative_ontology:cs_kernel_codification('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', distributed).
narrative_ontology:cs_authority_grounding('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', distributed).
narrative_ontology:cs_reading_relation('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', foundational, punishment_justified_only_by_future_crime_prevention).
narrative_ontology:cs_axiom_status(punishment_justified_only_by_future_crime_prevention, holdable).
narrative_ontology:cs_axiom_grounding('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', punishment_justified_only_by_future_crime_prevention, empirically_contingent).
narrative_ontology:cs_axiom('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', secondary, acceptable_cost_ceiling_on_instrumental_state_killing).
narrative_ontology:cs_axiom_status(acceptable_cost_ceiling_on_instrumental_state_killing, holdable).
narrative_ontology:cs_axiom_grounding('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', acceptable_cost_ceiling_on_instrumental_state_killing, instrumental).
narrative_ontology:cs_reference_frame('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', consequentialist_penal_efficacy_standard).
narrative_ontology:cs_drift_state('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', post_nrc_2012_deterrence_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a63eee9-0c2e-4612-aaf3-c0cedf8591c2', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, public_safety_apparatus).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, prosecutorial_offices).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, wrongfully_convicted_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, capital_defense_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, murder_victims_families_seeking_deterrence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% An unidentifiable class of people whose lives are claimed to be saved by the deterrent effect of executions. They cannot be named in advance, cannot consent to or refuse the arrangement made on their behalf, and their 'benefit' is entirely counterfactual — inferred from statistical models, never observed as a rescued individual.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_victims, beneficiary,
    powerless, biographical, analytical, national).

% State prosecutors, legislatures, and executive branches administer capital statutes, decide when to seek death, and defend the deterrence rationale in courts and public debate. They control the machinery, set charging policy, and bear no personal cost if the deterrence claim turns out to be empirically false.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, public_safety_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Elected or appointed prosecutors gain leverage in plea negotiations, political capital from 'tough on crime' postures, and career advancement from securing death sentences, independent of whether any deterrent effect materializes.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, prosecutorial_offices, beneficiary,
    organized, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, prosecutorial_offices, agenda_setter).

% Individuals sentenced to death are treated as instruments whose execution is justified only by its purported effect on OTHER people's future behavior — a use of the condemned person's life as a means to an end that is separate from anything about their own culpability. They have no exit; appeals are the only recourse, and even successful appeals cannot restore years lost.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, local).

% A documented subset of the condemned population is factually innocent. Under a deterrence rationale, an irreversible execution error cannot be corrected, and the instrumental logic offers no special protection against this risk since deterrence value is claimed even for cases with residual doubt.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, wrongfully_convicted_persons, payer,
    powerless, biographical, trapped, national).

% Public defender systems and appellate courts absorb enormous resource costs litigating capital cases for decades, resources diverted from other defendants and other public needs, justified by a deterrence claim whose empirical support they argue is weak or absent.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, capital_defense_resources, payer,
    moderate, biographical, constrained, regional).

% Economists and criminologists conduct panel studies attempting to measure whether executions reduce murder rates. The empirical literature is contested: some studies find modest deterrent effects, the National Research Council's 2012 review found the evidence too weak to inform policy, and comparisons with abolitionist jurisdictions show no consistent divergence in homicide trends.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminology_researchers, observer,
    analytical, generational, analytical, national).

% Some victims' families support capital punishment specifically for its claimed deterrent value, believing executions prevent future families from suffering as they have. Their belief in the mechanism is sincere but does not itself establish that the mechanism operates.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, murder_victims_families_seeking_deterrence, beneficiary,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: If deterrence is empirically real, the arrangement coordinates a genuine collective-action problem: individuals who would murder absent the threat of execution are dissuaded, and the state internalizes an enforcement cost that individual citizens cannot bear alone (nobody can credibly threaten execution privately).
% TRANSFER_FUNCTION: Moves the condemned person's remaining life, and the state's execution and lengthy appellate-litigation resources, into a claimed reduction in future murder incidence distributed diffusely across the population — an exchange of one identified life and large state resources for a statistically inferred, never-individually-observed reduction in harm to unidentified others.
% ABSENT_VOICES: The condemned and the wrongfully convicted are structurally present as defendants but have no voice in whether the deterrence rationale itself is sound — they cannot contest the empirical premise that justifies their execution, only their individual guilt or process. Foreign jurisdictions without capital punishment that show no elevated murder rates are rarely admitted as comparative evidence in domestic capital sentencing proceedings.
% DISAPPEARANCE_RATIONALE: If capital punishment ended tomorrow, prosecutorial offices would lose a charging tool and negotiating leverage, and public safety agencies would need to justify safety claims on other grounds — a real institutional rearrangement. But whether murder rates would rise is exactly the contested empirical question the deterrence rationale depends on; if the deterrent effect is negligible (as much of the criminology literature suggests), the world for potential future victims would be unchanged, making this verdict genuinely split rather than resolvable by story-internal facts.
% FOUNDING_PROBLEM: Homicide as a social harm that legal systems seek to reduce; capital punishment specifically was proposed as a uniquely strong disincentive because of the severity and finality of the threatened penalty, more effective at the margin than life imprisonment.
% FOUNDING_PROBLEM_CORROBORATION: The National Research Council's 2012 Deterrence and the Death Penalty report and subsequent panel-data economics find the evidence for a marginal deterrent effect over long-term imprisonment too weak and methodologically fragile to support the claim; this is corroboration from an outside scientific body, not from prosecutorial offices or capital-punishment advocacy groups, who continue to assert the mechanism as live.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, contested).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that an identified person's life is taken to produce a statistically claimed, never-individually-verified benefit to an unidentified population; this is a real transfer with a genuinely uncertain benefit side. Suppression (0.58) captures the irreversibility of execution combined with the difficulty of contesting the deterrence premise itself within capital proceedings, which focus on guilt/process rather than sentencing-rationale efficacy. Theater ratio (0.55) is high and rising because the empirical case for marginal deterrence over life imprisonment has weakened under scrutiny (NRC 2012) even as the practice and its official justification persist — a classic Goodhart-style divergence between the stated function and the sustaining function. Accessibility collapse is moderate (0.4): once sentenced, the condemned person's alternatives collapse almost completely, but at the policy level alternatives (life imprisonment, abolition) remain visibly available and adopted by many peer jurisdictions, so collapse is not total at the systemic level.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential future victims are declared beneficiaries but their benefit is entirely counterfactual and unfalsifiable at the individual level, which is itself part of what makes this reading distinct and contestable — no one can point to a specific person whose life was saved. Prosecutorial offices and public safety institutions are also beneficiaries in a more concrete sense: political capital, leverage, and institutional resources flow to them regardless of the truth of the deterrence claim, which is why gain_flow is authored as diffuse rather than naming a single capturing seat — the closest thing to concentrated capture (prosecutorial advantage) is a byproduct, not the stated function's target. Condemned persons and wrongfully convicted persons sit at the extraction pole: trapped, immediate horizon, bearing the full instrumental cost. Capital defense systems pay in diverted resources across decades of appellate litigation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (homicide reduction) remains genuinely live as a policy goal, but the specific mechanism this reading depends on — that execution outperforms life imprisonment at the margin — has substantial outside corroboration (NRC 2012, panel-data criminology) suggesting the mechanism itself may be largely inert while the institutional practice persists. This is exactly the founding_problem_status=contested + disappearance_verdict=contested pattern the R5 mismatch check is designed to surface: if the deterrent mechanism is dead but the practice persists on other grounds (retribution relabeled as deterrence, political incentives), that is a capture/zombie signature distinct from a genuine live-function reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_status,
    'Does capital punishment produce a measurable marginal deterrent effect over long-term imprisonment, sufficient to satisfy this reading''s own conditional (justified IF AND ONLY IF it prevents future murders at acceptable cost)?',
    'Longitudinal panel-data studies comparing homicide rates across matched jurisdictions with and without capital punishment, controlling for confounds; NRC-style meta-review of the accumulated econometric literature.',
    'If the deterrent effect is negligible or unmeasurable, this reading''s OWN stated justification fails on its own terms, converting the practice from a conditionally-justified instrument into an unjustified one by the reading''s own criterion — this is the single most consequential empirical fact for this specific reading, though it does not resolve the sibling readings which do not depend on deterrence at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Whether the deterrence mechanism this reading depends on actually operates.').

omega_variable(
    acceptable_cost_threshold,
    'What cost (in wrongful executions, resource diversion, or moral cost of instrumentalizing a person) counts as ''acceptable'' under this reading''s own conditional, and who has authority to set that threshold?',
    'No empirical resolution is possible; this is a value question about how many wrongful executions or how much resource diversion offsets a given number of statistically-inferred lives saved. Different framings (utilitarian aggregation vs. rights-constrained side-limits) yield different answers.',
    'A low acceptable-cost threshold would render even a confirmed modest deterrent effect insufficient to justify the practice under this reading; a high threshold would sustain the practice even against weak deterrence evidence. The reading''s coherence depends on this threshold being specifiable, and it may not be.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(acceptable_cost_threshold, preference, 'The undefined cost-acceptability threshold internal to the deterrence conditional.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does adopting the deterrence-instrument reading as a jurisdiction''s stated legal rationale foreclose retributive_desert as a simultaneously operative rationale within the same sentencing framework, or can both coexist as parallel justifications offered for the same statute?',
    'Doctrinal analysis of actual capital sentencing statutes and appellate opinions to determine whether courts treat deterrence and retribution as independently sufficient (either alone justifies the penalty) or as jointly necessary.',
    'If jurisdictions treat the two rationales as independently sufficient, the constraints coexist rather than compete, meaning a failure of the deterrence rationale would not by itself invalidate the practice, since the retributive rationale remains available as a backstop — this matters directly for interpreting what a negative answer to the deterrence_empirical_status omega would actually change in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether deterrence and retributive rationales function as independent or joint justifications in actual legal practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__deterrence_instrument, theater_ratio, 8, 0.38).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__deterrence_instrument, theater_ratio, 16, 0.44).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__deterrence_instrument, theater_ratio, 24, 0.49).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__deterrence_instrument, theater_ratio, 32, 0.52).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__deterrence_instrument, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__deterrence_instrument, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__deterrence_instrument, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__deterrence_instrument, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__deterrence_instrument, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__deterrence_instrument, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__deterrence_instrument, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__deterrence_instrument, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the state_killing_authority kernel. deterrence_instrument (this story) conditions justification on empirical crime-prevention efficacy; retributive_desert grounds justification in backward-looking proportional desert independent of future consequences; categorical_abolition rules out state killing regardless of any claimed benefit or desert. Each reading has its own ε, beneficiary/victim structure, and classification — they are not the same constraint viewed three ways but three structurally distinct constraints sharing one contested kernel (the state's claimed authority to kill).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
