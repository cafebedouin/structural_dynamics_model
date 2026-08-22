% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Execution as Deterrence Signal (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   The deterrence reading of capital punishment justifies execution as a
 *   rational signal preventing future murders. Under this reading, the state
 *   instrumentalizes the condemned offender as the means by which the
 *   deterrent effect flows to potential future victims. This is ONE reading
 *   of the contested kernel of state killing legitimacy—a kernel that admits
 *   at least three incompatible readings: deterrence (this constraint),
 *   retribution (proportional desert), and abolition (categorical
 *   impermissibility). The deterrence reading claims moderate extractiveness
 *   because empirical support for the deterrent effect is substantially
 *   contested; the measured suppression is high because abolition advocates
 *   must be excluded from the sentencing framework for the reading to hold,
 *   and because capital punishment requires active enforcement machinery to
 *   maintain the credible threat. The theater ratio reflects the growing gap
 *   between the procedural apparatus (appeal processes, execution protocols)
 *   and the deterrent function—executions have become increasingly ritualized
 *   while evidence for their deterrent effect has weakened.
 *
 * KEY AGENTS:
 *   - Potential future victims: the unidentified beneficiary class whose murders are (under this reading) prevented.
 *   - Executed offenders: powerless, trapped agents who bear the cost and are instrumentalized as the signal mechanism.
 *   - State execution apparatus: institutional agenda-setter that maintains the legal and procedural infrastructure.
 *   - Murder victims' families: moderate-power agents who occupy a dual position (symbolic beneficiary via closure, but also payer if they oppose capital punishment morally).
 *   - Empirical criminologists: observers whose contested findings directly challenge the axiom that execution deters murder.
 *   - Abolition advocates: excluded from the sentencing framework; would argue categorical impermissibility regardless of deterrent effect.
 *   - Retributive justice advocates: observers whose reading of the same institutional practice diverges on the justifying principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.62).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.71).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Execution as Deterrence Signal (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'c8954d36-7984-4b6d-a1a7-4dfb27b3677b').
narrative_ontology:cs_kernel_codification('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', formalized).
narrative_ontology:cs_authority_grounding('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', extraction).
narrative_ontology:cs_interpretation_layer_present('c8954d36-7984-4b6d-a1a7-4dfb27b3677b').
narrative_ontology:cs_reading_relation('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', foundational, execution_prevents_future_murders).
narrative_ontology:cs_axiom_status(execution_prevents_future_murders, holdable).
narrative_ontology:cs_axiom_grounding('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', execution_prevents_future_murders, empirically_contingent).
narrative_ontology:cs_axiom('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', foundational, state_may_instrumentalize_offender_for_public_benefit).
narrative_ontology:cs_axiom_status(state_may_instrumentalize_offender_for_public_benefit, holdable).
narrative_ontology:cs_axiom_grounding('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', state_may_instrumentalize_offender_for_public_benefit, deontological).
narrative_ontology:cs_reference_frame('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', rational_deterrence_framework).
narrative_ontology:cs_drift_state('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c8954d36-7984-4b6d-a1a7-4dfb27b3677b', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, executed_offenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, murder_victims_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, murder_victims_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unidentified future people whose murders are (under this reading) prevented by the deterrent effect of execution. They benefit from the constraint insofar as the credible threat of capital punishment reduces homicide rates. They have no seat at the sentencing table and cannot negotiate the terms of their protection.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, biographical, trapped, national).

% Individuals convicted of murder and sentenced to death. Under the deterrence reading, they are instrumentalized as the means by which the deterrent signal is created and transmitted to potential future killers. Their death serves a purpose beyond proportional punishment—it is justified by its hypothesized preventive effect on strangers.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% The legal and executive machinery that administers capital punishment: courts, legislatures, correctional systems, and the procedural framework that transforms the deterrence justification into a lived institutional practice. Sets and enforces the rule; operationalizes the deterrent signal.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Families of murdered persons. They benefit from the symbolic closure and retributive satisfaction the deterrence framework offers (their loved one's death is answered by the offender's). They also may carry psychological costs of witnessing or participating in the execution process. Some families oppose capital punishment on moral grounds despite their loss.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, murder_victims_families, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, murder_victims_families, payer).

% Researchers investigating whether execution actually deters murder. Their findings are contested: some studies show modest deterrent effects; others find no detectable effect controlling for confounders. The empirical status of the foundational axiom—that execution prevents future murders—remains deeply uncertain.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, empirical_criminologists, observer,
    powerful, generational, analytical, global).

% Hold the view that state killing is categorically impermissible regardless of deterrent effect. They would argue that instrumentalizing offenders as means to protect strangers violates human dignity and that alternative enforcement mechanisms (life imprisonment, certainty of punishment) can achieve public safety without execution. They are excluded from capital sentencing decisions in death-penalty jurisdictions.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolition_advocates, excluded,
    organized, generational, constrained, global).

% Hold that execution is justified by the offender's desert (proportional punishment for murder) rather than by its deterrent effect. They argue the deterrence reading instrumentalizes offenders in a way that conflicts with desert-based justice. Their reading of the same institutional practice diverges fundamentally on what justifies the outcome.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, retributive_justice_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, state_execution_apparatus).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a credible threat of execution as a rational signal to potential future murderers: if the cost (death) is sufficiently salient and reliably imposed, the expected utility of committing murder decreases, reducing its frequency. The arrangement coordinates on a common understanding that state killing is an institutionalized tool for altering the payoff structure of violence.
% TRANSFER_FUNCTION: Moves the life of the executed offender—from the offender (who forfeits it under due process) to potential future victims (who are protected by the deterrent effect). The offender's death is the instrument by which protection flows to strangers. In this framing, the offender pays with certainty; strangers gain benefit with probability.
% ABSENT_VOICES: The executed offender has no voice in determining whether they will serve as a deterrent instrument; their perspective on being instrumentalized is structurally excluded from the sentencing framework under the deterrence reading. Abolition advocates are excluded from legislatures and courts in most death-penalty jurisdictions. International human rights bodies (UN, European courts) are excluded from enforcement in sovereignties that retain capital punishment.
% DISAPPEARANCE_RATIONALE: If capital punishment disappeared overnight, the deterrent signal would collapse; under the deterrence reading, murder rates would be expected to rise as the cost-benefit calculus of potential offenders shifted. (Empirically, this claim is contested, but the reading asserts it.) The arrangement's removal would fundamentally alter the incentive structure the reading claims prevents homicide.
% FOUNDING_PROBLEM: High rates of murder in societies with capital punishment; the problem framed as: how can the state reduce future murders through rational incentive engineering? The deterrence reading grounds capital punishment in an instrumental answer to that problem.
% FOUNDING_PROBLEM_CORROBORATION: Criminological evidence on deterrence is mixed and disputed. Some research (Ehrlich, 1975 onward) claims modest deterrent effects; other research (meta-analyses controlling for model specification and confounders) finds negligible or zero detectable effect. Death-penalty jurisdictions assert the deterrent effect as a persistent justification; abolitionist jurisdictions and international bodies assert the effect is not supported by evidence. Corroboration from outside the executing-state beneficiaries is weak: European abolition, meta-analytic criminology, and international human rights consensus argue the founding problem is either not solved or solvable through alternative means.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The deterrence reading claims tangled_rope because it asserts both coordination (solving the public safety problem through rational incentive engineering) AND extraction (using the offender's death as a means to protect strangers). Extractiveness is moderate (0.62) rather than high because the justification hinges on contested empirical claims; if the deterrent effect is real, the extraction is the price of coordination. Suppression is high (0.71) because the reading's persistence requires excluding abolition advocates from the institutional conversation and actively defending the credible threat against competing understandings. Theater ratio is moderate-high (0.48) and rising: the procedural machinery around executions (appellate review, due process protocols, clemency procedures) has grown more elaborate while evidence for deterrent effect has plateaued or declined, suggesting increasing proportional energy spent on theatrical legitimation rather than actual deterrence. The measurement series tracks a leveling-off pattern: extractiveness rises early (as institutional practice hardens) then stabilizes when it reaches institutional saturation; theater rises throughout as the gap between procedure and outcome grows.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus seat, the constraint is a necessary coordination mechanism: rational deterrence prevents murders and protects the public. From the executed offender's seat, the same structure is a mechanism for instrumentalizing them without their consent, justified by empirical claims they have no power to contest. From the abolition advocate's seat, the constraint is categorically illegitimate regardless of empirical deterrent effect, because it violates the offender's human dignity. From the retributive seat, execution is justified on different grounds (desert, not deterrence), which would support the constraint's operation but undermine its specific justification. The engine should compute different type classifications from each seat: the state apparatus and potential victims should compute rope or tangled_rope (coordination with extraction); the executed offender should compute snare (extraction with coercive machinery); the abolition advocate's external view should compute snare or piton (no real coordination, mostly theater). These divergences are not errors—they are the measurement the system exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential future victims occupy the beneficiary seat (d near 0.0): they receive protection at no direct cost, though the protection is probabilistic and conditional on the deterrent effect being real. Executed offenders occupy the payer seat (d near 1.0): they pay with certainty (their life) for a benefit (public safety) that accrues to strangers. The state apparatus occupies the agenda-setter seat (d moderate, ~0.4-0.5): it administers the constraint and extracts legitimacy from maintaining the framework, but faces growing political cost as empirical support weakens. Murder victims' families occupy an ambiguous dual position: they benefit from symbolic closure and retributive satisfaction, but also pay psychological costs and may hold contrary moral commitments. Abolition advocates are excluded from the decisional framework entirely. The directionality derivation is straightforward: the reading's beneficiaries are numerous but diffuse and prospective (future victims); the victims are concentrated and present (the condemned); suppression is high because the reading requires excluding alternatives from institutional consideration.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint classifies as tangled_rope because it claims to solve a real problem (future murders) through coordination (rational deterrence) while extracting from the condemned. However, mandatrophy risk is high: if empirical evidence conclusively shows execution does NOT deter murder, the coordination claim collapses, and the constraint becomes pure snare. The theater ratio rising to 0.48 while extractiveness plateaus suggests the constraint is already experiencing the early signals of mandatrophy—the procedural apparatus is elaborating (appellate review, due process) while the outcome is decoupling from the justifying function. An omega variable below addresses this empirical vulnerability directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_effect_empirical_truth,
    'Does execution actually reduce the murder rate relative to alternative punishments (e.g., life imprisonment with certainty)?',
    'Systematic meta-analysis of randomized or quasi-experimental evidence controlling for model specification, omitted variables, and publication bias. Jurisdictional natural experiments (abolitionist transitions in developed economies with low subsequent murder-rate increases).',
    'If deterrent effect is negligible or zero, the coordination justification collapses and the constraint becomes pure snare: offenders are killed for a benefit that does not materialize. If deterrent effect is substantial, the constraint remains tangled_rope. This omega directly determines whether the reading is coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_effect_empirical_truth, empirical, 'Whether execution prevents future murders, the foundational claim of the deterrence reading.').

omega_variable(
    kernel_contest_framing,
    'Is the contest between deterrence, retribution, and abolition a contest between three readings of one kernel (same institution, different justifications) or three separate claims about whether the institution should exist at all?',
    'Conceptual analysis: do all three readings accept execution as the institutional practice and disagree only on what justifies it? (If yes, they are readings of one kernel.) Or do some readings reject execution entirely? (If yes, it is unclear whether they are readings of the same kernel or different claims.)',
    'If the contest is among readings of one kernel, the three constraints should be authored as a family linked by network.affects_constraints, each with a different cs_structure. If the contest is between claims about whether execution should exist, the framing changes and some constraints may not be readable as kernels at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'Whether the three readings are interpretations of a single institutional practice or incompatible claims about its legitimacy.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.71) structural (legal barriers excluding abolition advocates from sentencing decisions, procedural constraints on alternative frameworks) or internalized (abolition advocates have internalized acceptance of deterrence framing such that they no longer mount effective resistance)?',
    'Examine the intensity and scope of resistance movements in death-penalty jurisdictions: high, organized, persistent resistance indicates structural suppression (barriers are externally enforced); decline in resistance over time while structural barriers persist indicates growing internalization. Post-abolition survey data from jurisdictions that have eliminated capital punishment show whether suppression was structural (resistance surges when barriers are lifted) or internalized (resistance remains muted after institutional change).',
    'If suppression is structural, the constraint''s persistence depends on active enforcement machinery and could change rapidly if political conditions shift. If suppression is internalized, the constraint carries psychological durability even if formal barriers were removed—the reading has become hegemonic. This affects the exit-options classification and the effective extraction from abolition advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of abolition voices is structural or internalized.').

omega_variable(
    beneficiary_temporal_asymmetry,
    'How should the framework model the beneficiary class (potential future victims) when the benefit is prospective, probabilistic, and distributed across an unidentified population? Does this asymmetry render the coordination frame incoherent relative to extraction concentrated on identified, present agents?',
    'Comparison with other constraints where coordination benefits are prospective and diffuse (e.g., infrastructure investment, public health measures) versus constraints where benefits are identified and present. Philosophical analysis of whether rational coordination can be justified by benefits to unidentified future persons when costs are borne by identified present agents.',
    'If the framework concludes prospective benefits to unidentified populations cannot ground legitimate extraction from identified present agents, the deterrence reading collapses into pure snare. If the framework allows it, the constraint remains tangled_rope but with unusual structural properties. This omega addresses a coherence gap in the reading itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_temporal_asymmetry, conceptual, 'Whether the deterrence reading can coherently justify extraction from present agents by appeal to benefits for unidentified future persons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__deterrence_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__deterrence_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__deterrence_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__deterrence_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__deterrence_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__deterrence_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__deterrence_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__deterrence_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__deterrence_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__deterrence_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__deterrence_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__deterrence_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__deterrence_reading, 0.18).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family decomposing the kernel 'state_killing_legitimacy.' Each reading instantiates a different constraint with a different ε, beneficiary structure, and cs_structure. The deterrence reading (this story) justifies execution by its hypothesized preventive effect; the retributive reading justifies it by proportional desert; the abolition reading rejects it categorically on dignity grounds. These are not different perspectives on the same constraint—they are structurally distinct claims about the same institutional practice. Each reading has its own ε-invariance profile. The network links them as a family because they contest the same kernel and influence each other's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
