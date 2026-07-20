% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Capital Punishment Deterrence Justification
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the deterrence reading of the
 *   state_killing_legitimacy kernel. Under this reading, execution is
 *   justified as a rational signal that prevents future murders by making the
 *   cost of homicide exceed its perceived benefit. The condemned offender is
 *   instrumentalized as a means to a social endâpublic securityârather
 *   than treated as an end in themselves. Potential future victims
 *   (represented by the general public) are the declared beneficiaries. The
 *   empirical evidence for a marginal deterrent effect beyond non-lethal
 *   sanctions is contested, producing a moderate extraction profile: the
 *   coordination claim is structurally present but empirically unstable,
 *   while the extraction (the offender's life) is total and terminal.
 *
 * KEY AGENTS:
 *   - State execution apparatus (agenda_setter): administers and enforces capital punishment, derives institutional authority from the monopoly on legitimate lethal force.
 *   - General public (beneficiary): receives claimed security benefits of deterrence, cannot opt out of the state's penal framework.
 *   - Condemned offenders (payer): bear terminal extraction, are physically trapped, and are procedurally excluded from policy discourse.
 *   - Abolitionist advocates (excluded): seek to end capital punishment on moral and empirical grounds; marginalized in retentionist jurisdictions.
 *   - Criminologists (observer): study deterrence empirically; their null findings are often contested by retentionist policymakers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.6).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.65).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment Deterrence Justification").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'da01f6d0-04b7-4509-8594-c1e26fae077a').
narrative_ontology:cs_kernel_codification('da01f6d0-04b7-4509-8594-c1e26fae077a', formalized).
narrative_ontology:cs_authority_grounding('da01f6d0-04b7-4509-8594-c1e26fae077a', lineage).
narrative_ontology:cs_interpretation_layer_present('da01f6d0-04b7-4509-8594-c1e26fae077a').
narrative_ontology:cs_reading_relation('da01f6d0-04b7-4509-8594-c1e26fae077a', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('da01f6d0-04b7-4509-8594-c1e26fae077a', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('da01f6d0-04b7-4509-8594-c1e26fae077a', foundational, execution_marginal_deterrent_effect).
narrative_ontology:cs_axiom_status(execution_marginal_deterrent_effect, holdable).
narrative_ontology:cs_axiom_grounding('da01f6d0-04b7-4509-8594-c1e26fae077a', execution_marginal_deterrent_effect, empirically_contingent).
narrative_ontology:cs_axiom('da01f6d0-04b7-4509-8594-c1e26fae077a', foundational, offender_instrumentalization_permissible).
narrative_ontology:cs_axiom_status(offender_instrumentalization_permissible, holdable).
narrative_ontology:cs_axiom_grounding('da01f6d0-04b7-4509-8594-c1e26fae077a', offender_instrumentalization_permissible, instrumental).
narrative_ontology:cs_reference_frame('da01f6d0-04b7-4509-8594-c1e26fae077a', classical_deterrence_framework).
narrative_ontology:cs_drift_state('da01f6d0-04b7-4509-8594-c1e26fae077a', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('da01f6d0-04b7-4509-8594-c1e26fae077a', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, general_public).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offenders).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_actor_theory).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, state_monopoly_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital trials, sentencing, death row incarceration, and execution protocols. Justifies the practice as a rational signal to prevent future murders. Derives institutional authority, budget, and symbolic monopoly over legitimate lethal force from the maintenance of capital punishment. Could abolish the practice through legislative or executive action but actively enforces it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives the claimed security benefit of deterrence: a purported reduction in homicide risk due to the state's credible threat of execution. Bears the fiscal and moral costs of maintaining the execution apparatus. Cannot individually opt out of the state's penal framework or the social contract that underwrites it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Sentenced to death after capital conviction. Confined on death row pending exhaustion of appeals and scheduled execution. Physically unable to exit the constraint; the extraction is terminal. Procedurally excluded from policy discourse about the justification for their own sentence.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offenders, payer,
    powerless, immediate, trapped, national).

% Organized groups seeking to end state-sanctioned execution on moral, empirical, or human-rights grounds. Provide legal representation and public campaigns. Structurally excluded from policy-making in retentionist jurisdictions despite mounting empirical challenges to the deterrence hypothesis.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Researchers studying the empirical relationship between capital punishment and homicide rates. Publish findings that are frequently contested or ignored by retentionist policymakers when results show null or negative deterrence effects. Hold an analytical seat with unimpeded professional exit.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminologists, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing future murders by imposing a cost (death) that exceeds the perceived benefit of homicide, thereby altering the rational calculus of potential offenders and coordinating social expectations around the severity of lethal violence.
% TRANSFER_FUNCTION: Transfers the life of the condemned offender to the claimed security benefit of the general public; transfers moral and operational authority over life and death to the state execution apparatus.
% ABSENT_VOICES: Condemned offenders are procedurally muted after sentencing; abolitionist advocates and criminologists demonstrating null deterrence effects are structurally marginalized in retentionist policy discourse; jurisdictions that have abolished are excluded from federal retentionist frameworks.
% DISAPPEARANCE_RATIONALE: Criminal codes would require amendment, sentencing regimes would shift toward life imprisonment, prison demographics would increase, death row infrastructure would close, and the state's claimed monopoly over legitimate lethal force would contract. The legal and penal landscape would reorganize around non-lethal maximum sentences.
% FOUNDING_PROBLEM: Unacceptably high rates of lethal violence threatening social order, and the perceived need for a deterrent signal of sufficient severity to dissuade potential murderers who might not be deterred by lesser sanctions.
% FOUNDING_PROBLEM_CORROBORATION: Criminologists and public health researchers outside the state apparatus attest to lethal violence as a persistent social problem, but contest whether the deterrence mechanism effectively addresses it; victim advocacy groups attest to the harm of murder from outside the benefiting parties, though they divide on whether execution prevents future victimization.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.6, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is moderate (0.60) because the constraint couples a real coordination claim (preventing future murders) with the most severe possible extraction (a human life). Suppression is moderate-high (0.65): the condemned are physically trapped, and abolitionist alternatives are suppressed by the state's active enforcement of execution protocols. Theater ratio is moderate and rising (0.50 at interval end) because as empirical evidence against deterrence has accumulated, the justification has increasingly relied on ritual, symbolic performance, and political signaling rather than demonstrable efficacy. Accessibility collapse (0.65) reflects the near-total collapse of alternatives for the condemned once sentenced, and the marginalization of non-punitive security frameworks in retentionist discourse. Resistance (0.55) captures persistent abolitionist legal challenges, moral opposition, and empirical critiques.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and general public experience the constraint as protective coordinationâa necessary hard edge of social order. The condemned offender experiences it as terminal extraction with no exit. The criminological observer seat sees an empirical claim that has not been reliably substantiated across jurisdictions, suggesting that the experienced coordination benefit may be partially illusory. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution apparatus is the structural agenda-setter with arbitrage-grade exit (it could change the law), placing it near the beneficiary end (low d). The general public is the declared beneficiary of deterrence, with constrained exit, sitting at low-to-moderate d. The condemned offender is the full target: powerless, trapped, and terminal, placing them at maximum d. Abolitionist advocates are excluded rather than coordinated; their exclusion is a structural feature of the retentionist framework. Criminologists hold an analytical seat with unimpeded exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents mislabeling in both directions. If deterrence were empirically proven and no less-extractive alternative existed, the constraint might approach rope. If the deterrence claim were pure cover with no contested empirical support, the constraint would be a snare. The contested evidenceâsome studies finding marginal effects, many finding null or negative effectsâplaces it in the hybrid zone where genuine coordination function and asymmetric extraction coexist. The rising theater ratio over the interval signals that the coordination story is becoming less functional and more performative, which the mandatrophy detector can track without premature reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_status,
    'Does capital punishment produce a marginal deterrent effect on homicide rates beyond the level achieved by non-lethal sanctions such as life without parole?',
    'Comparative jurisdictional analysis of homicide rates before and after abolition or moratorium, controlling for confounding variables; meta-analysis of econometric panel studies.',
    'If the effect is null or negative, the coordination function collapses and the constraint reclassifies toward snare. If a robust marginal effect exists, the tangled-rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Empirical status of the deterrence hypothesis').

omega_variable(
    instrumentalization_ethics,
    'Is the instrumentalization of the condemned offender''s life for the security benefit of the general public normatively permissible even if a deterrent effect exists?',
    'Philosophical analysis of Kantian dignity constraints versus utilitarian social-welfare frameworks; examination of constitutional jurisprudence on human dignity.',
    'If instrumentalization is categorically impermissible, the constraint is delegitimized regardless of empirical efficacy, shifting the kernel toward the abolition reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalization_ethics, preference, 'Normative permissibility of offender instrumentalization').

omega_variable(
    kernel_reading_locator,
    'This constraint is the deterrence reading of the state_killing_legitimacy kernel. The retributive reading reframes the offender as a morally liable desert-bearer rather than instrumentalized means; the abolition reading eliminates extraction entirely. What structural element carries the disagreement?',
    'Cross-reading comparison of beneficiary/victim arrays, axiom sets, and directionality structures in the compiled constraint family.',
    'Locates whether the kernel contest is empirical (does deterrence work?), deontological (is desert real?), or institutional (who holds legitimate authority over life?).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locator, conceptual, 'Structural location of the kernel disagreement').


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
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__deterrence_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__deterrence_reading, theater_ratio, 32, 0.46).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__deterrence_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__deterrence_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__deterrence_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__deterrence_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__deterrence_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__deterrence_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__deterrence_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__deterrence_reading, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
