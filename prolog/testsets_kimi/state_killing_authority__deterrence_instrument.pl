% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: State Killing Authority: Deterrence Instrument Reading
 *   domain: criminal_justice/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint is the deterrence_instrument reading of the
 *   state_killing_authority kernel: the claim that state execution of
 *   convicted murderers is justified exclusively by its efficacy in
 *   preventing future homicides at acceptable cost. It instantiates a
 *   normative framework where future potential victims are the structural
 *   beneficiaries, the condemned person is an instrumental cost, and state
 *   authority derives from crime-prevention efficacy rather than retributive
 *   desert or inalienable rights. The kernel is contested by
 *   retributive_desert (execution as proportional punishment) and
 *   categorical_abolition (state killing inherently impermissible) readings.
 *
 * KEY AGENTS:
 *   - state_criminal_justice_apparatus: Agenda setter (institutional/national) â sets legal criteria for capital sentencing, carries out executions, and commissions deterrence studies to justify the framework.
 *   - condemned_person: Primary target (powerless/trapped) â bears the ultimate extraction of life under the instrumental cost rationale; physically confined and procedurally excluded from the deterrence calculus.
 *   - future_potential_victims: Structural beneficiary (powerless/national, non-agent) â hypothetical individuals whose lives are claimed to be saved by the deterrent effect; their beneficiary status is projected onto an abstract public-safety ledger.
 *   - abolition_movement: Excluded voice (organized/national) â argues deterrence is unproven and instrumentalizing human life is illegitimate; structurally absent from the cost-benefit justification.
 *   - empirical_criminologists: Analytical observer (organized/analytical) â assess deterrence claims with empirical methods; contradictory findings are often sidelined in policy discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.82).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.78).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "State Killing Authority: Deterrence Instrument Reading").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '9cbf2a67-e948-423b-8ac8-517320c09f4e').
narrative_ontology:cs_kernel_codification('9cbf2a67-e948-423b-8ac8-517320c09f4e', formalized).
narrative_ontology:cs_authority_grounding('9cbf2a67-e948-423b-8ac8-517320c09f4e', practice).
narrative_ontology:cs_interpretation_layer_present('9cbf2a67-e948-423b-8ac8-517320c09f4e').
narrative_ontology:cs_reading_relation('9cbf2a67-e948-423b-8ac8-517320c09f4e', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('9cbf2a67-e948-423b-8ac8-517320c09f4e', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('9cbf2a67-e948-423b-8ac8-517320c09f4e', foundational, execution_justified_iff_deterrent_efficacy_proven).
narrative_ontology:cs_axiom_status(execution_justified_iff_deterrent_efficacy_proven, holdable).
narrative_ontology:cs_axiom_grounding('9cbf2a67-e948-423b-8ac8-517320c09f4e', execution_justified_iff_deterrent_efficacy_proven, instrumental).
narrative_ontology:cs_axiom('9cbf2a67-e948-423b-8ac8-517320c09f4e', foundational, condemned_life_subordinate_to_public_safety).
narrative_ontology:cs_axiom_status(condemned_life_subordinate_to_public_safety, holdable).
narrative_ontology:cs_axiom_grounding('9cbf2a67-e948-423b-8ac8-517320c09f4e', condemned_life_subordinate_to_public_safety, instrumental).
narrative_ontology:cs_reference_frame('9cbf2a67-e948-423b-8ac8-517320c09f4e', instrumental_public_safety_framework).
narrative_ontology:cs_drift_state('9cbf2a67-e948-423b-8ac8-517320c09f4e', post_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9cbf2a67-e948-423b-8ac8-517320c09f4e', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_victims).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_person).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legal criteria for capital sentencing, carries out executions, and commissions or cites deterrence studies to justify the framework. Controls the carceral and execution infrastructure. Could abolish the practice through legislative or judicial action but is constrained by political economy and institutional inertia.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_criminal_justice_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Convicted of capital murder and sentenced to death. Bears the ultimate extraction of life under the instrumental cost rationale. Physically confined on death row, procedurally excluded from the deterrence calculus that justifies their execution, and dependent on appellate processes for survival.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_person, payer,
    powerless, immediate, trapped, national).

% Hypothetical individuals whose lives the constraint claims to save through the deterrent effect of execution. They do not act, choose, or collect rents; their beneficiary status is projected by the deterrence reading onto an abstract public-safety ledger.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_victims, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(state_killing_authority__deterrence_instrument, future_potential_victims).

% Advocates for the abolition of capital punishment on empirical and moral grounds. Argues that deterrence is unproven and that instrumentalizing human life is illegitimate. Structurally excluded from the deterrence framework's cost-benefit calculus and from legislative chambers where the constraint is maintained.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolition_movement, excluded,
    organized, generational, constrained, national).

% Conduct meta-analyses and comparative studies on the deterrent effect of capital punishment. Their findings are often contradictory and politically contested; when results contradict the deterrence hypothesis, they are sidelined in policy discourse.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, empirical_criminologists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents future murders by creating a credible threat of execution that deters potential killers, thereby coordinating public safety without requiring individual self-defense.
% TRANSFER_FUNCTION: Moves the condemned person's life from the condemned to the state's carceral and execution apparatus, justified by the claim that this transfer deters future homicides and protects future victims.
% ABSENT_VOICES: The condemned person's voice is procedurally marginalized as the instrumental cost. Abolition movements and empirical criminologists who contest deterrence are structurally excluded from the justification framework; their presence would collapse the cost-benefit calculus if deterrence is unproven.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the state would lose its claimed authority to execute for deterrence; penal policy would shift to incarceration, the death row population would not be executed, and the public safety justification would require replacement with alternative crime-prevention frameworks.
% FOUNDING_PROBLEM: High homicide rates and the perceived inadequacy of non-lethal sanctions to deter the most serious murders.
% FOUNDING_PROBLEM_CORROBORATION: Independent criminological meta-analyses and international human rights bodies attest that the deterrence justification is empirically contested. Law enforcement agencies sometimes assert the founding problem is still live, but corroboration from outside the benefiting parties comes from academic research and abolition advocacy.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint extracts the condemned person's life â the maximum possible extraction. Suppression is high (0.78) because the constraint depends on physically preventing the condemned's escape, silencing their voice in the justification framework, and marginalizing abolitionist challenge. Theater ratio is moderate-high (0.45) because the deterrence function relies heavily on the visibility and symbolism of execution; the performative threat must be credible to coordinate behavior. Accessibility collapse is high (0.75) for the condemned (no exit from death row except procedural reversal); for the policy, alternatives like life without parole are structurally present but delegitimized within this reading. Resistance is substantial (0.72) from abolition movements, legal challenges, and empirical critique.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state criminal justice apparatus) experiences the constraint as a necessary public safety coordination mechanism whose extraction is regrettable but justified. The payer seat (condemned person) experiences it as total extraction with no reciprocal benefit. The excluded seat (abolition movement) sees the coordination claim as empirically hollow. The engine computes this divergence from the structural data: the state has constrained exit (can abolish but faces political costs), the condemned has trapped exit, and future victims are non-agents who cannot contest their beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned_person is the clear structural target (high d â high effective extraction) because they bear the life-cost of the constraint with no exit. The state_criminal_justice_apparatus sits near the beneficiary end (low d) because it controls the constraint and gains operational authority and public safety legitimacy. Future_potential_victims are beneficiaries (low d) by design of the reading, but as non-agents they do not actively collect and are excluded from directionality derivation. The abolition_movement, though excluded, would experience high d because they bear the moral and social costs of living under a state that kills instrumentally.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by requiring both coordination and extraction for tangled rope. The deterrence reading has a genuine coordination function (preventing future murders) that is structurally separable from the extraction (killing the condemned). If deterrence were empirically proven and applied without error, the constraint would remain tangled rope because the coordination and extraction remain inseparably bundled through the same condemned body. If deterrence is disproven, the coordination function dies and the constraint becomes a snare (pure extraction under false cover) or piton (inertial persistence). The temporal measurements show extraction accumulation as empirical doubt grows, suggesting possible drift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint instantiates the deterrence_instrument reading of the state_killing_authority kernel; how would the beneficiary/victim structure and directionality change under the retributive_desert or categorical_abolition readings?',
    'Comparative analysis of the sibling constraint stories in the same kernel family, examining whether the condemned person''s directionality shifts from instrumental cost (deterrence) to just desert recipient (retributive) or to rights-bearing subject (abolition).',
    'Under retributive_desert, the condemned is not a victim but a moral agent receiving proportional punishment, collapsing the extraction asymmetry. Under categorical_abolition, state killing has no beneficiary and the constraint dissolves entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Structural delta between deterrence reading and sibling readings of the same kernel.').

omega_variable(
    deterrence_empirical_validity,
    'Does capital punishment produce a marginal deterrent effect greater than alternative sanctions, and is the state''s application of this constraint actually conditioned on such evidence?',
    'Natural experiments comparing homicide trends in matched jurisdictions with and without capital punishment, controlling for certainty of apprehension and sentencing, alongside legislative history review to test whether repeal or retention tracks empirical findings.',
    'If deterrence is disproven, this reading''s coordination function collapses and the constraint drifts toward snare (pure extraction under false coordination cover) or piton (inertial persistence). If proven, the tangled rope classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_validity, empirical, 'Empirical foundation of the deterrence justification.').

omega_variable(
    acceptable_cost_boundary,
    'What rate of erroneous execution, racial disparity, or procedural failure is ''acceptable'' within the deterrence calculus, and does the constraint''s operation remain conditional on staying beneath that threshold?',
    'Comparative analysis of exoneration rates and demographic sentencing disparities against the claimed lives-saved ledger.',
    'If the state tolerates high error rates, the ''acceptable cost'' clause becomes a rhetorical fig leaf and the constraint shifts toward snare classification; if error rates are treated as dispositive, the constraint is more tightly bound to its instrumental logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_boundary, conceptual, 'Ambiguity in the acceptable-cost threshold of the deterrence justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__deterrence_instrument, theater_ratio, 10, 0.3).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__deterrence_instrument, theater_ratio, 20, 0.35).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__deterrence_instrument, theater_ratio, 30, 0.39).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.42).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__deterrence_instrument, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__deterrence_instrument, base_extractiveness, 10, 0.73).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__deterrence_instrument, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__deterrence_instrument, base_extractiveness, 30, 0.79).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__deterrence_instrument, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__deterrence_instrument, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__deterrence_instrument, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__deterrence_instrument, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__deterrence_instrument, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is the deterrence_instrument reading of the state_killing_authority kernel. The kernel decomposes into three structurally distinct constraints because the deterrence, retributive, and abolition readings produce different beneficiary/victim structures and different epsilon values. Each reading is authored as a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
