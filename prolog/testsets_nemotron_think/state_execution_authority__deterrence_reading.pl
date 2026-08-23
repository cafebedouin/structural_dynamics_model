% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The deterrence reading of state execution authority holds that capital
 *   punishment is justified because it raises the expected cost of committing
 *   capital crimes, thereby preventing future murders. The constraint is the
 *   state's legal power to execute, operationalized through statutes,
 *   sentencing guidelines, and execution protocols. The reading acknowledges
 *   that the executed offender bears an instrumental cost (their life) and
 *   that wrongful executions are utilitarian losses requiring procedural
 *   safeguards. The constraint's legitimacy rests on the empirical claim that
 *   execution deters more effectively than the next harshest penalty
 *   (typically life without parole). If that claim fails, the constraint
 *   becomes extractive without offsetting coordination benefit.
 *
 * KEY AGENTS:
 *   - state_execution_authority: agenda_setter (institutional/generational/arbitrage/national) — sets and enforces capital punishment statutes
 *   - executed_offenders: payer (powerless/immediate/trapped/national) — bear the ultimate cost of the constraint
 *   - wrongfully_executed: payer (powerless/immediate/trapped/national) — suffer the constraint's error rate
 *   - future_potential_victims: beneficiary (powerless/biographical/constrained/national) — receive the deterrence benefit
 *   - society_at_large: beneficiary (organized/generational/mobile/national) — gains collective security
 *   - abolition_advocates: excluded (organized/biographical/mobile/national) — would challenge the constraint's legitimacy
 *   - retributive_proponents: observer (organized/biographical/analytical/national) — offer a rival justification for the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.45).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.55).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '92736429-ecde-45c1-a5b8-6b1070f997bd').
narrative_ontology:cs_kernel_codification('92736429-ecde-45c1-a5b8-6b1070f997bd', formalized).
narrative_ontology:cs_authority_grounding('92736429-ecde-45c1-a5b8-6b1070f997bd', lineage).
narrative_ontology:cs_interpretation_layer_present('92736429-ecde-45c1-a5b8-6b1070f997bd').
narrative_ontology:cs_reading_relation('92736429-ecde-45c1-a5b8-6b1070f997bd', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('92736429-ecde-45c1-a5b8-6b1070f997bd', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('92736429-ecde-45c1-a5b8-6b1070f997bd', foundational, execution_justified_by_deterrence).
narrative_ontology:cs_axiom_status(execution_justified_by_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('92736429-ecde-45c1-a5b8-6b1070f997bd', execution_justified_by_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('92736429-ecde-45c1-a5b8-6b1070f997bd', secondary, error_rate_minimization_required).
narrative_ontology:cs_axiom_status(error_rate_minimization_required, holdable).
narrative_ontology:cs_axiom_grounding('92736429-ecde-45c1-a5b8-6b1070f997bd', error_rate_minimization_required, instrumental).
narrative_ontology:cs_reference_frame('92736429-ecde-45c1-a5b8-6b1070f997bd', deterrence_justification_framework).
narrative_ontology:cs_drift_state('92736429-ecde-45c1-a5b8-6b1070f997bd', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('92736429-ecde-45c1-a5b8-6b1070f997bd', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, society_at_large).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_executed).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, deterrence_theory_of_punishment).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, state_monopoly_on_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces capital punishment statutes, controls execution protocols, and bears the political cost of maintaining the system. Justifies the constraint as necessary for public safety. Can modify or abolish the death penalty legislatively but faces high political cost for doing so.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals convicted of capital crimes and sentenced to death. They bear the ultimate cost of the constraint (their lives). Their exit options are nonexistent once sentenced; appeals are procedural, not substitutive.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% The subset of executed offenders who are factually innocent. They bear the constraint's error cost. The reading treats them as a utilitarian loss to be minimized; structurally they are the most extreme payers.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_executed, payer,
    powerless, immediate, trapped, national).

% People who would be murdered if not for the deterrent effect of execution. They are not organized and cannot exit the constraint's protection (or lack thereof). Their benefit is probabilistic and diffuse.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, biographical, constrained, national).

% The collective beneficiary of reduced homicide rates (if deterrence works). Includes voters, taxpayers, and communities. They can influence the constraint through political action (mobility).
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% Groups and individuals who argue execution is categorically impermissible. They are structurally excluded from the constraint's operational logic — their preferred alternative (abolition) is treated as outside the policy menu by the agenda_setter.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolition_advocates, excluded,
    organized, biographical, mobile, national).

% Actors who justify execution on retributive grounds (moral desert, proportionality). They observe the same kernel but instantiate a different reading. They may ally with the deterrence reading politically but disagree on the constraint's normative structure.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, retributive_proponents, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Deters capital crimes by raising the expected cost of commission, thereby preventing future murders and protecting innocent lives.
% TRANSFER_FUNCTION: Transfers the cost of deterrence (the lives of executed offenders, plus risk of wrongful execution) from potential victims to offenders, administered by the state.
% ABSENT_VOICES: Abolition advocates and families of wrongfully executed individuals are structurally excluded; they would argue the deterrence benefit is illusory and the cost morally unacceptable, but the constraint's operational logic treats abolition as outside the conversation.
% DISAPPEARANCE_RATIONALE: If the deterrence justification vanished overnight, states would face a legitimacy crisis: they would either abolish execution (shifting to LWOP), retain it on retributive grounds alone (changing the constraint's type), or maintain it as pure theater. The institutional machinery (death rows, execution teams, appellate protocols) would not disappear automatically — it would be repurposed or dismantled.
% FOUNDING_PROBLEM: In the 1970s, after Furman v. Georgia, states reinstated capital punishment to address public fear of violent crime and perceived inadequacy of existing penalties. The founding problem was: how to credibly threaten the ultimate sanction to deter the worst crimes when prison sentences were seen as insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (prosecutors, victims' rights groups) attest the problem remains live, citing public opinion and high-profile crimes. Abolitionists and criminologists (outside the beneficiary set) attest the problem is dead: modern LWOP achieves incapacitation, and deterrence studies show no marginal effect of execution over LWOP. Legislative repeals in 11 states since 2007 corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.45) reflects the moderate but non-trivial cost imposed on offenders (death) and the risk of wrongful execution, balanced against the claimed deterrence benefit. The value is not higher because the reading explicitly conditions legitimacy on deterrence efficacy — if life without parole deters equally, the marginal extraction of execution is unnecessary. Suppression (0.55) captures the state's active enforcement machinery (death rows, execution protocols, appeals limits) and the structural exclusion of abolitionist alternatives from the policy menu. Theater ratio (0.3) is low because the deterrence function is genuinely invoked, though rising procedural theater (lengthy appeals, secrecy protocols) slightly inflates it. Accessibility collapse (0.5) is moderate: alternatives (life without parole) exist and are used in many jurisdictions, but the constraint's proponents actively resist their adoption as sufficient. Resistance (0.65) is high due to sustained abolitionist movements, international pressure, and judicial skepticism.
 *
 * PERSPECTIVAL GAP:
 *   The state (agenda_setter) experiences the constraint as coordination: it provides a unique deterrent signal that protects citizens. Executed offenders (payers) experience it as pure extraction: their lives are taken for a deterrence effect that may not exist. Future potential victims (beneficiaries) experience it as coordination if deterrence works, but as irrelevant if it does not. The engine will compute per-seat types from these structural positions; the divergence between agenda_setter (likely rope/tangled_rope) and payer (likely snare) is the core measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (future_potential_victims, society_at_large) are declared because the reading's entire justification is the protection of innocent lives. Victims (executed_offenders, wrongfully_executed) are declared because the constraint physically kills the former and risks killing the latter. The state is the agenda_setter: it writes the statutes, controls the execution machinery, and collects the legitimacy surplus. Directionality derivation: state d ≈ 0.1 (beneficiary of legitimacy), offenders d ≈ 0.9 (full targets), potential victims d ≈ 0.2 (net beneficiaries if deterrence works). Wrongful execution risk slightly raises potential victims' d but they remain net beneficiaries in the reading's own calculus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deterring capital crimes in an era of limited incarceration capacity) is contested: proponents argue it remains live; abolitionists and many criminologists argue it is dead because modern prisons achieve incapacitation and deterrence without execution. The constraint persists despite contested founding problem because the state extracts legitimacy from appearing 'tough on crime' and because the institutional machinery (prosecutors, death rows, execution teams) has become self-sustaining. This is not a piton — the constraint still has active beneficiaries (prosecutors, victims' rights groups) who profit from its maintenance — but it shows mandatrophy dynamics: the original deterrence rationale is eroding while the enforcement apparatus expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_deterrence_reading,
    'How does the deterrence reading''s structural classification change if the kernel''s committer frame is taken into account?',
    'Compare the deterrence reading''s ε, beneficiary/victim structure, and drift state against the retributive and abolition readings. The committer frame requires that each reading be a clean ε-invariant constraint; this omega records that this file is one reading of a contested kernel.',
    'If the kernel frame is ignored, the constraint might be misclassified as a single monolithic ''death penalty'' constraint rather than a family of structurally distinct readings. The committer frame forces decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_deterrence_reading, conceptual, 'This constraint is one reading of the state_execution_authority kernel; sibling readings are retributive_reading and abolition_reading.').

omega_variable(
    deterrence_efficacy_uncertainty,
    'Does execution deter capital crimes more effectively than life without parole?',
    'Natural experiments from abolitionist jurisdictions, panel studies of homicide rates before/after moratoriums, and meta-analyses of deterrence literature.',
    'If deterrence efficacy is zero or negative, the constraint''s extractiveness becomes unjustified coordination failure → reclassifies toward snare. If efficacy is robust, the constraint remains tangled_rope (coordination + asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_efficacy_uncertainty, empirical, 'The core empirical premise of the deterrence reading.').

omega_variable(
    substitution_by_lwop,
    'Is life without parole a functionally equivalent substitute for the deterrence function?',
    'Comparative deterrence studies matching execution and LWOP jurisdictions; surveys of offender risk perception.',
    'If LWOP substitutes, the marginal extraction of execution is unnecessary → ε rises toward snare territory. If LWOP fails to substitute, the constraint''s coordination function is unique → ε stays moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_by_lwop, empirical, 'Whether the coordination function requires execution specifically or any severe certain penalty.').

omega_variable(
    wrongful_execution_rate,
    'What is the actual rate of wrongful execution, and does the system''s error-correction machinery reduce it to near-zero?',
    'Post-execution exoneration data, innocence project audits, analysis of appellate reversal rates in capital cases.',
    'High wrongful execution rate increases the utilitarian loss term, raising effective extraction on the payer seat and potentially triggering snare classification for that seat. Low rate supports the reading''s error-minimization claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_rate, empirical, 'The utilitarian loss from executing the innocent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__deterrence_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__deterrence_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__deterrence_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__deterrence_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__deterrence_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__deterrence_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__deterrence_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__deterrence_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__deterrence_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the state_execution_authority kernel family. The deterrence reading claims moderate ε conditional on empirical efficacy; the retributive reading claims low ε (coordination of moral order); the abolition reading claims high ε (pure extraction). They are linked by network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
