% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority — Retributive Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story captures the retributive reading of state execution
 *   authority: the claim that execution restores moral balance by imposing
 *   proportionate punishment (life for life) for heinous crimes. The reading
 *   asserts that imprisonment cannot substitute — the moral debt created by
 *   certain crimes is incommensurable with any penalty short of death.
 *   Victims' families are structural beneficiaries (receiving moral closure);
 *   executed offenders are structural payers (forfeiting their lives as the
 *   required payment). The state execution authority administers the
 *   apparatus and derives institutional legitimacy from it. The constraint
 *   requires active enforcement (capital statutes, trial and appeal
 *   machinery, execution protocols) and exhibits high extraction (ε=0.78)
 *   because the cost to the offender is absolute and non-substitutable, while
 *   the coordination function (moral order restoration) is real but
 *   contested. Wrongful executions are treated within the framework as tragic
 *   errors that do not invalidate the retributive logic — a feature that
 *   sustains high suppression (0.85) as the system must defend against
 *   abolitionist challenges that use error rates as evidence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.78).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.85).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority — Retributive Reading").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '7453b551-b934-4bb6-98bf-a1996e8e48f3').
narrative_ontology:cs_kernel_codification('7453b551-b934-4bb6-98bf-a1996e8e48f3', formalized).
narrative_ontology:cs_authority_grounding('7453b551-b934-4bb6-98bf-a1996e8e48f3', extraction).
narrative_ontology:cs_interpretation_layer_present('7453b551-b934-4bb6-98bf-a1996e8e48f3').
narrative_ontology:cs_reading_relation('7453b551-b934-4bb6-98bf-a1996e8e48f3', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('7453b551-b934-4bb6-98bf-a1996e8e48f3', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('7453b551-b934-4bb6-98bf-a1996e8e48f3', foundational, proportionate_punishment_restores_moral_balance).
narrative_ontology:cs_axiom_status(proportionate_punishment_restores_moral_balance, holdable).
narrative_ontology:cs_axiom_grounding('7453b551-b934-4bb6-98bf-a1996e8e48f3', proportionate_punishment_restores_moral_balance, deontological).
narrative_ontology:cs_axiom('7453b551-b934-4bb6-98bf-a1996e8e48f3', foundational, heinous_crimes_create_moral_debt_requiring_life_forfeiture).
narrative_ontology:cs_axiom_status(heinous_crimes_create_moral_debt_requiring_life_forfeiture, holdable).
narrative_ontology:cs_axiom_grounding('7453b551-b934-4bb6-98bf-a1996e8e48f3', heinous_crimes_create_moral_debt_requiring_life_forfeiture, deontological).
narrative_ontology:cs_reference_frame('7453b551-b934-4bb6-98bf-a1996e8e48f3', retributive_justice_framework).
narrative_ontology:cs_drift_state('7453b551-b934-4bb6-98bf-a1996e8e48f3', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7453b551-b934-4bb6-98bf-a1996e8e48f3', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, society_seeking_moral_order).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_execution_authority).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportionate_punishment_restores_moral_balance).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, heinous_crimes_create_moral_debt_requiring_life_forfeiture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience the execution as moral closure and restoration of balance for the heinous crime that took their loved one. Their participation in the process (victim impact statements, witnessing) is structured by the legal framework. They cannot exit the crime's impact, but the execution provides a structured endpoint.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, national).

% Bear the ultimate cost of the constraint — their life — as the proportionate payment for the moral debt incurred by their crime. The legal process (appeals, clemency) provides procedural delay but not structural exit; the constraint's logic treats their death as the necessary and sufficient satisfaction of justice.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Administers the death penalty apparatus: legislates capital statutes, conducts trials and appeals, carries out executions. Claims legitimacy from the retributive framework (moral balance restored) and collects institutional authority from being the sole legitimate wielder of lethal punishment. Could abolish or restrict the practice but faces political pressure from beneficiary constituencies.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, state_execution_authority, beneficiary).

% Argue that state execution is categorically impermissible regardless of crime severity or procedural safeguards. They are structurally excluded from the constraint's internal logic — the retributive framework treats abolition as a rejection of justice itself, not a competing policy view. They seek to dismantle the constraint through litigation, legislation, and moral suasion.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Support execution on forward-looking grounds (preventing future murders) rather than backward-looking moral balance. They coexist with the retributive reading in political coalitions but disagree on the constraint's fundamental justification. Their empirical claims (deterrence effect) are contested and structurally separable from the retributive axiom.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, deterrence_advocates, observer,
    organized, generational, analytical, national).

% Interpret and apply the constitutional and statutory framework governing execution. They mediate between the retributive axiom and procedural safeguards (due process, Eighth Amendment). Their rulings shape the constraint's operational boundaries but do not challenge its foundational premise within the retributive reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, legal_scholars_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a definitive, proportionate response to heinous crimes that satisfies the moral order by imposing a punishment equal to the offense — life for life — thereby restoring balance that imprisonment cannot achieve.
% TRANSFER_FUNCTION: Moves the offender's life from the offender to the moral account of the victims and society, satisfying a debt that the retributive framework treats as incommensurable with any lesser penalty.
% ABSENT_VOICES: Abolitionists who reject the moral legitimacy of state killing entirely; families of offenders who bear collateral suffering; international human rights bodies that categorize execution as a violation of the right to life. These voices are excluded from the constraint's internal deliberative space — the retributive logic treats their objection as a category error.
% DISAPPEARANCE_RATIONALE: If the retributive execution constraint vanished overnight, the punishment system would reorganize around life without parole as the maximum penalty, victims' families would lose the structured closure mechanism the constraint provides, and the state would lose its most potent symbol of moral authority over life and death. The moral vocabulary of 'just deserts' for heinous crimes would lose its institutional anchor.
% FOUNDING_PROBLEM: How to respond to crimes so heinous that they rupture the moral order, such that any punishment less than death fails to acknowledge the gravity of the offense and leaves a moral debt unpaid.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Stuart Banner, 'The Death Penalty: An American History') document the founding era's embrace of retributive proportionality. Victims' rights organizations (e.g., Justice for All) attest the problem remains live. Abolitionist scholars (e.g., Carol Steiker, Jordan Steiker) and international bodies (UN Human Rights Committee) attest the problem is either dead (morally illegitimate) or never validly founded. The status is genuinely contested across institutional and moral authorities outside the beneficiary set.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78) reflects the non-substitutability of death as punishment — the constraint's logic insists that only life answers for life, making the extraction from the offender total. Suppression (0.85) is high because the constraint's persistence depends on actively maintaining the legal and procedural machinery that prevents exit (for offenders) and excludes alternatives (abolition, life without parole as sufficient). Theater ratio (0.42) is moderate: the elaborate procedural apparatus (appeals, clemency, witness rituals) serves both functional due process and performative legitimation. Accessibility collapse (0.92) is near-total for the offender (no exit from the constraint once sentenced to death) but lower for society (alternative moral frameworks exist). Resistance (0.55) is moderate: sustained abolitionist litigation, declining execution rates, and pharmaceutical supply constraints create friction but have not overturned the constraint in retentionist jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the state_execution_authority and victims_families seats, the constraint computes as coordination (moral order restored, justice served). From the executed_offender seat, it computes as pure extraction (life taken with no possible restitution). The engine will compute this divergence from the structural power/exit asymmetry: the agenda setter has arbitrage-grade exit (could abolish the practice) while the payer has zero exit. The abolition_advocate seat, though excluded, would compute the constraint as snare if included — its exclusion is what allows the retributive reading to maintain its coordination claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families (moderate power, constrained exit) are beneficiaries — they receive moral closure but cannot control the process. Executed offenders (powerless, trapped) are full targets — directionality near 1.0. State execution authority (institutional, arbitrage) is the agenda setter and partial beneficiary — it collects institutional authority and controls the machinery, but faces political costs. Abolition advocates (organized, constrained) are excluded — their structural position is outside the constraint's logic, not within it as payers. Deterrence advocates (organized, analytical) are observers — they share the constraint's operational conclusion but not its foundational axiom. Legal scholars/courts (institutional, analytical) are observers — they interpret boundaries without challenging the retributive premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The retributive reading does not exhibit mandatrophy in the classic sense — its founding problem (responding to heinous crimes with proportionate punishment) is contested but not dead. However, the constraint shows extraction accumulation (rising ε) and enforcement intensification (rising suppression) over the interval, suggesting the coordination function is being layered with additional justificatory burdens (procedural perfectionism, method-of-execution litigation) that serve to maintain the constraint's legitimacy rather than improve its moral function. The theater ratio rise from 0.25 to 0.42 indicates growing performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the retributive reading a distinct constraint from the deterrence and abolition readings, or a measurement perspective on a single constraint?',
    'Per ε-invariance principle: if changing the reading changes the beneficiary/victim structure, ε, or claimed type, they are distinct constraints. This reading has victims_families as beneficiaries and executed_offenders as payers with high ε; abolition_reading has no beneficiaries and executed_offenders as victims with different ε; deterrence_reading has potential_future_victims as beneficiaries. The structural deltas confirm distinct constraints.',
    'Confirms this story correctly models one reading as a standalone constraint linked via network.affects_constraints, not a parameterized variant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Validates the kernel-reading decomposition per DP-001 ε-invariance.').

omega_variable(
    moral_balance_measurability,
    'Is ''moral balance restoration'' an empirically observable coordination outcome or a purely normative claim that cannot be falsified?',
    'If moral balance restoration admits empirical indicators (victim family closure measures, community moral sentiment surveys, recidivism of moral order violations), it has a coordination function that can be assessed. If it is definitionally satisfied by the execution itself, the coordination claim is tautological and the constraint reduces to extraction with a normative cover story.',
    'If tautological, the claimed coordination function collapses and the constraint reclassifies toward snare. If measurable, the tangled_rope classification holds with a genuine coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_balance_measurability, conceptual, 'Whether the retributive coordination function has empirical content or is definitionally self-validating.').

omega_variable(
    wrongful_execution_threshold,
    'At what rate of wrongful execution does the retributive framework''s ''tragic error'' internalization break down, forcing reclassification?',
    'Track exoneration rates (e.g., Death Penalty Information Center data) and their effect on public legitimacy, judicial willingness to impose death sentences, and legislative abolition momentum. A structural break occurs when the error rate becomes a constraint on the constraint — i.e., when the machinery of execution cannot operate because the error rate undermines the moral authority the retributive axiom requires.',
    'If the framework internalizes error indefinitely, suppression reflects pure enforcement capacity. If error rate triggers legitimacy collapse, suppression includes a fragility component that the current metric does not capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_threshold, empirical, 'Whether wrongful executions are a tolerable cost within the framework or a structural fault line.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t1976, state_execution_authority__retributive_reading, theater_ratio, 1976, 0.25).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t1985, state_execution_authority__retributive_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t1995, state_execution_authority__retributive_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t2005, state_execution_authority__retributive_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t2015, state_execution_authority__retributive_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t2024, state_execution_authority__retributive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t1976, state_execution_authority__retributive_reading, base_extractiveness, 1976, 0.65).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t1985, state_execution_authority__retributive_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t1995, state_execution_authority__retributive_reading, base_extractiveness, 1995, 0.73).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t2005, state_execution_authority__retributive_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t2015, state_execution_authority__retributive_reading, base_extractiveness, 2015, 0.77).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t2024, state_execution_authority__retributive_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t1976, state_execution_authority__retributive_reading, suppression_requirement, 1976, 0.7).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t1985, state_execution_authority__retributive_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t1995, state_execution_authority__retributive_reading, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t2005, state_execution_authority__retributive_reading, suppression_requirement, 2005, 0.82).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t2015, state_execution_authority__retributive_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t2024, state_execution_authority__retributive_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__retributive_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint is the retributive_reading of the state_execution_authority kernel. The abolition_reading forecloses this reading's foundational axiom; the deterrence_reading coexists with it. All three readings share the kernel's constitutional/legal infrastructure but instantiate different constraints with different ε, beneficiary/victim structures, and claimed types. The kernel's authority_grounding is lineage (constitutional tradition); this reading's authority_grounding is extraction (institutional authority derived from maintaining the retributive framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__retributive_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_execution_authority__retributive_reading, powerless, 0.98).
constraint_indexing:directionality_override(state_execution_authority__retributive_reading, moderate, 0.2).
constraint_indexing:directionality_override(state_execution_authority__retributive_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
