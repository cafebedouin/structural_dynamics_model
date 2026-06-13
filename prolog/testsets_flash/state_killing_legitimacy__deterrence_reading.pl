% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Killing Legitimacy (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence reading' of state killing
 *   legitimacy, where capital punishment is justified as a rational signal to
 *   prevent future murders. The offender is instrumentalized as a means to a
 *   social end, with potential future victims as the primary beneficiaries.
 *   The constraint operates as a Tangled Rope due to its genuine (though
 *   empirically contested) coordination function and its clear asymmetric
 *   extraction from the convicted offender. The metrics reflect a moderate
 *   level of extraction and high suppression, maintained by active
 *   enforcement, despite ongoing empirical challenges to its core premise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.6).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.7).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Killing Legitimacy (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'f9b89510-8610-48c5-8ad3-1e5f56174bfa').
narrative_ontology:cs_kernel_codification('f9b89510-8610-48c5-8ad3-1e5f56174bfa', formalized).
narrative_ontology:cs_authority_grounding('f9b89510-8610-48c5-8ad3-1e5f56174bfa', lineage).
narrative_ontology:cs_interpretation_layer_present('f9b89510-8610-48c5-8ad3-1e5f56174bfa').
narrative_ontology:cs_reading_relation('f9b89510-8610-48c5-8ad3-1e5f56174bfa', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9b89510-8610-48c5-8ad3-1e5f56174bfa', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('f9b89510-8610-48c5-8ad3-1e5f56174bfa', foundational, execution_prevents_future_crimes).
narrative_ontology:cs_axiom_status(execution_prevents_future_crimes, holdable).
narrative_ontology:cs_axiom_grounding('f9b89510-8610-48c5-8ad3-1e5f56174bfa', execution_prevents_future_crimes, empirically_contingent).
narrative_ontology:cs_axiom('f9b89510-8610-48c5-8ad3-1e5f56174bfa', secondary, state_has_right_to_protect_citizens_by_any_means).
narrative_ontology:cs_axiom_status(state_has_right_to_protect_citizens_by_any_means, holdable).
narrative_ontology:cs_axiom_grounding('f9b89510-8610-48c5-8ad3-1e5f56174bfa', state_has_right_to_protect_citizens_by_any_means, conventional).
narrative_ontology:cs_reference_frame('f9b89510-8610-48c5-8ad3-1e5f56174bfa', rational_state_deterrence_model).
narrative_ontology:cs_drift_state('f9b89510-8610-48c5-8ad3-1e5f56174bfa', contemporary_criminological_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f9b89510-8610-48c5-8ad3-1e5f56174bfa', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_security_apparatus).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, convicted_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, families_of_offenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces capital punishment, justifying it as a necessary tool for public safety and crime prevention. Benefits from the perceived authority and control over extreme violence.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_security_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct targets of the constraint, losing their lives. They are instrumentalized as a means to a social end (deterrence) without their consent or benefit.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, convicted_offenders, payer,
    powerless, immediate, trapped, local).

% Are theorized to benefit from the deterrent effect of capital punishment, as their lives are hypothetically saved by the prevention of future murders. This benefit is diffuse and unmeasurable directly.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Bear the emotional and social costs of the execution, experiencing profound loss and stigma. They have no agency in the process and no means of exit from its impact.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, families_of_offenders, payer,
    powerless, biographical, constrained, local).

% Argue against capital punishment on moral and ethical grounds, often citing its ineffectiveness as a deterrent and its irreversible nature. While organized, they are excluded from the decision-making process in jurisdictions that maintain capital punishment.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Analyze empirical data to determine the actual deterrent effect of capital punishment. Their findings often challenge the premise of deterrence, but their role is primarily analytical, not executive.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminologists_and_statisticians, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social behavior by establishing a clear, extreme consequence for murder, thereby deterring potential offenders and ensuring public safety.
% TRANSFER_FUNCTION: Transfers the life of the convicted offender to the state, with the theoretical benefit of preventing future murders and enhancing public safety. The cost is borne by the offender and their family.
% ABSENT_VOICES: The convicted offenders themselves, whose voices are silenced by the act. Abolitionist advocates, who are often marginalized in public discourse in pro-capital punishment jurisdictions, would argue for alternative, less extreme forms of justice.
% DISAPPEARANCE_RATIONALE: Proponents argue that without capital punishment, murder rates would rise, leading to a less safe society. Opponents argue that its disappearance would have no significant impact on crime rates, or might even lead to a more just society, as other forms of punishment are available.
% FOUNDING_PROBLEM: The problem of deterring heinous crimes, particularly murder, and ensuring public safety through the most severe possible punishment.
% FOUNDING_PROBLEM_CORROBORATION: The state security apparatus and some segments of the public attest that the problem of deterrence is live. Criminologists and human rights organizations, from outside the benefiting parties, widely dispute the empirical efficacy of capital punishment as a deterrent, citing extensive research that shows no significant deterrent effect compared to life imprisonment.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).

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
 *   The extractiveness (0.6) is moderate because the 'benefit' of deterrence is diffuse and empirically weak, while the cost to the offender is absolute. Suppression (0.7) is high due to the state's monopoly on legitimate violence and the irreversible nature of the punishment. The theater ratio (0.4) reflects the performative aspect of executions, which are often public spectacles intended to reinforce the deterrence message, even as the empirical evidence for deterrence remains contested. Resistance (0.8) is high due to strong opposition from human rights groups and a significant portion of the public.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state security apparatus, the constraint is a necessary, albeit severe, tool for public safety. From the perspective of convicted offenders and their families, it is pure, irreversible extraction. Criminologists often view it as an ineffective and costly policy, while abolitionist advocates see it as a fundamental violation of human rights.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and, theoretically, potential future victims are beneficiaries (d near 0.0-0.2), as they are meant to gain from increased public safety. Convicted offenders and their families are clear targets (d near 1.0), bearing the ultimate cost. Criminologists and abolitionist advocates are observers or excluded, with their directionality determined by their ability to influence policy or their direct exposure to the constraint's effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (deterrence) is highly contested. If empirical evidence definitively disproves deterrence, the constraint would shift from a Tangled Rope to a Snare, as its coordination function would collapse, leaving only asymmetric extraction. The persistence of the constraint despite weak empirical support suggests a degree of institutional inertia and a performative aspect (theater_ratio 0.4) that maintains its existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_deterrence_validity,
    'Does capital punishment actually deter murder more effectively than life imprisonment?',
    'Longitudinal, cross-jurisdictional empirical studies controlling for confounding variables, with consensus among criminological experts.',
    'If deterrence is empirically disproven, the constraint''s coordination function collapses, reclassifying it from Tangled Rope to Snare. If proven, its coordination function is strengthened, potentially moving it closer to a Rope (though still extractive).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_deterrence_validity, empirical, 'The empirical validity of the deterrence premise.').

omega_variable(
    instrumentalization_ethics,
    'Is it ethically permissible to instrumentalize an individual (the offender) as a means to a social end (deterrence), regardless of their culpability?',
    'Philosophical consensus on deontological ethics and human rights principles, or a shift in societal moral norms.',
    'If instrumentalization is deemed unethical, the constraint''s legitimacy is fundamentally undermined, regardless of empirical deterrence, pushing it towards a Snare or even a Mountain of injustice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_ethics, conceptual, 'Ethical permissibility of instrumentalizing offenders for deterrence.').

omega_variable(
    deterrence_vs_retribution_framing,
    'Is the primary justification for capital punishment in practice deterrence, retribution, or a blend, and how does this framing affect its perceived legitimacy?',
    'Analysis of judicial opinions, legislative debates, and public discourse to identify the dominant justifications invoked by proponents.',
    'If retribution is the dominant underlying justification, the constraint should be re-analyzed under the ''retributive_reading'' kernel, which has different structural properties and ethical considerations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_retribution_framing, conceptual, 'Dominant justification for capital punishment (deterrence vs. retribution).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_legitimacy__deterrence_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(stat_tr_t1990, state_killing_legitimacy__deterrence_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(stat_tr_t2000, state_killing_legitimacy__deterrence_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(stat_tr_t2010, state_killing_legitimacy__deterrence_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(stat_tr_t2024, state_killing_legitimacy__deterrence_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1976, 0.65).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(stat_be_t2000, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(stat_be_t2024, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1976, 0.75).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(stat_su_t2000, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(stat_su_t2024, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'deterrence reading' of the 'state_killing_legitimacy' kernel. It is distinct from the 'retributive_reading' and 'abolition_reading' due to differing core justifications and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
