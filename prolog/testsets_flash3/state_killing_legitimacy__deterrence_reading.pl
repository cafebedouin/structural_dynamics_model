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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Execution as Deterrent Signal
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence' reading of state killing
 *   legitimacy, where execution is justified primarily as a rational signal
 *   to prevent future murders. It instrumentalizes the convicted offender as
 *   a means to a social end. The empirical basis for this claim is highly
 *   contested, leading to a moderate-to-high extractiveness score due to the
 *   irreversible nature of the 'cost' (life) and the uncertain 'benefit'
 *   (deterrence). This is one reading of the 'state_killing_legitimacy'
 *   kernel, alongside 'retributive_reading' and 'abolition_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.65).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.7).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Execution as Deterrent Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'df393a58-c9d9-4285-921b-dec354f3ac56').
narrative_ontology:cs_kernel_codification('df393a58-c9d9-4285-921b-dec354f3ac56', formalized).
narrative_ontology:cs_authority_grounding('df393a58-c9d9-4285-921b-dec354f3ac56', lineage).
narrative_ontology:cs_interpretation_layer_present('df393a58-c9d9-4285-921b-dec354f3ac56').
narrative_ontology:cs_reading_relation('df393a58-c9d9-4285-921b-dec354f3ac56', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('df393a58-c9d9-4285-921b-dec354f3ac56', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('df393a58-c9d9-4285-921b-dec354f3ac56', foundational, execution_deters_future_crimes).
narrative_ontology:cs_axiom_status(execution_deters_future_crimes, holdable).
narrative_ontology:cs_axiom_grounding('df393a58-c9d9-4285-921b-dec354f3ac56', execution_deters_future_crimes, empirically_contingent).
narrative_ontology:cs_axiom('df393a58-c9d9-4285-921b-dec354f3ac56', secondary, state_has_right_to_protect_citizens_via_deterrence).
narrative_ontology:cs_axiom_status(state_has_right_to_protect_citizens_via_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('df393a58-c9d9-4285-921b-dec354f3ac56', state_has_right_to_protect_citizens_via_deterrence, deontological).
narrative_ontology:cs_reference_frame('df393a58-c9d9-4285-921b-dec354f3ac56', rational_state_deterrence_model).
narrative_ontology:cs_drift_state('df393a58-c9d9-4285-921b-dec354f3ac56', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('df393a58-c9d9-4285-921b-dec354f3ac56', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_prosecutors).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, convicted_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, offenders_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for capital punishment, arguing its necessity for public safety and crime prevention. They are responsible for seeking and executing death sentences, framing them as a necessary deterrent.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Are the direct targets of the constraint, losing their lives. They are instrumentalized as a means to a social end (deterrence), with no agency in the process once convicted.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, convicted_offenders, payer,
    powerless, immediate, trapped, local).

% Are the theoretical beneficiaries, as their lives are purportedly saved by the deterrent effect of executions. Their benefit is diffuse and statistical, not direct.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, analytical, national).

% Bear the emotional and social costs of the execution, experiencing profound loss and stigma. They have limited legal recourse to prevent the execution once a sentence is final.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, offenders_families, payer,
    powerless, generational, constrained, local).

% Actively campaign against capital punishment, citing its ineffectiveness as a deterrent, its moral implications, and the risk of executing innocent individuals. Their arguments are often marginalized in jurisdictions committed to deterrence.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, death_penalty_abolitionists, excluded,
    organized, generational, constrained, global).

% Conduct empirical studies on the deterrent effect of capital punishment. Their findings often show no significant deterrent effect, challenging the foundational premise of this reading.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, social_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal behavior by establishing a clear, severe consequence for murder, thereby deterring potential future offenders and maintaining public order.
% TRANSFER_FUNCTION: Transfers the life of the convicted offender from their control to the state, with the implicit promise of transferring safety to potential future victims by deterring crime.
% ABSENT_VOICES: The voices of those who would be deterred are inherently absent, as they are hypothetical. The voices of those who have been wrongly convicted and executed are permanently silenced. Abolitionist arguments are often excluded from policy debates in retentionist states.
% DISAPPEARANCE_RATIONALE: Proponents argue that without capital punishment, murder rates would rise, leading to societal chaos. Opponents argue that its disappearance would have no measurable impact on crime rates, and might even improve the moral standing of the state. The empirical evidence is inconclusive and highly debated.
% FOUNDING_PROBLEM: The problem of preventing heinous crimes and maintaining public safety through the most severe possible punishment.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutors and some public segments attest the problem is live and capital punishment is a necessary tool. Social scientists and abolitionist organizations, from outside the benefiting parties, present extensive empirical evidence suggesting the problem is not solved by capital punishment, or that the problem itself is framed incorrectly.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is high because the cost to the victim (life) is absolute, while the benefit (deterrence) is empirically unproven and often disputed. Suppression (0.70) is high due to the state's monopoly on legitimate force and the finality of the act. Theater ratio (0.40) reflects the performative aspect of executions, intended to send a message, even if the actual deterrent effect is low. The claimed type is 'tangled_rope' because it attempts to coordinate society (deterrence) but does so through asymmetric extraction (life of offender) and requires active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state prosecutors, this is a necessary, albeit tragic, coordination mechanism for public safety. From the perspective of convicted offenders and their families, it is pure extraction. Social scientists often view it as an empirically unsupported claim. The engine will compute these divergent classifications based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State prosecutors act as agenda-setters, benefiting from the perceived order and justice. Convicted offenders are the ultimate payers, losing their lives. Potential future victims are the diffuse beneficiaries. Offenders' families bear significant costs. Social scientists and abolitionists act as observers or excluded voices, challenging the premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_deterrence_validity,
    'Does capital punishment actually deter murder more effectively than life imprisonment?',
    'Longitudinal, cross-jurisdictional empirical studies with robust controls for confounding variables, and meta-analyses of such studies.',
    'If deterrence is empirically disproven, the foundational premise of this reading collapses, reclassifying it closer to a Snare or Piton, as its coordination function would be revealed as cover for pure extraction or inertia. If proven, its extractiveness might be re-evaluated downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_deterrence_validity, empirical, 'Uncertainty regarding the empirical validity of the deterrence claim.').

omega_variable(
    instrumentalization_morality,
    'Is it morally permissible to instrumentalize a human life (the convicted offender) as a means to a social end (deterrence)?',
    'Philosophical and ethical debate, potentially leading to shifts in societal values and legal frameworks.',
    'If instrumentalization is deemed morally impermissible, this reading would be foreclosed by the abolition_reading, regardless of empirical deterrence, shifting its classification towards a Snare or Piton based on a moral rather than empirical failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_morality, preference, 'Ethical debate on the instrumentalization of human life for societal benefit.').

omega_variable(
    deterrence_vs_retribution_dominance,
    'In practice, is the primary justification for capital punishment in a given jurisdiction deterrence or retribution?',
    'Analysis of judicial opinions, legislative debates, and public discourse in specific jurisdictions. This is a conceptual distinction that often blurs in practice.',
    'If retribution is the dominant justification, this reading''s claimed coordination function (deterrence) is revealed as secondary or theatrical, pushing it towards a Piton or Snare if the deterrence claim is merely cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_retribution_dominance, conceptual, 'Ambiguity in the primary justification for capital punishment in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__deterrence_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__deterrence_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__deterrence_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_killing_legitimacy' kernel. Each reading presents a distinct justification and structural profile for capital punishment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
