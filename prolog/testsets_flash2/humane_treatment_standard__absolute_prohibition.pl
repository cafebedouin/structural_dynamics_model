% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition of Torture (Common Article 3 Reading)
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'absolute prohibition' reading of Common
 *   Article 3 of the Geneva Conventions, which establishes non-derogable
 *   minimum standards for humane treatment in armed conflict. Under this
 *   reading, no circumstances, including national security imperatives,
 *   permit torture or degrading treatment. Detainees are considered full
 *   rights-holders, and state interrogation methods are absolutely
 *   constrained. This reading asserts the constraint as a fundamental,
 *   natural law of human dignity, with negligible extraction or suppression
 *   inherent to its operation, as it simply reflects an irreducible moral
 *   limit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.1).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.05).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.1).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition of Torture (Common Article 3 Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '885c0de4-d474-4874-ade0-81aeda95a615').
narrative_ontology:cs_kernel_codification('885c0de4-d474-4874-ade0-81aeda95a615', fixed_text).
narrative_ontology:cs_authority_grounding('885c0de4-d474-4874-ade0-81aeda95a615', lineage).
narrative_ontology:cs_interpretation_layer_present('885c0de4-d474-4874-ade0-81aeda95a615').
narrative_ontology:cs_reading_relation('885c0de4-d474-4874-ade0-81aeda95a615', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('885c0de4-d474-4874-ade0-81aeda95a615', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('885c0de4-d474-4874-ade0-81aeda95a615', foundational, human_dignity_is_absolute).
narrative_ontology:cs_axiom_status(human_dignity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('885c0de4-d474-4874-ade0-81aeda95a615', human_dignity_is_absolute, deontological).
narrative_ontology:cs_axiom('885c0de4-d474-4874-ade0-81aeda95a615', foundational, no_derogation_from_humane_treatment).
narrative_ontology:cs_axiom_status(no_derogation_from_humane_treatment, holdable).
narrative_ontology:cs_axiom_grounding('885c0de4-d474-4874-ade0-81aeda95a615', no_derogation_from_humane_treatment, deontological).
narrative_ontology:cs_reference_frame('885c0de4-d474-4874-ade0-81aeda95a615', post_wwii_universal_human_rights_framework).
narrative_ontology:cs_drift_state('885c0de4-d474-4874-ade0-81aeda95a615', post_9_11_security_paradigm, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('885c0de4-d474-4874-ade0-81aeda95a615', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, military_personnel).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, universal_human_dignity).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogable_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals held in custody during armed conflict, whose fundamental human dignity and right to humane treatment are protected absolutely by this standard, regardless of their status or perceived threat. They are the direct recipients of the protection.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, immediate, trapped, global).

% States that have ratified the Geneva Conventions and are bound by Common Article 3. This reading requires them to uphold the absolute prohibition of torture and degrading treatment, even in times of armed conflict, without exception. Their role is to ensure compliance within their armed forces and detention facilities.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Organizations like the UN Human Rights Committee and the International Criminal Court, which monitor compliance with international humanitarian law and investigate alleged violations. They interpret and reinforce the absolute nature of the prohibition.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Individual soldiers, interrogators, and commanders who are directly responsible for the treatment of detainees. This standard imposes strict limits on their conduct, requiring adherence to humane treatment even under pressure or in challenging operational environments.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, military_personnel, payer,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable baseline for humane treatment of persons not participating in hostilities, ensuring a minimum standard of dignity and preventing a race to the bottom in conflict situations.
% TRANSFER_FUNCTION: Transfers the absolute right to humane treatment to all detainees, imposing a corresponding absolute duty on state parties and their agents to refrain from torture or degrading treatment, regardless of circumstances.
% ABSENT_VOICES: Those who advocate for 'enhanced interrogation techniques' or 'security exceptions' to human rights in times of conflict are structurally excluded from this reading's framework, as it permits no such derogations. Their arguments are foreclosed by the absolute nature of the prohibition.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished, the legal and ethical landscape of armed conflict would fundamentally shift. States would face immense pressure to adopt more permissive interrogation policies, leading to a rapid increase in torture and degrading treatment, and a collapse of the international human rights framework for detainees.
% FOUNDING_PROBLEM: The widespread atrocities and inhumane treatment of combatants and civilians during World War II, which demonstrated the urgent need for universal, non-derogable standards for the protection of individuals in armed conflict.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and the consistent jurisprudence of international courts corroborate that the problem of inhumane treatment in conflict remains live, necessitating the continued enforcement of absolute prohibitions. Historical records and survivor testimonies from outside state parties also attest to the original problem.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the structural position of this reading: extractiveness is near zero because the prohibition is seen as a fundamental moral limit, not an imposed cost. Suppression is also near zero because it is not actively 'enforced' against legitimate alternatives, but rather reflects a universal moral imperative. Theater ratio is zero as there is no performative aspect to a natural law. Accessibility collapse is high (0.9) because this reading asserts that no legitimate alternatives to humane treatment exist. Resistance is low (0.05) because, from this perspective, any 'resistance' is illegitimate and not a valid challenge to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, there is no legitimate perspectival gap regarding the absolute nature of the prohibition. Any 'alternative' perspective that seeks to justify torture is seen as a violation of the constraint, not a different reading of it. The engine's classification will reflect this 'mountain' claim, and any divergence from other readings will highlight the contestation over the kernel itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are full beneficiaries (d=0.0) as the constraint exists solely for their protection. State parties are agenda-setters (d=0.5) as they are bound to uphold and implement the standard, incurring costs but also gaining legitimacy. Military personnel are payers (d=1.0) as they bear the direct burden of adhering to strict rules of engagement and interrogation. International human rights bodies are observers (d=0.5) as they monitor and interpret without direct enforcement power over individual actions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_compliance_gap,
    'Does the actual practice of state parties consistently align with this absolute prohibition, or is there a persistent gap between declared policy and operational reality?',
    'Comprehensive, independent monitoring of detention facilities and interrogation practices, coupled with robust accountability mechanisms for violations.',
    'If a significant and persistent gap exists, the constraint''s effective extractiveness (from detainees) and suppression (of state agents seeking to comply) would be higher than this reading suggests, potentially reclassifying it as a ''tangled_rope'' or ''snare'' from the perspective of actual practice, despite its ''mountain'' claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_compliance_gap, empirical, 'Gap between the absolute prohibition and real-world state practice.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the absolute prohibition a genuine natural law (a ''mountain'') reflecting irreducible human dignity, or a constructed legal norm (a ''rope'' or ''tangled_rope'') that requires continuous enforcement and is subject to political contestation?',
    'Analysis of the historical evolution of human rights law, philosophical grounding of dignity, and the political processes involved in its adoption and enforcement. The existence of ''contextual_necessity'' and ''proportionality_balancing'' readings suggests it is not universally accepted as a natural law.',
    'If it is primarily a constructed legal norm, its classification would shift from ''mountain'' to a ''rope'' (if genuinely coordinating) or ''tangled_rope'' (if also extractive), acknowledging the ongoing political and legal work required to maintain its status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Natural law vs. constructed norm status of the absolute prohibition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of torture structural (legal prohibitions, institutional oversight) or internalized (moral conviction of military personnel)?',
    'Post-exit suppression trajectory: if military personnel continue to refrain from torture even when legal barriers are removed or oversight is weak, reclassify as partially internalized. If violations increase with weakened external barriers, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the moral norm carries the suppression with them. If purely structural, the constraint is more vulnerable to shifts in legal or political will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for torture prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.0).
narrative_ontology:measurement(huma_tr_t1970, humane_treatment_standard__absolute_prohibition, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(huma_tr_t1990, humane_treatment_standard__absolute_prohibition, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.0).
narrative_ontology:measurement(huma_tr_t2010, humane_treatment_standard__absolute_prohibition, theater_ratio, 2010, 0.0).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__absolute_prohibition, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(huma_be_t1970, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(huma_be_t1990, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.1).
narrative_ontology:measurement(huma_be_t2010, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2010, 0.1).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.05).
narrative_ontology:measurement(huma_su_t1970, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(huma_su_t1990, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.05).
narrative_ontology:measurement(huma_su_t2010, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2010, 0.05).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'humane_treatment_standard' kernel (Common Article 3). Its absolute prohibition stance differs from 'contextual_necessity' and 'proportionality_balancing' readings, which permit derogations or balancing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
