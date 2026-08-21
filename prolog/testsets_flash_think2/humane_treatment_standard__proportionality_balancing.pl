% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality_balancing' reading of
 *   Common Article 3, which mandates a careful weighing of detainee dignity
 *   against legitimate state security needs. It explicitly rejects both
 *   absolute prohibitions on certain interrogation techniques and unlimited
 *   discretion for state actors. This reading positions courts as
 *   gatekeepers, deciding treatment permissibility case-by-case, aiming for a
 *   moderate constraint on interrogators with procedural safeguards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.6).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.7).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, 'c72344f6-b2ee-4fb8-ae5d-86ed4811d19e').
narrative_ontology:cs_kernel_codification('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', formalized).
narrative_ontology:cs_authority_grounding('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', lineage).
narrative_ontology:cs_interpretation_layer_present('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e').
narrative_ontology:cs_reading_relation('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_axiom('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', foundational, dignity_and_security_require_balancing).
narrative_ontology:cs_axiom_status(dignity_and_security_require_balancing, holdable).
narrative_ontology:cs_axiom_grounding('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', dignity_and_security_require_balancing, conventional).
narrative_ontology:cs_axiom('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', foundational, judicial_review_is_essential_for_balance).
narrative_ontology:cs_axiom_status(judicial_review_is_essential_for_balance, holdable).
narrative_ontology:cs_axiom_grounding('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', judicial_review_is_essential_for_balance, conventional).
narrative_ontology:cs_reference_frame('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', post_geneva_conventions_era).
narrative_ontology:cs_drift_state('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', post_9_11_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c72344f6-b2ee-4fb8-ae5d-86ed4811d19e', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, judicial_system).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for national security, they seek to maximize intelligence gathering and operational effectiveness. This standard constrains their methods but provides a legal framework for permissible actions, preventing absolute prohibitions.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_security_agencies, agenda_setter,
    institutional, immediate, constrained, national).

% Individuals held by state authorities, whose fundamental dignity and rights are subject to a balancing act against security imperatives. They bear the direct cost of any compromises in humane treatment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees, payer,
    powerless, immediate, trapped, local).

% Acts as the primary interpreter and enforcer of the proportionality standard, adjudicating specific cases and setting precedents. It benefits from having a framework to manage the tension between security and rights.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, judicial_system, agenda_setter,
    institutional, generational, analytical, national).

% Work to protect detainee rights and ensure humane treatment. They bear the cost of the proportionality framework when it permits actions they view as infringing on fundamental dignity, and must constantly challenge its application.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_advocates, payer,
    organized, generational, constrained, global).

% Monitor state compliance with international humanitarian law, including Common Article 3. They provide guidance and issue non-binding opinions, influencing the interpretation and application of the standard.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and ethical framework for states to navigate the inherent tension between legitimate national security needs and the fundamental human dignity of detainees, aiming to prevent both arbitrary cruelty and unworkable absolute prohibitions.
% TRANSFER_FUNCTION: Transfers some degree of detainee dignity, autonomy, and protection to state security interests, in exchange for a legally defined (though often contested) boundary on permissible treatment and a mechanism for judicial oversight.
% ABSENT_VOICES: Detainees themselves, whose direct experiences and perspectives on the proportionality of treatment are often mediated through legal counsel or state reports. Victims of state security overreach, whose voices may be suppressed or marginalized in the legal discourse.
% DISAPPEARANCE_RATIONALE: If this proportionality balancing standard vanished overnight, states would likely revert to either claiming unlimited discretion in security matters (leading to widespread abuses and a collapse of IHL norms) or, less likely, face absolute prohibitions that could be seen as unworkable. The legal and ethical landscape of conflict and detention would fundamentally reorganize.
% FOUNDING_PROBLEM: To establish a minimum standard of humane treatment for persons not taking an active part in hostilities, particularly in non-international armed conflicts, where traditional Geneva Conventions might not fully apply, while acknowledging states' legitimate security concerns and the need for a flexible framework.
% FOUNDING_PROBLEM_CORROBORATION: International courts, human rights organizations, and legal scholars (outside state security agencies) consistently affirm the ongoing need for such a standard to prevent abuses while allowing for security operations, even while disputing its precise application and outcomes.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is moderate-to-high because, in practice, the 'balancing' often allows for some infringement on detainee dignity in the name of security, making detainees net payers. Suppression (0.7) is substantial as the standard requires active judicial enforcement to prevent states from claiming unlimited discretion. Theater ratio (0.4) reflects that while genuine balancing occurs, there's also a tendency for states to frame security needs in ways that justify more intrusive measures, leading to performative compliance. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the ongoing tension and states' persistent efforts to push the boundaries of permissible action.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state security agencies, this standard is a necessary, flexible tool for operating in complex environments. From the perspective of human rights advocates, it represents a dangerous compromise that can be exploited to justify abuses. The engine's per-seat classification will reflect this divergence, with the operator seat computing a more 'rope-like' experience and the payer seats computing a more 'snare-like' experience.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies and the judicial system are beneficiaries, as this reading provides a workable legal framework that accommodates security needs while maintaining a semblance of legality. Detainees and human rights advocates are victims, as they bear the costs of dignity compromises and must constantly challenge the application of the balancing act. The judicial system, while a beneficiary of the framework, also acts as an agenda-setter, shaping its interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_definition_ambiguity,
    'What constitutes ''proportional'' balancing in specific, high-stakes security contexts, and how is this objectively measured or adjudicated?',
    'Development of clear, internationally agreed-upon metrics and independent expert panels for assessing proportionality in practice, or a shift towards a more prescriptive, less discretionary legal framework.',
    'If proportionality remains ill-defined, the standard risks becoming a ''snare'' where security claims consistently override dignity. Clearer definitions would strengthen its ''tangled_rope'' function by reducing arbitrary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_definition_ambiguity, conceptual, 'Ambiguity in defining and measuring ''proportionality'' in practice.').

omega_variable(
    judicial_discretion_scope,
    'To what extent does judicial discretion in applying the proportionality standard lead to inconsistent or politically influenced outcomes, rather than principled balancing?',
    'Empirical analysis of judicial rulings across different jurisdictions and political climates, or the establishment of supra-national appellate bodies with binding authority to harmonize interpretations.',
    'If discretion leads to inconsistent outcomes, the constraint''s effectiveness as a ''tangled_rope'' is undermined, and it may function more as a ''snare'' for detainees in certain contexts. Reduced discretion would enhance its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_scope, empirical, 'Impact of judicial discretion on consistent application of the standard.').

omega_variable(
    security_imperative_framing,
    'Are ''security needs'' genuinely assessed and balanced, or are they often framed as absolute imperatives to justify otherwise impermissible treatment?',
    'Independent oversight mechanisms with powers of investigation and subpoena, and a legal culture that prioritizes transparency and accountability over deference to security claims.',
    'If security needs are routinely exaggerated or used as pretexts, the constraint''s ''tangled_rope'' function degrades into a ''snare'', with the coordination story serving as cover for extraction. Robust oversight would push it closer to a ''rope'' or ''scaffold'' if temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_imperative_framing, empirical, 'Risk of security claims being used as pretexts for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.3).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.33).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.36).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__proportionality_balancing, theater_ratio, 30, 0.4).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__proportionality_balancing, theater_ratio, 40, 0.43).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__proportionality_balancing, theater_ratio, 50, 0.46).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__proportionality_balancing, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__proportionality_balancing, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__proportionality_balancing, base_extractiveness, 50, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__proportionality_balancing, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__proportionality_balancing, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__proportionality_balancing, suppression_requirement, 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, interrogation_guidelines).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, detention_policy).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('proportionality_balancing') of the 'humane_treatment_standard' kernel, which also includes 'absolute_prohibition' and 'contextual_necessity' readings. Each reading instantiates a distinct constraint with its own ε and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
