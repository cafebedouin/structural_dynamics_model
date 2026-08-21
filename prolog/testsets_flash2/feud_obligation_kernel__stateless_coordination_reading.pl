% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligations (Stateless Coordination Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.2).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.3).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligations (Stateless Coordination Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '2f7c546c-b42e-4546-acba-2df5938a3efd').
narrative_ontology:cs_kernel_codification('2f7c546c-b42e-4546-acba-2df5938a3efd', implicit).
narrative_ontology:cs_authority_grounding('2f7c546c-b42e-4546-acba-2df5938a3efd', practice).
narrative_ontology:cs_interpretation_layer_present('2f7c546c-b42e-4546-acba-2df5938a3efd').
narrative_ontology:cs_reading_relation('2f7c546c-b42e-4546-acba-2df5938a3efd', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f7c546c-b42e-4546-acba-2df5938a3efd', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('2f7c546c-b42e-4546-acba-2df5938a3efd', foundational, kin_group_sovereignty_in_justice).
narrative_ontology:cs_axiom_status(kin_group_sovereignty_in_justice, holdable).
narrative_ontology:cs_axiom_grounding('2f7c546c-b42e-4546-acba-2df5938a3efd', kin_group_sovereignty_in_justice, conventional).
narrative_ontology:cs_axiom('2f7c546c-b42e-4546-acba-2df5938a3efd', foundational, retribution_as_deterrence).
narrative_ontology:cs_axiom_status(retribution_as_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('2f7c546c-b42e-4546-acba-2df5938a3efd', retribution_as_deterrence, empirically_contingent).
narrative_ontology:cs_reference_frame('2f7c546c-b42e-4546-acba-2df5938a3efd', stateless_kin_justice_system).
narrative_ontology:cs_drift_state('2f7c546c-b42e-4546-acba-2df5938a3efd', contemporary_analytical_perspective, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f7c546c-b42e-4546-acba-2df5938a3efd', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_justice).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, community_members_seeking_deterrence).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors_from_feud_obligations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, wergild_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups receive a form of justice and redress for wrongs committed against their members, ensuring that offenses do not go unpunished in the absence of state authority. Their honor and social standing are maintained through adherence to the feud.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_justice, beneficiary,
    organized, generational, constrained, local).

% Benefit from the deterrent effect of the feud system, which discourages offenses due to the predictable and severe consequences. They participate in the social enforcement of the system, even if not directly involved in a feud.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, community_members_seeking_deterrence, beneficiary,
    moderate, biographical, constrained, local).

% Individuals or kin groups who fail to uphold their feud obligations face severe social sanctions, including loss of honor, expulsion from their kin network, and lack of protection. They pay the cost of social ostracism and vulnerability.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors_from_feud_obligations, payer,
    powerless, immediate, identity_locked, local).

% While not a centralized authority, these figures often mediate disputes, negotiate wergild (blood money) payments, and interpret customary law, guiding the feud process towards resolution or alternative forms of justice. They administer the customary norms.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, community_elders_or_arbiters, agenda_setter,
    powerful, biographical, mobile, local).

% Specialized individuals or groups who facilitate the payment of wergild as an alternative to ongoing feuds. Their existence demonstrates the flexibility and non-suppressive nature of the system, offering a less violent path to resolution.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_negotiators, beneficiary,
    moderate, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized mechanism for maintaining social order and administering justice in societies lacking formal state institutions, ensuring accountability for offenses and deterring future transgressions.
% TRANSFER_FUNCTION: Transfers the burden of retribution from the wronged party to the offending party (or their kin), often through violence or symbolic acts, ultimately aiming for a rebalancing of honor or a negotiated settlement (wergild).
% ABSENT_VOICES: Individuals or families who might prefer to avoid the cycle of violence entirely, even at the cost of honor, but are compelled by social norms and kin obligations. Their voices are suppressed by the strong identity-lock of kinship.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight in a stateless society, the immediate result would be a collapse of the existing justice and deterrence system, leading to unchecked offenses, widespread insecurity, and a rapid search for alternative, potentially more chaotic, forms of self-help or emergent authority.
% FOUNDING_PROBLEM: The fundamental problem of maintaining social order, deterring crime, and achieving justice for wrongs in societies without a centralized state or formal legal apparatus capable of enforcing laws.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of stateless societies and historical accounts of early medieval Europe corroborate that the problem of maintaining order without a state is a persistent challenge, and that such systems emerged as functional responses. These sources are external to the direct beneficiaries of the feud itself.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_cost_benefit_balance,
    'Does the ''justice and deterrence'' benefit of the feud system genuinely outweigh its social and economic costs (e.g., loss of life, depletion of productive capacity, intergenerational trauma)?',
    'Quantitative historical and anthropological studies comparing societies with and without feud systems, or with different forms of dispute resolution, to assess net societal welfare outcomes.',
    'If costs consistently outweigh benefits, the constraint would shift towards a more extractive classification (e.g., Tangled Rope or Snare), as the coordination story would be undermined by the negative sum game it creates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_cost_benefit_balance, empirical, 'Assessing the true net impact of feud obligations on societal well-being.').

omega_variable(
    suppression_of_alternatives,
    'To what extent were alternatives to feud (e.g., wergild, arbitration) genuinely unsuppressed, or were they only viable for certain social strata or under specific conditions, with feud remaining the dominant and enforced norm for others?',
    'Detailed historical and archaeological analysis of dispute resolution patterns, including frequency of wergild vs. feud, social status of participants, and enforcement mechanisms for each.',
    'If alternatives were more suppressed than currently assessed, the constraint''s suppression metric would increase, potentially shifting it towards a Tangled Rope or Snare classification, indicating less genuine choice for participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives, empirical, 'Clarifying the true accessibility and viability of non-feud dispute resolution.').

omega_variable(
    kernel_reading_stateless_coordination,
    'Is this constraint a genuine self-enforcing coordination mechanism, or is it better understood as a destructive extraction cycle (extraction_cycle_reading) or a violation of divine/royal authority (christianized_pacification_reading)?',
    'This is a conceptual omega, resolved by adopting a specific analytical framework. The ''stateless coordination'' reading prioritizes the functional aspects in the absence of a state. Resolution depends on the normative and empirical lens applied.',
    'Adopting the ''extraction_cycle_reading'' would significantly increase extractiveness and suppression, likely reclassifying to Snare. Adopting the ''christianized_pacification_reading'' would frame it as illegitimate and highly suppressive of individual agency, also likely shifting to Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_stateless_coordination, conceptual, 'This constraint is one reading of the ''feud_obligation_kernel''. This omega documents the core conceptual contest with sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t25, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(feud_tr_t75, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(feud_be_t25, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(feud_be_t75, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 75, 0.2).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(feud_su_t25, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.3).
narrative_ontology:measurement(feud_su_t75, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 75, 0.3).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.08).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
