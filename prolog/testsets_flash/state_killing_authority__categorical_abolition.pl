% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition of State Killing Authority
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the philosophical and legal position that
 *   state killing, regardless of the crime committed or its potential
 *   consequences, is inherently impermissible because human life is
 *   inalienable. It asserts a categorical moral limit on state power. This is
 *   one reading of the broader 'state_killing_authority' kernel, specifically
 *   the 'categorical_abolition' reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.05).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.1).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.05).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, mountain).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing Authority").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:emerges_naturally(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, 'bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41').
narrative_ontology:cs_kernel_codification('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', formalized).
narrative_ontology:cs_authority_grounding('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', lineage).
narrative_ontology:cs_interpretation_layer_present('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41').
narrative_ontology:cs_reading_relation('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', foundational, state_power_is_morally_limited).
narrative_ontology:cs_axiom_status(state_power_is_morally_limited, holdable).
narrative_ontology:cs_axiom_grounding('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', state_power_is_morally_limited, deontological).
narrative_ontology:cs_reference_frame('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', universal_human_rights_declaration).
narrative_ontology:cs_drift_state('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bccf2fe8-df0d-4cf0-9e0c-67e9604c8e41', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, human_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, victims_families_abolitionist).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, the_state_as_executor).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, inalienable_right_to_life_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, state_moral_authority_limits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals sentenced to death whose lives are protected by the categorical abolition principle. Their existence is directly contingent on this constraint's recognition.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% Organizations and individuals who champion the inalienable right to life and actively work to abolish capital punishment. This constraint vindicates their core principles.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% The governmental entity that possesses the power to impose and carry out death sentences. This constraint directly limits its authority and action, forcing it to forgo a punitive option.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, the_state_as_executor, payer,
    institutional, generational, constrained, national).

% Families of victims who, despite their loss, advocate against capital punishment, aligning with the categorical abolition principle. They find solace or justice in the state's refusal to kill.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_abolitionist, beneficiary,
    moderate, biographical, constrained, local).

% Families of victims who seek retribution through capital punishment. This constraint excludes their desired form of justice, often leading to feelings of marginalization by the legal system.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_retributivist, excluded,
    moderate, biographical, constrained, local).

% Academics and legal experts who analyze and argue for the categorical impermissibility of state killing, contributing to the intellectual and jurisprudential grounding of the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, legal_scholars_abolitionist, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared moral and legal baseline that human life is inviolable by the state, coordinating state action around a principle of non-killing and fostering a consistent human rights framework.
% TRANSFER_FUNCTION: Transfers the 'right to life' from a potentially forfeitable status (under retributive or deterrence readings) to an inalienable one, thereby transferring the burden of non-killing onto the state and protecting the condemned.
% ABSENT_VOICES: Retributivist and deterrence-focused legal theorists, politicians advocating for 'tough on crime' policies, and victims' families seeking execution are often marginalized or excluded from the core discourse of categorical abolition, as their premises are fundamentally incompatible with it.
% DISAPPEARANCE_RATIONALE: If the principle of categorical abolition vanished, the legal and moral landscape regarding state power would fundamentally shift. States would be free to implement capital punishment without this inherent moral barrier, leading to a re-evaluation of justice systems, human rights norms, and the status of condemned persons globally.
% FOUNDING_PROBLEM: The problem of state overreach and the potential for irreversible injustice, as well as the moral question of whether any human authority can legitimately take a life, even in response to crime.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as evidenced by ongoing debates about state power, human rights violations, and the fallibility of justice systems globally. International human rights bodies, legal scholars, and abolitionist movements consistently corroborate the enduring relevance of this founding problem, independent of state actors who might benefit from expanded punitive authority.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_authority__categorical_abolition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_authority__categorical_abolition),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because it posits an irreducible moral limit on state action, akin to a natural law, rather than a human-made coordination mechanism. Its extractiveness is low (0.05) as it primarily limits state action rather than extracting from individuals. Suppression is low (0.1) because its persistence relies on moral conviction and legal precedent, not active coercion against those who would violate it (though states that violate it face moral and legal condemnation). Theater ratio is 0.0 as there's no performative maintenance; it either holds or it doesn't. Accessibility collapse is high (0.9) because, if accepted, it fundamentally closes off the option of state killing. Resistance is low (0.05) as the resistance is against the act of state killing, not against the principle of abolition itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates and condemned persons, this constraint is a fundamental protection. From the perspective of retributivists or deterrence advocates, it is a moral claim that may conflict with other perceived state duties or societal needs. The engine's classification will reflect the structural reality of the constraint as a limit on state power, rather than the contested moral arguments.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are direct beneficiaries (d=0.0) as their lives are protected. Human rights advocates are also beneficiaries (d=0.0) as their core principles are vindicated. The state, when considering execution, is the target of this constraint (d=1.0), as its power is limited. Victims' families are split: some align with abolition, others with retribution, making their directionality complex and context-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a categorical moral claim, is not subject to mandatrophy in the same way a policy or institution might be. Its mandate is inherent and unchanging: to assert the inalienable right to life. The question is not whether its function has atrophied, but whether its moral claim is accepted or rejected by a given authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_morality,
    'Is the inalienable right to life a natural law (Mountain) or a constructed moral/legal principle (Rope/Tangled Rope)?',
    'Philosophical consensus on meta-ethics, or universal legal codification without coercive enforcement.',
    'If purely constructed, its persistence depends on active defense and could be reclassified as a Rope or Tangled Rope, with identifiable beneficiaries and payers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_morality, conceptual, 'Ambiguity between natural law and constructed moral principle.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''categorical_abolition'' reading of the ''state_killing_authority'' kernel. What would change if the ''retributive_desert'' or ''deterrence_instrument'' readings were adopted?',
    'Analysis of legal and ethical frameworks adopting sibling readings.',
    'Adopting ''retributive_desert'' would shift the condemned person from rights-holder to forfeit-life status, increasing state authority. Adopting ''deterrence_instrument'' would make state killing conditional on empirical efficacy, shifting the constraint from categorical to instrumental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the state_killing_authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel, focusing on the categorical impermissibility of state killing. Sibling readings include 'retributive_desert' and 'deterrence_instrument', which offer alternative justifications or conditions for state killing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
