% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Definition (Geneva Article 4 Reading)
 *   domain: international_humanitarian_law/political
 *
 * SUMMARY:
 *   This constraint defines combatant status strictly according to Geneva
 *   Convention Article 4, requiring formal state military organization. It
 *   categorically excludes non-state armed groups from Prisoner of War (POW)
 *   protections, treating them as unlawful combatants or criminals. This is
 *   one reading of the 'combatant_status_definition' kernel, emphasizing
 *   state sovereignty and control over the legitimate use of force. The high
 *   extractiveness and suppression reflect the severe consequences for
 *   non-state actors who fall outside this narrow definition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.85).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.92).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, snare).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Definition (Geneva Article 4 Reading)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law/political").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '04315e0c-4f46-4487-813f-e3fd1944f214').
narrative_ontology:cs_kernel_codification('04315e0c-4f46-4487-813f-e3fd1944f214', fixed_text).
narrative_ontology:cs_authority_grounding('04315e0c-4f46-4487-813f-e3fd1944f214', lineage).
narrative_ontology:cs_interpretation_layer_present('04315e0c-4f46-4487-813f-e3fd1944f214').
narrative_ontology:cs_reading_relation('04315e0c-4f46-4487-813f-e3fd1944f214', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('04315e0c-4f46-4487-813f-e3fd1944f214', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('04315e0c-4f46-4487-813f-e3fd1944f214', foundational, state_monopoly_on_legitimate_force).
narrative_ontology:cs_axiom_status(state_monopoly_on_legitimate_force, holdable).
narrative_ontology:cs_axiom_grounding('04315e0c-4f46-4487-813f-e3fd1944f214', state_monopoly_on_legitimate_force, conventional).
narrative_ontology:cs_axiom('04315e0c-4f46-4487-813f-e3fd1944f214', foundational, formal_organization_as_prerequisite_for_status).
narrative_ontology:cs_axiom_status(formal_organization_as_prerequisite_for_status, holdable).
narrative_ontology:cs_axiom_grounding('04315e0c-4f46-4487-813f-e3fd1944f214', formal_organization_as_prerequisite_for_status, conventional).
narrative_ontology:cs_reference_frame('04315e0c-4f46-4487-813f-e3fd1944f214', post_geneva_conventions_1949).
narrative_ontology:cs_drift_state('04315e0c-4f46-4487-813f-e3fd1944f214', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('04315e0c-4f46-4487-813f-e3fd1944f214', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_military_personnel).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, insurgent_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As signatories, they define and enforce combatant status, benefiting from the clarity and control it provides over who is afforded POW protections. They actively prosecute non-state fighters under domestic law.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Receive full Prisoner of War (POW) protections under this definition, ensuring humane treatment and repatriation if captured. Their status is clear and internationally recognized.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_military_personnel, beneficiary,
    organized, biographical, mobile, global).

% Categorically denied POW status, they are treated as criminals or unlawful combatants if captured, subject to domestic prosecution, torture, or extrajudicial killing. Their only 'exit' is surrender or death.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters, payer,
    powerless, immediate, trapped, regional).

% Their members are denied POW status, which severely hampers their ability to wage war and protect their personnel. They face a fundamental legitimacy challenge under this reading of IHL.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, insurgent_movements, payer,
    moderate, generational, constrained, regional).

% Applies IHL principles in prosecuting war crimes, but its jurisdiction and interpretation of combatant status can be influenced by state-centric definitions, even while acknowledging broader humanitarian principles.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_criminal_court, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, internationally recognized framework for distinguishing legitimate combatants from criminals, thereby coordinating state behavior in armed conflict and providing a basis for reciprocal treatment of captured personnel.
% TRANSFER_FUNCTION: Transfers the right to humane treatment and POW protections from non-state armed group fighters to state military personnel, effectively granting immunity to one group while denying it to another based on organizational structure.
% ABSENT_VOICES: Non-state armed groups and their political representatives are largely excluded from the formal diplomatic processes that define and revise IHL, though they are the primary targets of this definition's exclusionary force. They would argue for status based on functional criteria rather than state affiliation.
% DISAPPEARANCE_RATIONALE: If this state-centric definition vanished, the legal landscape of armed conflict would fundamentally shift. Non-state actors would immediately claim POW protections, states would lose a key legal tool for prosecuting them, and the distinction between 'war' and 'crime' would blur, forcing a re-evaluation of international law.
% FOUNDING_PROBLEM: To regulate warfare between sovereign states by establishing clear rules for who is a legitimate combatant and how they should be treated if captured, thereby limiting brutality and ensuring reciprocity.
% FOUNDING_PROBLEM_CORROBORATION: State parties to the Geneva Conventions universally attest that the problem of regulating inter-state conflict and protecting their own military personnel remains live. Non-state actors and some IHL scholars contest that the definition's narrowness now exacerbates conflict rather than regulating it, by denying protections to a growing class of combatants.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because non-state fighters are denied fundamental protections, facing prosecution or worse. Suppression is very high (0.92) as states actively enforce this distinction through military action, legal frameworks, and diplomatic pressure, suppressing any alternative claims to status. Theater ratio is low (0.1) because the enforcement is direct and consequential, not merely performative. The historical measurements show a rise in extractiveness and suppression, particularly after the Cold War, as conflicts increasingly involved non-state actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state parties, this definition is a necessary coordination mechanism for international order and military discipline. From the perspective of non-state fighters, it is a snare designed to delegitimize their struggle and deny them basic human rights in conflict. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and their military personnel are clear beneficiaries, gaining legal clarity and protection. Non-state armed group fighters and insurgent movements are the primary victims, bearing the full cost of exclusion. The ICC acts as an observer, applying the law but also subject to its state-centric biases.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_non_state_actors,
    'Does the categorical exclusion of non-state actors from POW status undermine the broader humanitarian goals of IHL by incentivizing greater brutality or denying protections to legitimate resistance movements?',
    'Empirical study of conflict outcomes in contexts with high non-state actor involvement, comparing adherence to IHL principles by states and non-state groups under different legal interpretations.',
    'If it is found to undermine humanitarian goals, it would challenge the normative justification for the high extractiveness, potentially reclassifying the constraint as a more severe snare or even a piton if its original coordination function is entirely subverted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_non_state_actors, empirical, 'Whether the state-centric definition is counterproductive to IHL''s aims.').

omega_variable(
    interpretation_of_organized_groups,
    'How ''organized'' must a non-state armed group be to functionally meet the spirit of Article 4 criteria, even if not formally part of a state''s military?',
    'Development of international legal precedents or scholarly consensus on functional criteria for organization and command structure, independent of state affiliation.',
    'A broader functional interpretation would reduce the extractiveness for some non-state actors, potentially shifting the constraint towards a tangled rope or even a rope for those groups that meet the functional criteria.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_of_organized_groups, conceptual, 'Ambiguity in ''organized'' criteria for non-state groups.').

omega_variable(
    state_sovereignty_vs_humanitarian_protection,
    'To what extent should state sovereignty and the state''s monopoly on legitimate force override the imperative for universal humanitarian protection in armed conflict?',
    'Ongoing international legal and political debate, potentially leading to new treaties or customary international law that rebalances these principles.',
    'A shift towards prioritizing universal protection would fundamentally alter the constraint''s structure, likely reducing its extractiveness and suppression for non-state actors. A reaffirmation of state sovereignty would entrench the current high extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_sovereignty_vs_humanitarian_protection, preference, 'The normative tension between state sovereignty and universal humanitarian protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__state_centric_reading, theater_ratio, 1977, 0.08).
narrative_ontology:measurement(comb_tr_t1991, combatant_status_definition__state_centric_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__state_centric_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__state_centric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.7).
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.75).
narrative_ontology:measurement(comb_be_t1991, combatant_status_definition__state_centric_reading, base_extractiveness, 1991, 0.8).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.88).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__state_centric_reading, base_extractiveness, 2010, 0.87).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__state_centric_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.7).
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.75).
narrative_ontology:measurement(comb_su_t1991, combatant_status_definition__state_centric_reading, suppression_requirement, 1991, 0.85).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.95).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__state_centric_reading, suppression_requirement, 2010, 0.93).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__state_centric_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'state_centric_reading' of the 'combatant_status_definition' kernel. It is linked to the 'national_liberation_reading' and 'functional_protection_reading' as sibling interpretations of the same core legal concept, each with different implications for who receives protection in armed conflict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
