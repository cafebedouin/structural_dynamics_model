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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Definition (Geneva Article 4)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the state-centric reading of combatant status,
 *   primarily derived from Geneva Convention III Article 4, which grants
 *   Prisoner of War (POW) protections almost exclusively to members of state
 *   armed forces meeting specific criteria. Non-state armed groups and their
 *   individual fighters are categorically excluded from these protections,
 *   leaving them vulnerable to prosecution under domestic law. This reading
 *   is a foundational element of traditional International Humanitarian Law
 *   (IHL) but faces increasing challenge from evolving forms of conflict
 *   involving prominent non-state actors.
 *
 * KEY AGENTS:
 *   - state_governments: Agenda-setter (institutional/constrained) — defines and enforces status
 *   - state_militaries: Beneficiary (institutional/constrained) — receive POW protections
 *   - non_state_armed_groups: Payer (organized/trapped) — denied POW status, members criminalized
 *   - individual_non_state_fighters: Payer (powerless/identity_locked) — face prosecution, no POW immunity
 *   - international_humanitarian_law_scholars: Observer (analytical/analytical) — analyze legal implications and tensions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.85).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.9).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, snare).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Definition (Geneva Article 4)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '13737d68-c592-4ce2-88dd-5b4adf084824').
narrative_ontology:cs_kernel_codification('13737d68-c592-4ce2-88dd-5b4adf084824', fixed_text).
narrative_ontology:cs_authority_grounding('13737d68-c592-4ce2-88dd-5b4adf084824', lineage).
narrative_ontology:cs_interpretation_layer_present('13737d68-c592-4ce2-88dd-5b4adf084824').
narrative_ontology:cs_reading_relation('13737d68-c592-4ce2-88dd-5b4adf084824', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('13737d68-c592-4ce2-88dd-5b4adf084824', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('13737d68-c592-4ce2-88dd-5b4adf084824', foundational, state_monopoly_on_legitimate_force).
narrative_ontology:cs_axiom_status(state_monopoly_on_legitimate_force, holdable).
narrative_ontology:cs_axiom_grounding('13737d68-c592-4ce2-88dd-5b4adf084824', state_monopoly_on_legitimate_force, conventional).
narrative_ontology:cs_axiom('13737d68-c592-4ce2-88dd-5b4adf084824', foundational, reciprocity_as_basis_for_protection).
narrative_ontology:cs_axiom_status(reciprocity_as_basis_for_protection, holdable).
narrative_ontology:cs_axiom_grounding('13737d68-c592-4ce2-88dd-5b4adf084824', reciprocity_as_basis_for_protection, instrumental).
narrative_ontology:cs_reference_frame('13737d68-c592-4ce2-88dd-5b4adf084824', post_geneva_conventions_1949).
narrative_ontology:cs_drift_state('13737d68-c592-4ce2-88dd-5b4adf084824', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('13737d68-c592-4ce2-88dd-5b4adf084824', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_governments).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, individual_non_state_fighters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the criteria for combatant status, primarily through their military and legal systems. They benefit from the ability to prosecute non-state fighters under domestic law, denying them POW protections, which simplifies counter-insurgency operations and maintains state monopoly on legitimate force.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, constrained, global).

% Their personnel are granted Prisoner of War (POW) status and protections under the Geneva Conventions when captured, provided they meet Article 4 criteria (e.g., wearing uniforms, carrying arms openly, responsible command). This provides a crucial layer of protection for their combatants.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, biographical, constrained, global).

% Their members are categorically denied POW status under this reading, regardless of their organization or adherence to the laws of war. This exposes them to prosecution as criminals under domestic law upon capture, increasing the personal risk of combat and hindering their operational legitimacy.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_groups, payer,
    organized, generational, trapped, regional).

% Face the highest personal risk, as they are subject to immediate prosecution and harsh penalties upon capture, without the protections afforded to state combatants. Their identity as fighters for a cause often locks them into this perilous position, with no legal 'exit' from criminalization.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, individual_non_state_fighters, payer,
    powerless, immediate, identity_locked, local).

% Analyze the legal implications and practical consequences of this state-centric interpretation, often highlighting its tension with evolving forms of conflict and the principle of humane treatment for all detainees. They document the gap between legal theory and battlefield reality.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit narrow, framework for distinguishing legitimate combatants from criminals in armed conflict, aiming to regularize warfare between states and provide reciprocal protections for their captured personnel.
% TRANSFER_FUNCTION: Transfers the right to humane treatment and immunity from prosecution for acts of war from non-state fighters to state-organized military personnel, effectively criminalizing non-state combat.
% ABSENT_VOICES: Non-state armed groups and their advocates, who would argue for a more inclusive definition of combatant status based on functional criteria (e.g., adherence to IHL, organized command) rather than solely state affiliation. Their voices are often heard in academic discourse and advocacy, but not in the formal legal frameworks that define status.
% DISAPPEARANCE_RATIONALE: If this state-centric definition vanished, the legal landscape of armed conflict would be fundamentally altered. States would lose a key tool for delegitimizing non-state adversaries, and the protections for state combatants would become less clear, leading to a chaotic re-evaluation of detention and prosecution policies globally.
% FOUNDING_PROBLEM: To regulate warfare between sovereign states, ensuring that captured soldiers were treated humanely and not prosecuted as common criminals, thereby encouraging reciprocity and limiting brutality in interstate conflict.
% FOUNDING_PROBLEM_CORROBORATION: The problem of regulating interstate warfare and protecting captured state combatants remains live, as attested by state governments and international legal bodies. However, the problem of regulating non-state conflict, which this reading largely excludes, is increasingly contested by IHL scholars and human rights organizations, who argue the definition is inadequate for contemporary conflicts.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).

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
 *   The extractiveness is high (0.85) because non-state fighters are stripped of fundamental protections, facing criminal prosecution for acts of war that would be immune for state combatants. Suppression is very high (0.9) as states actively enforce this distinction through legal systems, military doctrine, and diplomatic pressure, effectively criminalizing alternative forms of combat. Theater ratio is low (0.1) because the distinction, while contested, is genuinely applied and enforced by states; it is not merely performative. The rising extractiveness and suppression over time reflect the increasing prominence of non-state actors in modern conflicts and the hardening of state positions against granting them equivalent status.
 *
 * PERSPECTIVAL GAP:
 *   State governments and militaries experience this as a legitimate and necessary framework for regulating warfare and protecting their own personnel. Non-state armed groups and their fighters experience it as a snare, designed to delegitimize and criminalize their actions, denying them basic protections under IHL. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and militaries are clear beneficiaries (d near 0.0) as they gain legal and operational advantages. Non-state armed groups and individual fighters are clear targets (d near 1.0) as they bear the full cost of criminalization and lack of protection. IHL scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophic in the traditional sense, as the founding problem of regulating interstate warfare remains live. However, its application to contemporary conflicts, which increasingly involve non-state actors, reveals a functional mismatch. The constraint's persistence in its state-centric form, despite the rise of non-state warfare, indicates a 'functional obsolescence' for a significant portion of modern conflict, leading to its classification as a snare for non-state actors. The engine's classification prevents mislabeling this as a simple 'rope' for all parties, by highlighting the severe asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_centric_vs_functional_necessity,
    'Is the state-centric definition of combatant status a structural necessity for maintaining order in armed conflict, or is it a policy choice that primarily benefits states?',
    'Comparative analysis of conflicts where functional criteria (e.g., adherence to IHL, organized command) have been applied to non-state actors, assessing impacts on conflict duration, civilian protection, and prisoner treatment.',
    'If a functional approach proves viable without undermining order, the state-centric reading''s high extractiveness would be re-evaluated as a policy-driven snare rather than a coordination necessity. If functional approaches lead to greater chaos, it would reinforce the state-centric reading''s coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_vs_functional_necessity, empirical, 'Assesses whether the state-centric definition is a functional necessity or a state-benefiting policy.').

omega_variable(
    legitimacy_of_non_state_combat,
    'To what extent do non-state armed groups, particularly those fighting for self-determination or against oppressive regimes, possess a legitimate claim to combatant status under evolving international law?',
    'Analysis of evolving state practice, UN resolutions, and jurisprudence from international criminal tribunals regarding the status of non-state actors in specific contexts (e.g., national liberation movements, anti-apartheid struggles).',
    'If a strong trend towards recognizing the legitimacy of certain non-state combat emerges, the ''snare'' classification for non-state fighters would be reinforced, highlighting the gap between legal reality and normative aspiration. If states consistently reject such claims, it would underscore the enduring power of the state-centric reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_non_state_combat, conceptual, 'Examines the normative legitimacy of non-state combatants in IHL.').


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
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__state_centric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.7).
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.75).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.8).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__state_centric_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.75).
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.8).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__state_centric_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, national_liberation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'combatant_status_definition' kernel. This state-centric reading directly influences and is in tension with the 'national_liberation_reading' and 'functional_protection_reading', which seek to expand combatant status or protections beyond state actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
