% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions Protective Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/armed_conflict
 *
 * SUMMARY:
 *   This constraint represents a state-centric reading of the Geneva
 *   Conventions, where protections for combatants are strictly limited to
 *   uniformed personnel under responsible command meeting Article 4 criteria.
 *   Unprivileged belligerents (e.g., members of non-state armed groups) fall
 *   outside this protective scope, denying them POW status and combatant
 *   immunity. This reading is actively enforced by state governments and
 *   conventional militaries, who benefit from the legal latitude it provides
 *   in asymmetric conflicts. It is a reading of the
 *   'geneva_conventions_protective_scope' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions Protective Scope (State-Centric Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, 'a608a9db-b58a-4a1c-950a-8ac0740a20c8').
narrative_ontology:cs_kernel_codification('a608a9db-b58a-4a1c-950a-8ac0740a20c8', fixed_text).
narrative_ontology:cs_authority_grounding('a608a9db-b58a-4a1c-950a-8ac0740a20c8', lineage).
narrative_ontology:cs_interpretation_layer_present('a608a9db-b58a-4a1c-950a-8ac0740a20c8').
narrative_ontology:cs_reading_relation('a608a9db-b58a-4a1c-950a-8ac0740a20c8', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('a608a9db-b58a-4a1c-950a-8ac0740a20c8', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('a608a9db-b58a-4a1c-950a-8ac0740a20c8', foundational, combatant_status_derived_from_state_affiliation).
narrative_ontology:cs_axiom_status(combatant_status_derived_from_state_affiliation, holdable).
narrative_ontology:cs_axiom_grounding('a608a9db-b58a-4a1c-950a-8ac0740a20c8', combatant_status_derived_from_state_affiliation, conventional).
narrative_ontology:cs_axiom('a608a9db-b58a-4a1c-950a-8ac0740a20c8', foundational, reciprocity_among_states_is_primary_ihl_driver).
narrative_ontology:cs_axiom_status(reciprocity_among_states_is_primary_ihl_driver, holdable).
narrative_ontology:cs_axiom_grounding('a608a9db-b58a-4a1c-950a-8ac0740a20c8', reciprocity_among_states_is_primary_ihl_driver, conventional).
narrative_ontology:cs_reference_frame('a608a9db-b58a-4a1c-950a-8ac0740a20c8', post_wwii_state_centric_ihl).
narrative_ontology:cs_drift_state('a608a9db-b58a-4a1c-950a-8ac0740a20c8', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a608a9db-b58a-4a1c-950a-8ac0740a20c8', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear distinction between lawful combatants and unprivileged belligerents, allowing them to target the latter without granting POW status. This reduces legal and political constraints on their operations in asymmetric conflicts.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, constrained, global).

% Actively uphold and interpret the Geneva Conventions in a manner that prioritizes state sovereignty and the traditional combatant/civilian distinction. They enforce this reading through military doctrine, legal interpretations, and diplomatic pressure.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, constrained, global).

% Bear the full cost of this interpretation, being denied combatant immunity and POW status if captured, and subject to prosecution under domestic law. Their actions are criminalized, and they lack the protections afforded to state forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, local).

% Their members are classified as unprivileged belligerents, exposing them to severe legal and physical risks. They resist this classification through their actions and by seeking alternative legal frameworks, but their options are limited by the dominant state-centric view.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, constrained, regional).

% Argue for a broader application of human rights law and Common Article 3 to all persons in armed conflict, challenging the strict state-centric interpretation. Their arguments are often marginalized in state-led legal and diplomatic forums.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_human_rights_advocates, excluded,
    organized, generational, analytical, global).

% Interpret and apply international humanitarian law, often navigating the tension between state-centric and universalist readings. Their rulings can influence the practical application of the conventions but are subject to political pressures and state cooperation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_criminal_courts, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit narrow, framework for distinguishing combatants from civilians, aiming to limit violence to legitimate military targets and provide a basis for prisoner exchange and humane treatment among recognized state forces.
% TRANSFER_FUNCTION: Transfers legal immunity and protective status from unprivileged belligerents to conventional state militaries, allowing states greater latitude in prosecuting and targeting non-state actors in armed conflict.
% ABSENT_VOICES: Unprivileged belligerents and their advocates are largely excluded from the interpretive process, which is dominated by state legal advisors and military strategists. They would argue for expanded protections based on the realities of modern asymmetric warfare.
% DISAPPEARANCE_RATIONALE: If this state-centric interpretation vanished, the legal landscape of armed conflict would be profoundly altered. State militaries would face increased legal scrutiny for operations against non-state actors, and the distinction between combatant and civilian would become far more ambiguous, leading to a re-evaluation of targeting rules and prisoner treatment.
% FOUNDING_PROBLEM: To regulate warfare between states, ensuring humane treatment of captured combatants and protection of civilians, by establishing clear criteria for combatant status and reciprocal obligations.
% FOUNDING_PROBLEM_CORROBORATION: State governments and conventional militaries argue the problem of regulating inter-state conflict remains live and this reading is essential for military discipline and reciprocity. Human rights advocates and non-state actors argue the founding problem has evolved to include non-international armed conflicts, rendering the state-centric reading obsolete and harmful; their arguments are supported by UN reports and academic legal analysis.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading denies fundamental protections to a significant class of actors in modern conflicts, effectively criminalizing their participation. Suppression (0.78) is also high, as states actively enforce this interpretation through legal frameworks, military doctrine, and the suppression of alternative readings. The theater ratio (0.20) is relatively low, as the distinction between combatants and non-combatants is a genuinely functional aspect of IHL, even if its application is contested. However, some performativity exists in maintaining the 'purity' of the state-centric view against evolving conflict realities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state militaries, this reading provides essential clarity and order in warfare, enabling them to distinguish legitimate targets and maintain discipline. From the perspective of unprivileged belligerents, it is a highly extractive framework that denies them basic human dignity and legal protection, effectively legitimizing their targeting and prosecution.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and state governments are clear beneficiaries, as this reading reduces constraints on their operations and reinforces their legal authority. Unprivileged belligerents and non-state armed groups are the primary victims, facing severe legal and physical consequences. International human rights advocates are excluded, as their arguments for broader protections are not formally incorporated into this interpretation. International criminal courts act as observers, navigating the tensions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolving_nature_of_conflict,
    'Does the state-centric reading adequately address the realities of modern asymmetric warfare, where non-state actors play a significant role?',
    'Empirical analysis of conflict patterns and casualties in non-international armed conflicts, comparing outcomes under strict state-centric application versus more expansive interpretations.',
    'If the state-centric reading is found to be ill-suited to modern conflicts, it would strengthen arguments for re-interpreting or amending the Geneva Conventions to include broader protections, potentially shifting its classification towards a Snare for non-state actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evolving_nature_of_conflict, empirical, 'Assesses the fit of the state-centric reading to contemporary conflict dynamics.').

omega_variable(
    legitimacy_of_non_state_actors,
    'Is the denial of combatant status to non-state armed groups a legitimate distinction, or does it serve primarily to maintain state power in the international system?',
    'Conceptual analysis of sovereignty and self-determination in international law, alongside historical studies of how combatant status has evolved. This is a conceptual, not empirical, question.',
    'If the distinction is found to be primarily a power-maintenance mechanism, it would significantly increase the perceived extractiveness and suppression of this reading, pushing it closer to a Snare from the perspective of non-state actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_non_state_actors, conceptual, 'Examines the normative basis for denying combatant status to non-state actors.').

omega_variable(
    state_centric_vs_universal_rights_tension,
    'Is the state-centric reading fundamentally incompatible with the universal human rights framework, or can the two be reconciled?',
    'Legal scholarship and international court rulings that attempt to harmonize IHL and IHRL, particularly in contexts of non-international armed conflict. This is a conceptual and legal question.',
    'If found incompatible, the state-centric reading''s legitimacy would be further challenged by the universal rights framework, potentially leading to increased resistance and pressure for re-interpretation. If reconcilable, its perceived legitimacy might increase, though its extractiveness for non-state actors would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_centric_vs_universal_rights_tension, conceptual, 'Addresses the tension between state-centric IHL and universal human rights law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'geneva_conventions_protective_scope' kernel. Its state-centric interpretation directly influences the operational space and perceived legitimacy of universal rights and hybrid proportionality readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
