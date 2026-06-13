% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Conventions Protective Scope: Universal Rights Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universal rights' reading of the Geneva
 *   Conventions' protective scope, asserting that protections extend to all
 *   persons affected by armed conflict, regardless of combatant status, with
 *   Common Article 3 and human rights law establishing a universal floor.
 *   This reading significantly expands the victim set to include non-state
 *   actors and civilian populations, while simultaneously restricting the
 *   operational flexibility of state military and intelligence agencies. It
 *   is a 'tangled_rope' because it genuinely coordinates humanitarian
 *   protection but imposes substantial costs on state actors who prefer
 *   narrower interpretations.
 *
 * KEY AGENTS:
 *   - state_military_operations: Primary target (institutional/constrained) — bears extraction through restricted targeting and detention practices.
 *   - non_state_armed_groups: Primary beneficiary (organized/constrained) — benefits from protective floor, even if not fully compliant.
 *   - civilian_populations: Primary beneficiary (powerless/trapped) — benefits from expanded protective scope.
 *   - intelligence_agencies: Primary target (institutional/constrained) — bears extraction through restrictions on interrogation and surveillance.
 *   - international_human_rights_advocates: Agenda setter/Observer (organized/analytical) — actively promotes and monitors this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Conventions Protective Scope: Universal Rights Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '26eaee63-873b-43eb-8ccc-af5463fd3a1e').
narrative_ontology:cs_kernel_codification('26eaee63-873b-43eb-8ccc-af5463fd3a1e', fixed_text).
narrative_ontology:cs_authority_grounding('26eaee63-873b-43eb-8ccc-af5463fd3a1e', lineage).
narrative_ontology:cs_interpretation_layer_present('26eaee63-873b-43eb-8ccc-af5463fd3a1e').
narrative_ontology:cs_reading_relation('26eaee63-873b-43eb-8ccc-af5463fd3a1e', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('26eaee63-873b-43eb-8ccc-af5463fd3a1e', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('26eaee63-873b-43eb-8ccc-af5463fd3a1e', foundational, human_dignity_universal_in_conflict).
narrative_ontology:cs_axiom_status(human_dignity_universal_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('26eaee63-873b-43eb-8ccc-af5463fd3a1e', human_dignity_universal_in_conflict, deontological).
narrative_ontology:cs_axiom('26eaee63-873b-43eb-8ccc-af5463fd3a1e', foundational, common_article_3_minimum_floor).
narrative_ontology:cs_axiom_status(common_article_3_minimum_floor, holdable).
narrative_ontology:cs_axiom_grounding('26eaee63-873b-43eb-8ccc-af5463fd3a1e', common_article_3_minimum_floor, conventional).
narrative_ontology:cs_reference_frame('26eaee63-873b-43eb-8ccc-af5463fd3a1e', post_world_war_ii_human_rights_expansion).
narrative_ontology:cs_drift_state('26eaee63-873b-43eb-8ccc-af5463fd3a1e', post_9_11_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('26eaee63-873b-43eb-8ccc-af5463fd3a1e', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detainees_and_prisoners_of_war).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operations).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, intelligence_agencies).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, universal_human_rights_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, jus_cogens_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must adhere to expanded rules of engagement, targeting restrictions, and detention standards, even when fighting non-state actors or in non-international armed conflicts. This limits tactical flexibility and increases the cost of operations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_operations, payer,
    institutional, biographical, constrained, global).

% Benefit from a baseline of protection under Common Article 3 and human rights law, even if they do not fully comply with IHL themselves. This grants them a degree of legal recognition and protection against certain state actions.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, biographical, constrained, regional).

% Receive enhanced protection from indiscriminate attacks, arbitrary detention, and other abuses, as the universal rights reading emphasizes their inherent human dignity regardless of conflict context. Their situation is often one of extreme vulnerability.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Face restrictions on interrogation techniques, surveillance, and detention practices, particularly concerning individuals not classified as traditional prisoners of war. This impacts their ability to gather intelligence and conduct covert operations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% Actively promote and litigate for this expansive interpretation of IHL, viewing it as essential for upholding universal human dignity. They benefit from the increased normative force of human rights in armed conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_human_rights_advocates, agenda_setter,
    organized, generational, analytical, global).

% Interpret and apply IHL, often leaning towards an expansive, universalist reading. Their rulings reinforce this constraint, shaping state practice and accountability mechanisms.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humanitarian protection for all individuals in armed conflict, aiming to reduce suffering and ensure basic human dignity regardless of their status or the nature of the conflict.
% TRANSFER_FUNCTION: Transfers operational flexibility and impunity from state military and intelligence agencies to a protective floor for non-state actors, civilians, and detainees, increasing the legal and moral burden on states.
% ABSENT_VOICES: Hardline military strategists and national security hawks within states, who would argue for maximum operational freedom and minimal legal constraints in conflict, are often marginalized in international legal discourse but exert significant internal pressure.
% DISAPPEARANCE_RATIONALE: If this universal protective scope vanished, state actors would likely revert to narrower interpretations, leading to increased civilian casualties, more permissive detention and interrogation practices, and a significant erosion of human rights in conflict zones. The legal and moral landscape of armed conflict would fundamentally shift.
% FOUNDING_PROBLEM: The original Geneva Conventions did not fully address non-international armed conflicts or the status of non-state combatants, leaving significant gaps in protection for many individuals affected by modern warfare.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and numerous legal scholars consistently attest that the problem of protecting all persons in armed conflict remains live, citing ongoing conflicts and violations. While states acknowledge the problem, many contest the universal rights reading as the appropriate solution, preferring more state-centric approaches.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading imposes significant operational constraints and legal burdens on state actors, who prefer more permissive interpretations. Suppression (0.7) is also high, as states actively resist this expansive interpretation through legal arguments, non-compliance, and attempts to narrow its application. The theater ratio (0.4) reflects that while some compliance is genuine, a substantial portion involves rhetorical adherence without full implementation, or attempts to create legal loopholes. Accessibility collapse (0.4) is moderate; while the norm is widely accepted, practical alternatives for states (e.g., 'war on terror' frameworks) are often pursued. Resistance (0.75) is high due to consistent pushback from states seeking to preserve military flexibility.
 *
 * PERSPECTIVAL GAP:
 *   State military and intelligence agencies experience this as a highly extractive constraint, limiting their ability to achieve objectives. Non-state armed groups and civilian populations, however, experience it as a crucial protective mechanism. International human rights advocates view it as a necessary and just coordination mechanism for human dignity in conflict. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-state armed groups and civilian populations are clear beneficiaries (d near 0.0) as the constraint expands their protections. State military operations and intelligence agencies are targets (d near 1.0) as their operational freedom is curtailed. International human rights advocates act as agenda-setters, pushing for this interpretation, and thus benefit from its adoption (d near 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; rather, it is a contested expansion of an existing mandate. The 'universal rights' reading seeks to prevent the original mandate (protection in armed conflict) from atrophying in the face of evolving conflict types and state practices that seek to narrow its application. The contest is over the scope and beneficiaries of the mandate, not its obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine universal application of IHL, or a contested interpretation that overextends the original treaty scope?',
    'International Court of Justice advisory opinions or widespread state practice explicitly adopting this reading.',
    'If confirmed as genuine, it strengthens the protective floor for all; if contested, its application remains subject to state discretion and political will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''universal_rights_reading'' of the ''geneva_conventions_protective_scope'' kernel. Sibling readings (''state_centric_reading'', ''hybrid_proportionality_reading'') would narrow the protective scope and reduce the burden on state military operations.').

omega_variable(
    enforcement_gap_vs_normative_force,
    'Does the frequent violation of this universal protective scope indicate a lack of normative force, or merely an enforcement gap?',
    'Analysis of state justifications for violations: do states deny the norm itself, or claim operational necessity/exceptions?',
    'If states deny the norm, the constraint''s effective suppression is lower; if they claim exceptions, the norm''s underlying force is acknowledged, but enforcement is weak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_vs_normative_force, empirical, 'The gap between the declared universal protective scope and actual state practice in armed conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1990, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'universal_rights_reading' of the 'geneva_conventions_protective_scope' kernel. It expands the protective scope compared to the 'state_centric_reading' and offers a more expansive floor than the 'hybrid_proportionality_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
