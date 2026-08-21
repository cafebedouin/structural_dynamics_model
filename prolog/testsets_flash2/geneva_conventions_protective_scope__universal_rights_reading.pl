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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Conventions Protective Scope: Universal Rights Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universal rights' reading of the Geneva
 *   Conventions' protective scope, asserting that protections extend to all
 *   persons affected by armed conflict, irrespective of their combatant
 *   status, by integrating Common Article 3 with broader human rights law.
 *   This reading expands the victim set to include all conflict participants
 *   and significantly restricts state military operational flexibility. It is
 *   a contested interpretation, actively enforced by human rights advocates
 *   but resisted by state military and intelligence agencies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Conventions Protective Scope: Universal Rights Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '8f3c3551-a022-4709-a6b1-2887848dc8c7').
narrative_ontology:cs_kernel_codification('8f3c3551-a022-4709-a6b1-2887848dc8c7', formalized).
narrative_ontology:cs_authority_grounding('8f3c3551-a022-4709-a6b1-2887848dc8c7', lineage).
narrative_ontology:cs_interpretation_layer_present('8f3c3551-a022-4709-a6b1-2887848dc8c7').
narrative_ontology:cs_reading_relation('8f3c3551-a022-4709-a6b1-2887848dc8c7', geneva_conventions_protective_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('8f3c3551-a022-4709-a6b1-2887848dc8c7', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('8f3c3551-a022-4709-a6b1-2887848dc8c7', foundational, universal_human_dignity_in_conflict).
narrative_ontology:cs_axiom_status(universal_human_dignity_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('8f3c3551-a022-4709-a6b1-2887848dc8c7', universal_human_dignity_in_conflict, deontological).
narrative_ontology:cs_axiom('8f3c3551-a022-4709-a6b1-2887848dc8c7', foundational, ihrl_applies_extraterritorially_in_conflict).
narrative_ontology:cs_axiom_status(ihrl_applies_extraterritorially_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('8f3c3551-a022-4709-a6b1-2887848dc8c7', ihrl_applies_extraterritorially_in_conflict, conventional).
narrative_ontology:cs_reference_frame('8f3c3551-a022-4709-a6b1-2887848dc8c7', post_common_article_3_era).
narrative_ontology:cs_drift_state('8f3c3551-a022-4709-a6b1-2887848dc8c7', contemporary_counter_terrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f3c3551-a022-4709-a6b1-2887848dc8c7', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detainees_and_prisoners_of_war).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operations).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, intelligence_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a universal floor of protection that applies to their members regardless of formal combatant status, limiting state reprisals and ensuring humane treatment. Their operational flexibility is not directly constrained by this reading, but their legitimacy claims may be enhanced.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    moderate, biographical, constrained, regional).

% Receive expanded protections against targeting, indiscriminate attacks, and collateral damage, as well as guarantees of basic human rights even in conflict zones. Their safety and well-being are prioritized over military necessity.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Are guaranteed humane treatment, due process, and protection from torture or degrading treatment, regardless of their combatant status or the nature of the conflict. This reading extends these protections to all persons deprived of liberty in connection with armed conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detainees_and_prisoners_of_war, beneficiary,
    powerless, immediate, trapped, local).

% Bear increased constraints on targeting, detention, interrogation, and rules of engagement. Their operational flexibility is reduced by the expanded scope of protection, requiring greater adherence to human rights standards even in non-international armed conflicts. This raises the cost and complexity of military action.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_operations, payer,
    institutional, biographical, constrained, global).

% Face significant restrictions on their ability to conduct interrogations, surveillance, and covert operations, particularly concerning the treatment of detainees and the collection of intelligence from non-state actors. This reading directly challenges their traditional operational doctrines.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% Actively promote and enforce this reading through legal challenges, advocacy campaigns, and international monitoring. They shape the interpretive discourse and push for universal application of human rights law in armed conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_human_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Would argue that this reading unduly restricts military effectiveness and blurs the lines between combatants and civilians, making conflict resolution more difficult. They are often excluded from the interpretive process that advances this reading, as their operational concerns are seen as secondary to human rights.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, traditional_military_strategists, excluded,
    powerful, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable floor of humane treatment and basic rights for all persons affected by armed conflict, regardless of their status, aiming to reduce suffering and prevent atrocities.
% TRANSFER_FUNCTION: Transfers operational flexibility and targeting discretion from state military and intelligence agencies to non-state actors and civilian populations, in exchange for enhanced human dignity and protection.
% ABSENT_VOICES: Traditional military strategists and national security hardliners are often excluded from the discourse that advances this reading; they would argue for greater deference to military necessity and state sovereignty, fearing that universalizing protections undermines effective warfare.
% DISAPPEARANCE_RATIONALE: If this universal rights reading vanished, state actors would likely revert to more restrictive interpretations of IHL, leading to increased targeting of non-state actors, harsher detention conditions, and a general erosion of protections for civilians and unprivileged belligerents, fundamentally altering the landscape of armed conflict.
% FOUNDING_PROBLEM: The problem of widespread suffering, atrocities, and lack of protection for civilians and non-state combatants in non-international armed conflicts, where traditional IHL frameworks were often deemed insufficient.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and numerous academic legal scholars corroborate that the problem of protecting all persons in armed conflict remains live, citing ongoing violations and the evolving nature of warfare. This is attested from outside the benefiting parties (e.g., by independent legal experts and human rights monitors).
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is high because this reading imposes substantial costs on state military operations, limiting targeting, detention, and interrogation practices. Suppression (0.75) is also high, reflecting the active legal and political enforcement required to compel states to adhere to this expanded interpretation, often against their strategic interests. Theater ratio (0.20) is relatively low, as the advocates for this reading are genuinely committed to its principles, though some states may pay lip service while resisting full implementation. The metrics show a gradual increase in both extractiveness and suppression over time, reflecting the growing assertiveness of this reading in international law.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates, this reading is a necessary evolution of IHL to meet contemporary conflict realities, a 'rope' that coordinates universal protection. From the perspective of state military and intelligence agencies, it is an 'extraction' that unduly burdens their ability to conduct operations and protect national interests, effectively a 'snare' on their operational freedom. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-state armed groups, civilian populations, and detainees are the primary beneficiaries, gaining expanded protections. State military operations and intelligence agencies are the primary victims, bearing the costs of restricted operational flexibility. International human rights advocates act as agenda-setters, pushing for this interpretation. Traditional military strategists are excluded, as their perspectives on military necessity are often sidelined in this discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (universal protection of human dignity in conflict) is considered live and increasingly urgent by its proponents. The contest is over its scope and application, not its fundamental purpose. The classification as a Tangled Rope reflects the ongoing tension between its genuine coordination function (universal protection) and its asymmetric extraction from state actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_human_rights_in_armed_conflict,
    'To what extent do international human rights law obligations apply extraterritorially during armed conflict, and how do they interact with IHL?',
    'Further jurisprudence from international courts (e.g., ICJ, ECtHR) clarifying the extraterritorial application of human rights treaties in conflict zones, or a new international convention explicitly harmonizing IHL and IHRL.',
    'Clearer delineation would either solidify this reading''s expanded scope (if IHRL applies broadly) or limit it (if IHL is deemed lex specialis, overriding IHRL in certain contexts), affecting the extractiveness on state actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_human_rights_in_armed_conflict, conceptual, 'Ambiguity regarding the precise interplay and hierarchy between International Humanitarian Law and International Human Rights Law in armed conflict.').

omega_variable(
    state_compliance_vs_resistance,
    'What is the actual level of state compliance with this universal rights reading, versus rhetorical acceptance or active resistance?',
    'Empirical studies tracking state practice, military manuals, and judicial decisions across multiple jurisdictions, combined with monitoring by independent human rights bodies.',
    'If compliance is largely rhetorical, the effective extractiveness and suppression are lower than claimed, indicating a more performative constraint. If resistance is widespread and effective, the constraint''s actual force is weaker, potentially reclassifying it towards a Piton or even a failed Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_compliance_vs_resistance, empirical, 'The gap between the normative force of the universal rights reading and the actual behavior of states in armed conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1995, 0.17).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.4).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1965, 0.58).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'geneva_conventions_protective_scope' kernel. This 'universal_rights_reading' expands protections by integrating human rights law, contrasting with the 'state_centric_reading' (which limits protections to uniformed combatants) and the 'hybrid_proportionality_reading' (which scales protections by conflict type). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
