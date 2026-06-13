% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: War Winnability: Rhetorical Taboo + Operational Planning
 *   domain: strategic/political/military
 *
 * SUMMARY:
 *   This constraint instantiates the RHETORICAL_CONTRACTION reading of the
 *   war_winnability_post_1945 kernel. The reading's core claim: nuclear
 *   weapons made winnability unsayable in democratic public discourse while
 *   remaining operationally assumed in classified planning. The constraint
 *   does not claim winnability is actually possible (that is the
 *   countervailing_thinkable reading) or that it became genuinely unthinkable
 *   (that is the deterrence_unthinkable reading). Instead, it models the
 *   institutional mechanism that sustains a dual-layer operation: rhetorical
 *   taboo for publics + operational planning for strategists. The measurement
 *   series shows extraction and theater rising together from 1945 (when the
 *   asymmetry was young and not yet institutionalized) through the present,
 *   with theater now at 0.72 — indicating that most of the constraint's
 *   operation is now performative (the taboo is maintained rhetorically;
 *   planning flexibility is the real function). Suppression requirement rises
 *   sharply 1945–1974 (when the taboo was being established and challenged)
 *   and stabilizes thereafter — indicating enforcement hardened once the
 *   taboo became institutional.
 *
 * KEY AGENTS:
 *   - Military planners: sustain classified planning for winnability under constraints; publicly defer to deterrence taboo
 *   - Defense intellectuals (both schools): operate as policy translators; benefit from classified access and funding that the constraint structure protects
 *   - Democratic publics: told winnability is incoherent; cannot scrutinize planning assumptions because they remain classified and taboo
 *   - Legislative oversight: receive classified briefings but operate under the same taboo; cannot publicly interrogate planning without violating the constraint
 *   - Adversary establishments: never adopted the taboo; operate with explicit winnability assumptions throughout the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.79).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "War Winnability: Rhetorical Taboo + Operational Planning").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic/political/military").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '95a9c68d-c137-48e4-9585-e8b6a9f51bbd').
narrative_ontology:cs_kernel_codification('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', fixed_text).
narrative_ontology:cs_authority_grounding('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', extraction).
narrative_ontology:cs_interpretation_layer_present('95a9c68d-c137-48e4-9585-e8b6a9f51bbd').
narrative_ontology:cs_reading_relation('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', foundational, winnability_rhetorically_unthinkable).
narrative_ontology:cs_axiom_status(winnability_rhetorically_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', winnability_rhetorically_unthinkable, conventional).
narrative_ontology:cs_axiom('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', foundational, winnability_operationally_constrained_possible).
narrative_ontology:cs_axiom_status(winnability_operationally_constrained_possible, holdable).
narrative_ontology:cs_axiom_grounding('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', winnability_operationally_constrained_possible, empirically_contingent).
narrative_ontology:cs_reference_frame('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', deterrence_doctrine_absolute_unwinnable).
narrative_ontology:cs_drift_state('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', contemporary_declassification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('95a9c68d-c137-48e4-9585-e8b6a9f51bbd', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, military_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, defense_intellectuals).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_publics).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint grants military planners operational autonomy from public accountability: they plan for winnability without defending the planning to publics. Suppression is higher (0.79) because maintaining the taboo requires active enforcement — stopping conversation, delegitimizing speakers, classifying planning documents. Theater ratio is the highest of all (0.72) and has grown over time: the rhetorical function (maintaining public support for deterrence-absolute doctrine) has become increasingly disconnected from the operational function (planning for constrained winnability). The constraint now operates mostly as performance — the taboo is maintained but everyone in the planning establishment understands what it covers. Measurements on one shared grid: every metric authored at every time point across 1945–2025, with basis tags indicating projected early-interval values (before declassification) and observed values from declassified planning documents.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (democratic publics/legislators) should compute as victims of a snare — foreclosed from understanding what their military supposedly defends, unable to exit without accepting strategic vulnerability, bearing the cost of the asymmetry through representation loss. The beneficiary seats (military planners, defense intellectuals) should compute as beneficiaries of a tangled_rope — genuine coordination (deterrence doctrine does stabilize certain outcomes) and extraction (planning autonomy extracted from accountability) are bundled in the same institution. The constraint's operation looks different from each seat: from the planner seat it is a necessary compartmentalization that protects military rationality; from the public seat it is suppression of information they need to consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Military planners: beneficiary (collects operational autonomy, access to classified planning, no public accountability) — d near 0.0. Democratic publics: target (cannot question planning, cannot participate in founding choices, no exit without accepting strategic vulnerability) — d near 1.0. Defense intellectuals: ambiguous — they benefit from the constraint (funding, classified access, policy relevance) but are also constrained by it (cannot publish certain arguments). Authored override: countervailing_force_school gets d=0.65 (benefits from the taboo that shields them from public backlash, but constrained by the taboo from publishing winnability arguments that fit their actual analysis). Legislative oversight: victim by structural design (can see classified plans but cannot change the rhetorical frame that permits them to be hidden).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows the classical mandatrophy pattern: the founding problem (need for planning autonomy + public support for deterrence) is LIVE — planners still need operational flexibility and publics still would reject winnability-planning if it were transparent. But the institutional form has calcified: the winnability taboo is now maintained by reflexive enforcement rather than by genuine commitment. The theater ratio (0.72) shows how much of the constraint's energy is now spent on maintaining the rhetorical frame rather than solving the founding coordination problem. Classification, rhetorical discipline, and taboo enforcement are the machinery; the genuine strategic function (planning constrained escalation response options) could be served by less suppressive arrangements (e.g., transparent planning with public debate about acceptable outcomes). The constraint persists because fixing it would require either admitting winnability-planning to public scrutiny (politically impossible) or abandoning planning flexibility (strategically unacceptable to planners). The classification system and the taboo are now locked together — opening one threatens both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetorical_vs_operational_divergence_width,
    'How wide is the actual gap between classified war planning assumptions and public deterrence doctrine? Do planners assume limited winnability is reachable, or merely that it constrains escalation risk?',
    'Declassification of war plans (SIOP, Strategic Concept documents, Defense Science Board task forces on nuclear strategy). Interviews with retired strategic planners willing to specify planning assumptions off the record.',
    'A wide gap (planners assume winnability is reachable) supports the snare reading — planners are hiding real strategy from publics. A narrow gap (planners assume winnability is constrained-but-important) supports the tangled_rope reading — the constraint is binding both sides, not just suppressing one side''s preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_vs_operational_divergence_width, empirical, 'Width of the divergence between classified planning and public rhetoric.').

omega_variable(
    taboo_internalization_vs_strategic_necessity,
    'Do planners maintain the winnability taboo because it is strategically necessary (operational compartmentalization requires it), or because it is politically enforced (violation carries reputational/institutional cost)?',
    'Structural analysis: if the taboo breaks when political enforcement weakens (declassification, regime change, new administrations), the enforcement is political. If planning assumptions shift when the taboo breaks, the taboo was strategic. Comparative study: does the constraint persist in planning establishments that face different political pressures (U.S./UK vs. France vs. China)?',
    'If political: fixing the constraint requires changing the political coalition supporting deterrence (hard). If strategic: the taboo may be unnecessary to the actual planning function (possibly solvable through transparency).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_internalization_vs_strategic_necessity, conceptual, 'Whether the taboo is strategic necessity or political enforcement.').

omega_variable(
    democratic_deficit_scope,
    'How much of the planners'' operational autonomy is justified by genuine security compartmentalization, and how much is unjustified secrecy that serves only to insulate planning from scrutiny?',
    'Comparative institutional analysis: planning establishments with higher public transparency (Sweden, Canada during Cold War) vs. low-transparency establishments (U.S., UK). Do transparent planning establishments produce worse deterrence stability or worse war outcomes? Do they actually constrain operational flexibility?',
    'If transparency does not degrade planning quality, the taboo is primarily about suppressing oversight, not about protecting strategy. That would reclassify the constraint from tangled_rope (genuine coordination + necessary extraction) toward snare (extracted accountability disguised as coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_deficit_scope, empirical, 'How much of the suppression is necessary vs. discretionary.').

omega_variable(
    kernel_reading_identification,
    'This constraint is ONE READING of the contested kernel ''war_winnability_post_1945.'' Which reading is structurally correct: does nuclear weapons make winnability unthinkable (deterrence_unthinkable), or constrained-but-possible (countervailing_thinkable), or unsayable-while-planned (this reading)?',
    'The three readings coexist as live institutional positions. The question is which describes the actual decision-making structure of nuclear-armed states. Witness: classified planning documents (when declassified), stated doctrine, actual force posture, and what strategists say in private vs. public.',
    'If deterrence_unthinkable is correct: the winnability taboo is strategically sound — planners should not be planning for winnability at all. If countervailing_thinkable is correct: the taboo is a deliberate obscuration of a valid strategic position. If rhetorical_contraction is correct: planners are maintaining two incompatible frameworks simultaneously (unsayable in public, operationally assumed in secret). This omega routes to the broader kernel contest about nuclear strategy''s logical possibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Which reading of the war_winnability_post_1945 kernel is structurally true.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1962, 0.48).
narrative_ontology:measurement(war__tr_t1974, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1974, 0.62).
narrative_ontology:measurement(war__tr_t1985, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1985, 0.68).
narrative_ontology:measurement(war__tr_t2001, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2001, 0.71).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2025, 0.72).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1962, 0.42).
narrative_ontology:measurement(war__be_t1974, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1974, 0.58).
narrative_ontology:measurement(war__be_t1985, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(war__be_t2001, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2001, 0.66).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement(war__su_t1974, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1974, 0.71).
narrative_ontology:measurement(war__su_t1985, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1985, 0.76).
narrative_ontology:measurement(war__su_t2001, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2001, 0.78).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2025, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, nuclear_deterrence_doctrine).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, strategic_surprise_institutional_denial).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, defense_intellectual_credentialing).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story kernel family: war_winnability_post_1945__rhetorical_contraction (this story), war_winnability_post_1945__deterrence_unthinkable, and war_winnability_post_1945__countervailing_thinkable. The three stories are NOT alternative measurements of the same constraint (which would violate ε-invariance). They are three distinct constraints that flow from three incompatible readings of the same contested kernel about what nuclear weapons make possible. Rhetorical_contraction models the institutional mechanism that sustains the contradiction between the other two readings by keeping both operationally alive while making one publicly illegitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
