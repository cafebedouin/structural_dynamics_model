% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability (Strategic Culture Drift Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the ideational shift in strategic culture
 *   post-1945, where the concept of 'total war winnability' largely dropped
 *   from elite discourse, despite its physical reachability. It operates as a
 *   Piton because the active function of strategizing for total war has
 *   atrophied, but the constraint (the unthinkability) persists due to
 *   institutional inertia and the self-reinforcing nature of strategic
 *   culture. Defense intellectuals invested in limited war frameworks benefit
 *   from this constraint, while military planners and political leaders
 *   experience reduced strategic flexibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.7).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.6).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.7).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability (Strategic Culture Drift Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67').
narrative_ontology:cs_kernel_codification('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', implicit).
narrative_ontology:cs_authority_grounding('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', practice).
narrative_ontology:cs_interpretation_layer_present('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67').
narrative_ontology:cs_reading_relation('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_axiom('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', foundational, total_war_remains_physically_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_physically_reachable, holdable).
narrative_ontology:cs_axiom_grounding('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', total_war_remains_physically_reachable, empirically_contingent).
narrative_ontology:cs_axiom('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', foundational, strategic_culture_shapes_discourse_limits).
narrative_ontology:cs_axiom_status(strategic_culture_shapes_discourse_limits, holdable).
narrative_ontology:cs_axiom_grounding('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', strategic_culture_shapes_discourse_limits, conventional).
narrative_ontology:cs_reference_frame('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', post_nuclear_deterrence_era).
narrative_ontology:cs_drift_state('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', contemporary_strategic_discourse, gap(stable, substantial, true)).
narrative_ontology:cs_created_at('1ceb1a55-ec1f-4197-ae18-0ee7e0e17b67', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war_frameworks).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, political_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These academics and policy analysts benefit from the dominance of limited war frameworks, as their careers and influence are built upon these concepts. They actively shape and reinforce the strategic culture that marginalizes total war discourse.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war_frameworks, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals_limited_war_frameworks, agenda_setter).

% Military planners are constrained by the prevailing strategic culture, which limits their ability to conceptualize and plan for scenarios involving total war. This reduces their strategic flexibility and forces them to operate within narrower doctrinal boundaries.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_planners, payer,
    organized, biographical, constrained, national).

% Political leaders inherit a strategic culture that makes it difficult to articulate or pursue total war objectives, even in extreme circumstances. Their decision-making space is narrowed by the ideational constraint, potentially limiting their options in crises.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, political_leaders, payer,
    powerful, immediate, constrained, national).

% These scholars analyze the evolution and impact of strategic culture, including the decline of total war discourse. They are external to the direct operation of the constraint but provide critical analysis of its effects.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_culture_scholars, observer,
    analytical, generational, analytical, global).

% A small, marginalized group of thinkers who believe total war remains a viable or necessary strategic option. They are excluded from mainstream elite discourse, their arguments often dismissed as anachronistic or dangerous, reinforcing the ideational constraint.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, total_war_advocates_fringe, excluded,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates strategic thinking among elites around the concept of limited warfare, thereby implicitly coordinating efforts to avoid escalation to total war.
% TRANSFER_FUNCTION: Transfers intellectual and policy influence from proponents of total war strategies to those advocating for limited war frameworks, effectively reallocating cognitive resources and legitimizing certain strategic approaches over others.
% ABSENT_VOICES: Advocates for the winnability or necessity of total war are largely absent from elite strategic discourse. Their perspectives are marginalized, preventing a full spectrum of strategic options from being considered.
% DISAPPEARANCE_RATIONALE: If the ideational constraint against total war discourse vanished, strategic thinking would broaden significantly. Military doctrines, political rhetoric, and resource allocation for defense would likely shift to include a wider range of scenarios, potentially altering international relations and conflict dynamics.
% FOUNDING_PROBLEM: The perceived catastrophic consequences of nuclear war and the desire to prevent a repeat of the devastation of World War II led to a post-1945 effort to make total war 'unthinkable' or 'unwinnable' in elite strategic circles.
% FOUNDING_PROBLEM_CORROBORATION: Strategic culture analyses and historical shifts in military doctrine corroborate the ideational shift. However, the 'live' status of the problem is primarily asserted by those who benefit from the limited war frameworks, while critical scholars and some military figures (outside the immediate beneficiaries) suggest the problem's 'dead' status has created new vulnerabilities.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.7) because it significantly limits the intellectual and strategic options available to state actors. Suppression (0.6) is ideational, enforced through academic norms, policy consensus, and the marginalization of dissenting views, rather than direct coercion. The theater ratio is high (0.75) as much of the activity around 'limited war' serves to performatively maintain the unthinkability of total war, rather than genuinely solving the problem of total war itself. Accessibility collapse (0.65) reflects the difficulty of reintroducing total war discourse into elite circles, while resistance (0.2) is low due to the self-reinforcing nature of strategic culture.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of defense intellectuals, the constraint is a beneficial coordination mechanism that prevents catastrophic thinking. From the perspective of military planners and political leaders, it's a limitation on their strategic toolkit, potentially creating blind spots in crisis management. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals promoting limited war frameworks are beneficiaries (d near 0.0) as their intellectual capital and influence are amplified. Military planners and political leaders are targets (d near 1.0) as their strategic options are curtailed. Fringe total war advocates are excluded, experiencing high d due to their marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy because the original mandate (preventing nuclear war by making total war unthinkable) has led to an atrophy of strategic capacity. The 'unthinkability' has become an end in itself, rather than a means to an end, and its maintenance now extracts strategic flexibility. The high theater ratio reflects this performative maintenance. The classification as Piton highlights this atrophy and inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideational_vs_normative_causation,
    'To what extent is the decline of total war discourse driven by an ideational shift in strategic culture (this reading) versus a normative shift towards illegitimacy (sibling ''normative_reading_drop'')?',
    'Comparative historical analysis of strategic documents and international legal developments, examining whether the language of ''unthinkability'' preceded or followed the language of ''illegitimacy'' in key policy shifts.',
    'If primarily normative, the constraint might be reclassified closer to a Rope (coordination around shared norms); if primarily ideational, the Piton classification holds, emphasizing the inertial nature of cultural shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideational_vs_normative_causation, conceptual, 'Distinguishing ideational from normative drivers of discourse change.').

omega_variable(
    reachable_vs_impossible_ambiguity,
    'Is total war truly ''reachable'' (as this reading asserts) or ''structurally impossible'' (as the sibling ''structural_contraction_reading'' claims due to nuclear weapons)?',
    'A counterfactual analysis of a world without nuclear weapons, or a detailed examination of current military capabilities and doctrines to assess the physical possibility of large-scale, existential conflict beyond limited war scenarios.',
    'If total war is indeed structurally impossible, this reading''s premise (''remains reachable'') is undermined, potentially foreclosing it and shifting the classification towards a Mountain (physical limit) or a Snare (if the ''impossibility'' is a constructed narrative for extraction). If reachable, the Piton classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachable_vs_impossible_ambiguity, empirical, 'Ambiguity regarding the physical reachability of total war.').

omega_variable(
    quantifying_ideational_suppression,
    'How can the ''ideational suppression'' of total war discourse be more rigorously quantified beyond expert judgment?',
    'Content analysis of strategic publications, military doctrines, and political speeches over time, measuring the frequency and framing of ''total war'' concepts, and correlating with academic citations and policy influence metrics.',
    'More precise quantification could refine the ''suppression'' metric, potentially revealing a stronger or weaker coercive force of strategic culture than currently estimated, impacting the overall classification''s robustness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantifying_ideational_suppression, empirical, 'Methodological challenge in measuring ideational suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.6).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2005, 0.7).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2024, 0.75).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1965, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(tota_su_t1985, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_winnability_post1945' kernel. This 'strategic_culture_drift' reading focuses on the ideational shift in elite discourse, distinct from the 'structural_contraction_reading' (physical impossibility) and the 'normative_reading_drop' (normative illegitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
