% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Probability Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the 'dropping probability' reading of the total
 *   war reachability boundary, where total war remains a reachable, if less
 *   probable, outcome. Deterrence is framed as a 'tangled rope' – a
 *   coordination equilibrium that genuinely prevents war but involves
 *   significant asymmetric extraction from populations under threat, and
 *   requires active enforcement (maintaining arsenals, signaling resolve).
 *   The claimed type is 'tangled_rope' to reflect the inherent extraction and
 *   active maintenance, even as the probability of total war has dropped over
 *   time. The metrics reflect a decrease in extractiveness and suppression
 *   since the Cold War peak, but a persistent, non-zero level.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.25).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.4).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Probability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '61b8c239-b507-41fb-9790-fbf0ce912e62').
narrative_ontology:cs_kernel_codification('61b8c239-b507-41fb-9790-fbf0ce912e62', distributed).
narrative_ontology:cs_authority_grounding('61b8c239-b507-41fb-9790-fbf0ce912e62', practice).
narrative_ontology:cs_interpretation_layer_present('61b8c239-b507-41fb-9790-fbf0ce912e62').
narrative_ontology:cs_reading_relation('61b8c239-b507-41fb-9790-fbf0ce912e62', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('61b8c239-b507-41fb-9790-fbf0ce912e62', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('61b8c239-b507-41fb-9790-fbf0ce912e62', foundational, total_war_remains_feasible).
narrative_ontology:cs_axiom_status(total_war_remains_feasible, holdable).
narrative_ontology:cs_axiom_grounding('61b8c239-b507-41fb-9790-fbf0ce912e62', total_war_remains_feasible, empirically_contingent).
narrative_ontology:cs_axiom('61b8c239-b507-41fb-9790-fbf0ce912e62', foundational, deterrence_is_coordination_with_extraction).
narrative_ontology:cs_axiom_status(deterrence_is_coordination_with_extraction, holdable).
narrative_ontology:cs_axiom_grounding('61b8c239-b507-41fb-9790-fbf0ce912e62', deterrence_is_coordination_with_extraction, instrumental).
narrative_ontology:cs_reference_frame('61b8c239-b507-41fb-9790-fbf0ce912e62', post_nuclear_realism).
narrative_ontology:cs_drift_state('61b8c239-b507-41fb-9790-fbf0ce912e62', contemporary_strategic_environment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('61b8c239-b507-41fb-9790-fbf0ce912e62', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_establishments).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, global_populations_under_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, articulate deterrence doctrines, and engage in strategic signaling. They benefit from the stability deterrence provides but bear the costs and risks of maintaining the capability and the constant threat of escalation.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Receive funding and political capital for maintaining deterrence capabilities, developing strategic theory, and managing nuclear forces. Their professional identity is often tied to the continued relevance of nuclear deterrence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_establishments, beneficiary,
    organized, biographical, identity_locked, national).

% Live under the constant, if low-probability, threat of nuclear annihilation. They bear the psychological and material costs of maintaining deterrence (taxes, civil defense, anxiety) without direct agency in its operation.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, global_populations_under_threat, payer,
    powerless, immediate, trapped, universal).

% Are subject to the strategic dynamics set by nuclear powers, often without a direct voice. They bear the costs of proliferation risks and the economic burden of maintaining conventional defenses against nuclear-armed neighbors, or seeking security guarantees.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Analyze the risks and benefits of nuclear deterrence, advocating for treaties and policies to reduce arsenals and prevent proliferation. They seek to shift the constraint towards a more stable, less extractive form of coordination.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of nuclear-armed states by making the costs of direct conflict prohibitively high, thereby preventing large-scale conventional or nuclear war between them.
% TRANSFER_FUNCTION: Transfers the risk of existential catastrophe from direct military conflict to the global population, in exchange for a fragile strategic stability maintained by nuclear-armed states.
% ABSENT_VOICES: Future generations, who bear the long-term risks of nuclear waste and potential catastrophe, are absent. Populations in non-nuclear states, whose security is directly impacted but who have no direct say in nuclear policy, are also largely excluded.
% DISAPPEARANCE_RATIONALE: If the boundary of total war reachability vanished (i.e., total war became impossible or irrelevant), the strategic calculus of states would fundamentally alter. Conventional warfare might become more prevalent, or new forms of global governance would emerge to manage conflict, reorganizing international relations.
% FOUNDING_PROBLEM: The problem of preventing large-scale, devastating wars between great powers, particularly after the two World Wars and the advent of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international relations scholars widely corroborate the problem of great power conflict. While the specific mechanisms of deterrence are debated, the underlying problem of preventing total war remains a central concern for policymakers and analysts outside the nuclear powers themselves.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).
:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is moderate, reflecting the ongoing cost and risk borne by global populations, even with reduced tensions. Suppression (0.4) is also moderate, as the threat of nuclear retaliation actively suppresses large-scale conventional conflict. Theater ratio (0.1) is low, indicating that the threat is largely real, not performative, though some signaling may be theatrical. The claimed type 'tangled_rope' reflects the dual nature of deterrence: a coordination function (preventing war) coupled with asymmetric extraction (populations under threat pay the cost of fear and resource allocation to defense).
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers perceive deterrence as a necessary, if costly, coordination mechanism for global stability. Populations under threat, however, experience it as a constant, imposed burden and a source of existential anxiety. This divergence is captured by the 'tangled_rope' classification, which acknowledges both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers and their defense establishments are beneficiaries/agenda-setters, gaining strategic stability and resources. Global populations and non-nuclear states are payers, bearing the costs and risks. The directionality for nuclear powers is low (beneficiary), while for populations under threat it is high (target).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_vs_risk,
    'Is the current level of nuclear deterrence a stable equilibrium that minimizes total war risk, or does it inherently carry an unacceptable risk of accidental or escalatory conflict?',
    'Long-term historical analysis of near-miss incidents, game-theoretic modeling of escalation pathways, and empirical studies of decision-making under extreme stress.',
    'If deterrence is found to be inherently unstable, the extractiveness and suppression metrics would be re-evaluated upwards, potentially reclassifying it as a snare. If it''s a robust equilibrium, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_vs_risk, empirical, 'Assesses the true stability and safety of nuclear deterrence as a coordination mechanism.').

omega_variable(
    deterrence_as_coordination_or_extraction,
    'Is the primary function of nuclear deterrence genuine coordination (preventing war) or primarily a mechanism for great powers to extract strategic advantage and maintain global hierarchy?',
    'Analysis of resource allocation to nuclear programs versus other security measures, and examination of diplomatic outcomes where nuclear threats are implicitly or explicitly used to achieve non-proliferation or geopolitical goals.',
    'If primarily extraction, the ''tangled_rope'' classification would shift closer to ''snare'', with higher effective extraction for non-nuclear states and global populations. If coordination is dominant, the current classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_as_coordination_or_extraction, conceptual, 'Distinguishes the coordination function from the extractive function of nuclear deterrence.').

omega_variable(
    reachability_definition_ambiguity,
    'What constitutes ''reachability'' for total war? Is it merely technical capability, or does it include political will and strategic doctrine? How does this definition influence the perceived probability?',
    'Conceptual clarification through expert consensus on definitions of ''total war'' and ''reachability'' in strategic studies, and analysis of how different definitions impact risk assessments.',
    'A narrow definition of reachability (e.g., purely technical) might support the ''contraction_reading'' (total war is less reachable), while a broader definition (including political will) might reinforce this ''dropping_reading''. This would shift the perceived extractiveness and suppression accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''total war reachability'' and its impact on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.08).
narrative_ontology:measurement(tota_tr_t1989, total_war_reachability_boundary__dropping_reading, theater_ratio, 1989, 0.15).
narrative_ontology:measurement(tota_tr_t2001, total_war_reachability_boundary__dropping_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(tota_tr_t2014, total_war_reachability_boundary__dropping_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__dropping_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.8).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.9).
narrative_ontology:measurement(tota_be_t1989, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1989, 0.7).
narrative_ontology:measurement(tota_be_t2001, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(tota_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.85).
narrative_ontology:measurement(tota_su_t1989, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1989, 0.6).
narrative_ontology:measurement(tota_su_t2001, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(tota_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.45).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
