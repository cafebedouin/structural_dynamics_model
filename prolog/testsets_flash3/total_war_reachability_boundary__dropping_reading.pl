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
 *   This constraint represents the 'dropping probability' reading of the
 *   total war reachability boundary, where total war remains a reachable,
 *   albeit less probable, outcome. Deterrence is viewed as a coordination
 *   equilibrium (a 'rope') that prevents direct conflict between nuclear
 *   powers, but with inherent risks and costs. The constraint is classified
 *   as a Tangled Rope because it involves a genuine coordination function
 *   (preventing war) but also asymmetric extraction (nuclear powers benefit
 *   from stability, while the global population bears the existential risk).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.45).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.6).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Probability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'e5e9afa9-42d6-4026-ba88-d59966acc1ef').
narrative_ontology:cs_kernel_codification('e5e9afa9-42d6-4026-ba88-d59966acc1ef', distributed).
narrative_ontology:cs_authority_grounding('e5e9afa9-42d6-4026-ba88-d59966acc1ef', practice).
narrative_ontology:cs_interpretation_layer_present('e5e9afa9-42d6-4026-ba88-d59966acc1ef').
narrative_ontology:cs_reading_relation('e5e9afa9-42d6-4026-ba88-d59966acc1ef', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5e9afa9-42d6-4026-ba88-d59966acc1ef', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('e5e9afa9-42d6-4026-ba88-d59966acc1ef', foundational, total_war_remains_strategically_possible).
narrative_ontology:cs_axiom_status(total_war_remains_strategically_possible, holdable).
narrative_ontology:cs_axiom_grounding('e5e9afa9-42d6-4026-ba88-d59966acc1ef', total_war_remains_strategically_possible, empirically_contingent).
narrative_ontology:cs_axiom('e5e9afa9-42d6-4026-ba88-d59966acc1ef', foundational, deterrence_is_a_coordination_game).
narrative_ontology:cs_axiom_status(deterrence_is_a_coordination_game, holdable).
narrative_ontology:cs_axiom_grounding('e5e9afa9-42d6-4026-ba88-d59966acc1ef', deterrence_is_a_coordination_game, conventional).
narrative_ontology:cs_reference_frame('e5e9afa9-42d6-4026-ba88-d59966acc1ef', cold_war_deterrence_equilibrium).
narrative_ontology:cs_drift_state('e5e9afa9-42d6-4026-ba88-d59966acc1ef', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e5e9afa9-42d6-4026-ba88-d59966acc1ef', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, strategic_analysts).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, global_population).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, articulate deterrence doctrines, and engage in strategic signaling. They benefit from the stability deterrence provides but bear the cost and risk of maintaining the capability. Their exit options are constrained by the security dilemma.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Lives under the constant, if low, threat of nuclear annihilation. Bears the ultimate cost of deterrence failure. Has no direct agency in the maintenance or dismantling of nuclear arsenals.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, global_population, payer,
    powerless, immediate, trapped, global).

% Are protected by extended deterrence but also vulnerable to nuclear conflict. They bear the costs of living in a nuclear-armed world (e.g., proliferation risks, resource diversion to defense) without direct control over the deterrence mechanism. Their exit options are limited to seeking alliances or developing their own nuclear capabilities.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% Develop and refine deterrence theory, advising policymakers. They benefit from the intellectual challenge and professional standing derived from managing the nuclear dilemma. Their 'exit' is to shift focus to other areas of strategic studies, but their careers are often tied to the persistence of nuclear deterrence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_analysts, beneficiary,
    analytical, biographical, analytical, global).

% Advocate for nuclear disarmament and alternative security frameworks. They are largely excluded from the core decision-making processes of nuclear powers, despite representing a significant moral and political voice.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, peace_activists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of nuclear-armed states by establishing a mutual understanding that total war is too costly, thereby preventing direct military confrontation between them.
% TRANSFER_FUNCTION: Transfers the risk of catastrophic destruction from conventional warfare to the global population, in exchange for a perceived reduction in the probability of large-scale conventional conflict between major powers.
% ABSENT_VOICES: The global population, particularly those in non-nuclear states, and peace activists are largely absent from the direct strategic calculus, despite bearing the ultimate risk. They would argue for disarmament and alternative security paradigms.
% DISAPPEARANCE_RATIONALE: If the understanding that total war remains reachable vanished, the strategic calculus of nuclear powers would fundamentally shift. Without the existential threat, conventional military options might become more attractive, potentially leading to increased conflict or a rapid re-evaluation of nuclear arsenals and doctrines.
% FOUNDING_PROBLEM: The problem of preventing catastrophic large-scale warfare between great powers in an era of increasingly destructive conventional weaponry.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear powers and strategic analysts universally attest that the problem of preventing great power war remains live. Peace activists corroborate the problem's existence but dispute the efficacy and morality of the nuclear deterrence solution.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because while deterrence provides stability, it imposes a constant existential threat and diverts resources. Suppression is high (0.6) because the system relies on active enforcement (maintaining credible arsenals, signaling resolve) and suppresses alternatives like disarmament. Theater ratio is low (0.1) as the threat is largely real, not performative. Accessibility collapse is moderate (0.4) as alternatives (like conventional war or disarmament) are not entirely foreclosed but are heavily constrained. Resistance is moderate (0.3) from peace movements and non-nuclear states. The time series shows extractiveness peaking during the Cold War (Cuban Missile Crisis) and then dropping, reflecting periods of perceived détente and reduced tension, but remaining significant.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers and strategic analysts perceive deterrence as a necessary, if costly, coordination mechanism. The global population and non-nuclear states experience it as a constant, imposed threat. The engine's classification will reflect this divergence, with nuclear powers potentially seeing it as a Rope, while the global population experiences it as a Snare or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are agenda-setters and beneficiaries, gaining strategic stability and influence, though they also bear the costs of maintaining arsenals. The global population and non-nuclear states are victims, bearing the existential risk and indirect costs without direct control. Strategic analysts benefit from the intellectual domain and professional relevance. Peace activists are excluded, their voices marginalized in the core strategic discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling deterrence as a pure Snare by acknowledging its genuine coordination function in preventing total war. However, it also avoids mislabeling it as a pure Rope or Mountain by highlighting the significant, asymmetric extraction and active enforcement required for its persistence. The 'dropping probability' aspect acknowledges historical shifts in perceived risk without claiming the problem is solved or the constraint is natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_vs_risk,
    'Is the observed stability between nuclear powers a direct result of deterrence, or is it primarily due to other factors (e.g., economic interdependence, shared norms)?',
    'Counterfactual historical analysis or comparative studies of non-nuclear great power relations. If similar stability is observed in non-nuclear contexts, the causal link to deterrence is weakened.',
    'If deterrence is less causal, its coordination function is overstated, increasing its effective extractiveness and pushing it closer to a Snare. If highly causal, its Rope-like qualities are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_vs_risk, empirical, 'Assessing the true causal efficacy of nuclear deterrence in preventing total war.').

omega_variable(
    global_population_agency,
    'To what extent can the global population exert agency to alter the nuclear deterrence regime, given their ''trapped'' exit options?',
    'Analysis of historical instances where popular movements or non-nuclear states successfully influenced nuclear policy, or the development of new international legal frameworks.',
    'If agency is demonstrably higher than currently assessed, the ''trapped'' status of the global population is mitigated, potentially reducing the perceived suppression and extractiveness from that seat. If agency is negligible, the Snare-like qualities for this seat are reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_population_agency, empirical, 'Evaluating the potential for agency by the global population in nuclear policy.').

omega_variable(
    reachability_definition_ambiguity,
    'What constitutes ''reachability'' for total war? Is it merely technical capability, or does it include political will and strategic doctrine?',
    'Conceptual clarification through expert consensus or formal modeling of strategic decision-making under various conditions. The definition of ''total war'' itself is also part of this ambiguity.',
    'If reachability is defined narrowly (e.g., technical capability only), this reading''s claim of ''dropping probability'' might be less robust. If broadly defined, the claim is more nuanced and potentially more stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_definition_ambiguity, conceptual, 'Clarifying the definition of ''total war reachability'' and its components.').


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
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.85).
narrative_ontology:measurement(tota_be_t1989, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1989, 0.6).
narrative_ontology:measurement(tota_be_t2001, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(tota_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.9).
narrative_ontology:measurement(tota_su_t1989, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1989, 0.7).
narrative_ontology:measurement(tota_su_t2001, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(tota_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_proliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, arms_control_treaties).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_reachability_boundary' kernel. This 'dropping_reading' posits that total war remains reachable but less probable, with deterrence as a coordination equilibrium. It contrasts with the 'contraction_reading' (total war is no longer feasible) and the 'contingent_reachability_reading' (reachability is technology-dependent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
