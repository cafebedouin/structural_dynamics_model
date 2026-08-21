% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Security Council Article 27 Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story analyzes the UN Security Council's Article 27 veto
 *   power through an 'oligopoly reading,' focusing on its role in
 *   structurally entrenching the geopolitical power of the Permanent Five
 *   (P5) members. It argues that the veto, enabled by the UN Charter's
 *   immutability, allows the P5 to extract ongoing authority rents and block
 *   institutional evolution that would redistribute power, despite
 *   significant shifts in global geopolitics since its inception. The
 *   constraint is claimed as a Snare, reflecting its extractive and
 *   suppressive nature from the perspective of the non-P5 majority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.85).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.9).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Security Council Article 27 Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '7314405e-78b4-46be-9e02-859b56929e1e').
narrative_ontology:cs_kernel_codification('7314405e-78b4-46be-9e02-859b56929e1e', fixed_text).
narrative_ontology:cs_authority_grounding('7314405e-78b4-46be-9e02-859b56929e1e', extraction).
narrative_ontology:cs_interpretation_layer_present('7314405e-78b4-46be-9e02-859b56929e1e').
narrative_ontology:cs_reading_relation('7314405e-78b4-46be-9e02-859b56929e1e', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('7314405e-78b4-46be-9e02-859b56929e1e', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('7314405e-78b4-46be-9e02-859b56929e1e', foundational, institutional_power_must_be_earned_and_revisable).
narrative_ontology:cs_axiom_status(institutional_power_must_be_earned_and_revisable, holdable).
narrative_ontology:cs_axiom_grounding('7314405e-78b4-46be-9e02-859b56929e1e', institutional_power_must_be_earned_and_revisable, deontological).
narrative_ontology:cs_axiom('7314405e-78b4-46be-9e02-859b56929e1e', foundational, geopolitical_power_should_reflect_contemporary_realities).
narrative_ontology:cs_axiom_status(geopolitical_power_should_reflect_contemporary_realities, holdable).
narrative_ontology:cs_axiom_grounding('7314405e-78b4-46be-9e02-859b56929e1e', geopolitical_power_should_reflect_contemporary_realities, conventional).
narrative_ontology:cs_reference_frame('7314405e-78b4-46be-9e02-859b56929e1e', post_wwii_power_distribution).
narrative_ontology:cs_drift_state('7314405e-78b4-46be-9e02-859b56929e1e', contemporary_multipolar_world, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7314405e-78b4-46be-9e02-859b56929e1e', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, global_south_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wield the veto to protect national interests, maintain geopolitical status, and block reforms that would dilute their power within the UN Security Council. They benefit from the ongoing authority rents derived from the Charter's immutability.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of an unreformed Security Council, lacking effective means to influence global security decisions or reform the institution. Their proposals for reform are consistently blocked by the P5.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_un_members, payer,
    organized, generational, trapped, global).

% A subset of non-P5 members, often disproportionately affected by conflicts and resolutions, whose calls for reform are consistently ignored. They are identity-locked due to their reliance on the UN system for legitimacy, aid, and a platform for multilateral diplomacy, despite its structural inequities.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, global_south_states, payer,
    organized, generational, identity_locked, global).

% Administers the UN system and documents the impact of the veto, but has no power to alter the Charter or the veto mechanism. Its role is to facilitate, not to reform the fundamental power structure.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, observer,
    institutional, biographical, constrained, global).

% Analyze the legal and political implications of the veto, often advocating for reform based on principles of equity, effectiveness, and contemporary geopolitical realities. They provide critical analysis but lack direct power to change the constraint.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_law_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto was originally intended to ensure great power consensus on security matters, preventing the UN from becoming an instrument of one bloc against another, thereby guaranteeing the participation of major powers in the new global security architecture.
% TRANSFER_FUNCTION: Transfers effective decision-making power and geopolitical leverage to the Permanent Five members, allowing them to block actions detrimental to their interests, at the cost of the collective security and legitimacy of the UN for the non-P5 majority.
% ABSENT_VOICES: A truly representative global assembly, reflecting contemporary population and economic power, would object to the anachronistic power distribution and demand fundamental reform of the Security Council's structure and the veto mechanism.
% DISAPPEARANCE_RATIONALE: If the veto power vanished, the Security Council's decision-making dynamics would fundamentally shift, potentially leading to more resolutions but also risking P5 disengagement or alternative security arrangements. The global power balance would be forced to reconfigure, and the UN's role in international security would be profoundly altered.
% FOUNDING_PROBLEM: To prevent a repeat of the League of Nations' failure by ensuring that major powers (the victors of WWII) had a mechanism to protect their vital interests, thereby guaranteeing their participation and commitment to the new global security architecture.
% FOUNDING_PROBLEM_CORROBORATION: Non-P5 member states, international legal scholars, and many civil society organizations attest that the original problem of ensuring great power buy-in has been superseded by the P5's use of the veto to maintain an outdated power structure and extract rents. The P5 themselves, however, maintain the problem is still live, citing ongoing geopolitical tensions.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.85) is high because the veto allows the P5 to unilaterally block collective action, effectively extracting a 'toll' on global security governance by preventing outcomes not aligned with their narrow interests. Suppression (0.90) is severe due to the absolute nature of the veto, which completely collapses alternatives for institutional reform or collective action against a P5 member's will. The theater ratio (0.15) is low because the veto is actively and functionally used by the P5 to achieve their objectives, rather than being a mere performance. Resistance (0.75) is high, evidenced by persistent calls for reform from the non-P5 majority, though these efforts are consistently suppressed by the veto itself. Accessibility collapse (0.95) is near total for non-P5 members seeking to alter the Security Council's power structure.
 *
 * PERSPECTIVAL GAP:
 *   From the P5's perspective, the veto is a necessary tool for maintaining global stability and protecting national interests, perhaps aligning with a 'rope' or 'mountain' classification. However, from the perspective of the non-P5 members, particularly Global South states, the same mechanism functions as a 'snare,' extracting power and suppressing legitimate demands for institutional reform. The engine's computation of per-seat classifications will highlight this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Permanent Five members are clear beneficiaries and agenda-setters, directly wielding the veto to their advantage (low directionality). Non-P5 UN members and Global South states are the primary targets/victims, bearing the costs of an unreformed system and lacking effective recourse (high directionality). The UN Secretariat and international law scholars act as observers, documenting the constraint's effects without directly benefiting or being targeted by its extractive function.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading posits that the veto's original mandate—to ensure great power consensus for global security—has largely atrophied. While the P5 still frame it as essential for stability, its contemporary use often appears to serve the purpose of maintaining an outdated geopolitical oligopoly, blocking institutional evolution and the redistribution of power. The persistence of the veto, despite its perceived obsolescence by many, suggests a form of mandatrophy where the structure continues to extract rents long after its initial coordination function has been superseded by new geopolitical realities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_function_ambiguity,
    'Is the Article 27 veto power primarily a mechanism for great power coordination, an assertion of state sovereignty, or an instrument of geopolitical oligopoly?',
    'Comparative analysis of veto usage patterns across different historical periods and geopolitical contexts, alongside an examination of P5 justifications versus non-P5 critiques.',
    'If the coordination or sovereignty readings are found to be structurally dominant, the constraint''s classification would shift towards a ''rope'' or ''mountain'' respectively, with significantly lower extractiveness and suppression. If the oligopoly reading is confirmed, its ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_function_ambiguity, conceptual, 'Ambiguity regarding the primary structural function of the UN Security Council veto power.').

omega_variable(
    mandate_drift_vs_original_intent,
    'Has the veto''s original mandate (preventing great power conflict) atrophied into a mechanism for rent extraction, or does it still serve its foundational purpose in contemporary geopolitics?',
    'Historical analysis of veto use in relation to actual great power conflicts versus blocking resolutions on humanitarian interventions, institutional reform, or actions against P5 allies. Corroboration from independent geopolitical analysts and international relations scholars.',
    'If the original mandate is demonstrably dead, the constraint''s classification as a ''snare'' is strongly supported. If it is found to still be a live and necessary coordination function, the classification would shift towards a ''tangled_rope'' or ''rope'', acknowledging a genuine coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_drift_vs_original_intent, empirical, 'Whether the veto''s original mandate has drifted, leading to mandatrophy and rent-seeking behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t16, article_27_veto_power__oligopoly_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(arti_tr_t32, article_27_veto_power__oligopoly_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(arti_tr_t48, article_27_veto_power__oligopoly_reading, theater_ratio, 48, 0.13).
narrative_ontology:measurement(arti_tr_t64, article_27_veto_power__oligopoly_reading, theater_ratio, 64, 0.14).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(arti_be_t16, article_27_veto_power__oligopoly_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(arti_be_t32, article_27_veto_power__oligopoly_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(arti_be_t48, article_27_veto_power__oligopoly_reading, base_extractiveness, 48, 0.82).
narrative_ontology:measurement(arti_be_t64, article_27_veto_power__oligopoly_reading, base_extractiveness, 64, 0.84).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(arti_su_t16, article_27_veto_power__oligopoly_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(arti_su_t32, article_27_veto_power__oligopoly_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(arti_su_t48, article_27_veto_power__oligopoly_reading, suppression_requirement, 48, 0.87).
narrative_ontology:measurement(arti_su_t64, article_27_veto_power__oligopoly_reading, suppression_requirement, 64, 0.89).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_security_council_resolutions).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, international_humanitarian_law).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_peacekeeping_operations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Article 27 veto power kernel, focusing on its role in entrenching geopolitical oligopoly. Sibling readings address coordination and sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
