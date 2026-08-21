% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UNSC Article 27 Veto Power (Coordination Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story analyzes the P5 veto power in the UN Security
 *   Council through a 'coordination reading,' which posits the veto as a
 *   necessary mechanism to prevent great-power military confrontation. By
 *   requiring unanimity among the five nuclear-armed permanent members for
 *   any resolution authorizing force, the veto ensures no nuclear state is
 *   compelled into a military action it rejects, thereby safeguarding
 *   international system stability. The claimed type is 'rope' because, from
 *   this perspective, it solves a genuine collective-action problem (avoiding
 *   global war) with net benefits for all participants, despite its costs in
 *   terms of UNSC paralysis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.15).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.1).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UNSC Article 27 Veto Power (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'df84dc56-7173-4584-a664-9cffa0941353').
narrative_ontology:cs_kernel_codification('df84dc56-7173-4584-a664-9cffa0941353', fixed_text).
narrative_ontology:cs_authority_grounding('df84dc56-7173-4584-a664-9cffa0941353', lineage).
narrative_ontology:cs_interpretation_layer_present('df84dc56-7173-4584-a664-9cffa0941353').
narrative_ontology:cs_reading_relation('df84dc56-7173-4584-a664-9cffa0941353', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('df84dc56-7173-4584-a664-9cffa0941353', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('df84dc56-7173-4584-a664-9cffa0941353', foundational, great_power_unanimity_for_force).
narrative_ontology:cs_axiom_status(great_power_unanimity_for_force, holdable).
narrative_ontology:cs_axiom_grounding('df84dc56-7173-4584-a664-9cffa0941353', great_power_unanimity_for_force, deontological).
narrative_ontology:cs_axiom('df84dc56-7173-4584-a664-9cffa0941353', secondary, systemic_stability_priority).
narrative_ontology:cs_axiom_status(systemic_stability_priority, holdable).
narrative_ontology:cs_axiom_grounding('df84dc56-7173-4584-a664-9cffa0941353', systemic_stability_priority, instrumental).
narrative_ontology:cs_reference_frame('df84dc56-7173-4584-a664-9cffa0941353', post_wwii_collective_security_framework).
narrative_ontology:cs_drift_state('df84dc56-7173-4584-a664-9cffa0941353', contemporary_geopolitical_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('df84dc56-7173-4584-a664-9cffa0941353', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_un_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As permanent members of the UN Security Council, they wield the veto power, ensuring no resolution can compel them into military confrontation they reject. This preserves their national interests and is seen as a necessary safeguard against great-power war.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These states benefit from the overall stability of the international system, which is preserved by preventing direct military conflict between nuclear-armed great powers. However, they sometimes face frustration when collective action on other conflicts is blocked by a P5 veto.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_un_member_states, beneficiary,
    organized, biographical, constrained, global).

% The abstract good of a stable international order, which is preserved by the veto's function of preventing catastrophic direct conflict between major powers, especially those with nuclear capabilities.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_system_stability).

% Administers the United Nations and its various bodies. While committed to collective security, its capacity for action in certain crises is directly constrained by the P5 veto, which it must respect as part of the UN Charter.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_secretariat, observer,
    institutional, biographical, constrained, global).

% Academics and legal experts who analyze the legal and practical implications of the veto power, often debating its necessity for global stability against its potential for paralysis or abuse. They provide critical commentary on its function.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent direct military confrontation between nuclear-armed states by requiring unanimity among the P5 for Security Council resolutions authorizing the use of force, thereby avoiding catastrophic great-power war.
% TRANSFER_FUNCTION: Transfers the power to unilaterally block UN-mandated military action to each P5 member, effectively transferring a portion of collective security enforcement capacity to individual great-power discretion in exchange for reduced risk of global conflict.
% ABSENT_VOICES: States that are targets of aggression or internal conflict, where UN intervention is blocked by a P5 veto, would argue that the veto enables impunity and undermines collective security. Also, states advocating for a more democratic or equitable Security Council structure, who are excluded from the veto power itself.
% DISAPPEARANCE_RATIONALE: If the veto power vanished overnight, the risk of direct military confrontation between nuclear-armed states would increase dramatically, as the UNSC could compel action against a P5 member's vital interests. The UN collective security system would likely collapse, leading to a more fragmented and dangerous international order.
% FOUNDING_PROBLEM: The primary problem was to prevent a repeat of the World Wars, especially given the advent of nuclear weapons, by ensuring that great powers would not be compelled into military action against their vital interests, which could escalate into global conflict.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the UN's founding, many international relations theorists, and numerous non-P5 states (especially those reliant on the UN for stability) corroborate that the veto, while imperfect, has played a role in preventing larger conflicts. This perspective is supported by the historical absence of direct military conflict between P5 members.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.15) because the primary function is seen as coordination for global stability, with any 'extraction' being the unavoidable cost of preventing a greater catastrophe (great-power war). Suppression is also low (0.10) as the veto suppresses certain UN actions, not states themselves, and its existence is a negotiated feature of the UN Charter. The theater ratio is very low (0.05) because the function of preventing direct great-power conflict is considered genuine and vital. Accessibility collapse is moderate (0.20) as the veto limits UN-mandated actions but does not eliminate other diplomatic or military options for states. Resistance (0.30) exists from non-P5 states frustrated by paralysis, but the core principle of avoiding great-power war is widely accepted as necessary.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the P5 veto (e.g., the 'oligopoly' or 'sovereignty' readings) would emphasize the veto's role in entrenching power or asserting absolute state autonomy, leading to higher extractiveness and suppression scores. This 'coordination reading' focuses solely on the conflict-prevention function, which is why its metrics reflect a 'rope' classification. The engine's per-seat classification would show all seats as beneficiaries of this coordination, even if some are frustrated by its side effects.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 states are the agenda-setters and direct beneficiaries, as the veto protects their vital interests and prevents them from being coerced. Non-P5 UN member states are also beneficiaries, as they gain from the stability of avoided great-power war, even if they sometimes bear the cost of UNSC inaction. International system stability is an abstract beneficiary. There are no direct 'victims' in this reading, as all states are considered to benefit from the prevention of global conflict.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the P5 veto primarily a coordination mechanism for great powers, or is its coordination function a cover for geopolitical oligopoly or an expression of absolute sovereignty?',
    'Comparative analysis of UNSC actions/inactions, P5 state behavior, and the outcomes of alternative institutional designs. Examination of historical records and diplomatic negotiations surrounding the veto''s inception and use.',
    'If the oligopoly or sovereignty framing is dominant, the constraint''s extractiveness and suppression would be significantly higher, potentially leading to reclassification as a ''tangled_rope'' or ''snare'' from the perspective of non-P5 states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the primary structural function of the P5 veto power.').

omega_variable(
    effectiveness_vs_paralysis_tradeoff,
    'Does the veto primarily prevent great-power war, or does it primarily cause paralysis in response to other conflicts, thereby undermining collective security and human rights?',
    'Empirical study of UNSC interventions and non-interventions, correlating veto use with conflict outcomes, humanitarian crises, and great-power involvement. Analysis of counterfactual scenarios where the veto was absent.',
    'If paralysis and its negative consequences are deemed to outweigh the benefits of great-power war prevention, the effective extractiveness of the constraint (in terms of foregone collective action and human cost) would be higher, potentially shifting classification towards a ''tangled_rope'' due to its asymmetric costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_paralysis_tradeoff, empirical, 'The balance between the veto''s conflict-prevention and collective-security-paralysis functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__coordination_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__coordination_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__coordination_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__coordination_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__coordination_reading, base_extractiveness, 1965, 0.14).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__coordination_reading, base_extractiveness, 1985, 0.16).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__coordination_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__coordination_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.08).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__coordination_reading, suppression_requirement, 1965, 0.1).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__coordination_reading, suppression_requirement, 1985, 0.11).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__coordination_reading, suppression_requirement, 2005, 0.1).
narrative_ontology:measurement(arti_su_t2025, article_27_veto_power__coordination_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, un_security_council_resolutions).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, international_humanitarian_law).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the P5 veto power, focusing on its role in preventing great-power conflict. Sibling readings include 'article_27_veto_power__oligopoly_reading' and 'article_27_veto_power__sovereignty_reading', which emphasize power distribution and state autonomy, respectively. All three are distinct constraints derived from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
