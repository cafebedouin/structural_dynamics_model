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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UNSC Article 27 Veto Power (Coordination Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story analyzes the P5 veto power in the UN Security
 *   Council through the 'coordination reading,' which posits the veto as a
 *   necessary mechanism to prevent great-power military confrontation. It
 *   ensures that no Security Council resolution can compel a nuclear state
 *   into military action it rejects, thereby maintaining international
 *   stability. This reading emphasizes the veto's role in facilitating a
 *   'negative peace' by preventing the worst outcomes, even if it leads to
 *   inaction on other issues. The constraint is claimed as a Rope because it
 *   solves a genuine collective action problem (avoiding great-power war)
 *   with broad benefits, despite its asymmetric structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.15).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.05).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UNSC Article 27 Veto Power (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9').
narrative_ontology:cs_kernel_codification('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', fixed_text).
narrative_ontology:cs_authority_grounding('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', lineage).
narrative_ontology:cs_interpretation_layer_present('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9').
narrative_ontology:cs_reading_relation('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', foundational, great_power_unanimity_for_force).
narrative_ontology:cs_axiom_status(great_power_unanimity_for_force, holdable).
narrative_ontology:cs_axiom_grounding('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', great_power_unanimity_for_force, conventional).
narrative_ontology:cs_axiom('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', foundational, avoidance_of_great_power_war_is_paramount).
narrative_ontology:cs_axiom_status(avoidance_of_great_power_war_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', avoidance_of_great_power_war_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', post_wwii_pragmatic_consensus).
narrative_ontology:cs_drift_state('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0ed47ec0-9e9f-4c39-b4bd-fe138a3032a9', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, all_un_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members of the UN Security Council (China, France, Russia, United Kingdom, United States) who possess the veto power. From this reading, they use the veto to prevent the UN from compelling them into military action against their vital interests, thereby preventing direct great-power conflict. They are both beneficiaries of the stability and the primary agents of its maintenance.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_nuclear_states, agenda_setter,
    institutional, generational, arbitrage, global).

% All states, including those without veto power, benefit from the stability of the international system that this reading of the veto is claimed to provide. The avoidance of great-power war is a collective good, even if the mechanism for achieving it is asymmetric.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, all_un_member_states, beneficiary,
    organized, generational, constrained, global).

% The body within which the veto is exercised. Its resolutions are binding, but the veto ensures that no resolution can be passed that would directly threaten the vital interests of a P5 member, thus preserving the fragile consensus necessary for its operation.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the legal and political implications of the veto power, often debating its effectiveness in maintaining peace versus its role in paralyzing the Security Council. This reading emphasizes the 'negative peace' function.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct military confrontation between nuclear-armed great powers by ensuring no Security Council resolution can compel a P5 state into a conflict it deems against its vital interests, thereby avoiding escalation to global war.
% TRANSFER_FUNCTION: Transfers the 'cost' of potential Security Council paralysis (inability to act on certain issues) to the 'benefit' of avoiding great-power military conflict. It also transfers the power to block action to the P5 states.
% ABSENT_VOICES: Non-P5 states, particularly those that are victims of conflicts where the Security Council is deadlocked by a veto, would argue for reform or abolition of the veto. They are present in the UN General Assembly but lack direct power in the Security Council's decision-making on this matter.
% DISAPPEARANCE_RATIONALE: If the veto power vanished overnight, the Security Council could theoretically pass resolutions compelling P5 states. This would immediately raise the risk of direct military confrontation between nuclear powers, fundamentally altering the international security architecture and potentially leading to catastrophic global conflict. The entire system of collective security would need to be re-evaluated.
% FOUNDING_PROBLEM: The primary problem was to create an international organization capable of maintaining peace and security after two world wars, while acknowledging the reality of great-power politics and the destructive potential of nuclear weapons. The veto was designed to prevent the UN from becoming a tool for one bloc of great powers against another, which would lead to its collapse or a new world war.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the UN's founding, international relations theorists, and many diplomats (including those from non-P5 states who prioritize stability) corroborate that the veto was a pragmatic necessity for the UN's existence and for preventing great-power war. The ongoing risk of such conflicts, particularly with nuclear proliferation, keeps the problem live. This is attested by independent academic analysis and historical records, not just P5 self-assertion.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).

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
 *   The extractiveness (0.15) is low because, from this reading, the veto primarily serves a collective good (avoiding great-power war) rather than extracting rents. The 'cost' is Security Council paralysis on certain issues, which is seen as a necessary trade-off for stability. Suppression (0.05) is minimal because the veto's function is to prevent action, not to actively coerce. Theater ratio (0.1) is low, as the veto's function is largely direct and effective in preventing unwanted action. Accessibility collapse (0.8) is high because, without the veto, the alternative of a UN capable of compelling great powers into war is seen as highly unstable and dangerous. Resistance (0.1) is low from this perspective, as the core function of preventing great-power war is widely accepted as beneficial, even if the mechanism is criticized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of P5 states and those prioritizing global stability, the veto is a vital coordination mechanism. From the perspective of non-P5 states or those focused on humanitarian intervention, it can appear as an extractive tool that paralyzes the Council. This story focuses on the coordination reading, where the benefits of avoided great-power war outweigh the costs of inaction.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 nuclear states are the agenda-setters and primary beneficiaries (d near 0.0) as they directly wield the veto to protect their interests and benefit most directly from the avoided confrontation. All UN member states are also beneficiaries (d near 0.1-0.2) due to the collective good of international stability. There are no direct 'victims' in this reading, as the primary function is to prevent a universally detrimental outcome (great-power war). The 'cost' is diffuse inaction, not targeted extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the veto's mandate (preventing great-power war) is still very much live. The constraint has not atrophied; its function remains critical in a world with nuclear weapons and geopolitical rivalries. The classification as a Rope prevents mislabeling it as a Snare or Tangled Rope, which would imply a primary extractive function or a coordination function with significant asymmetric extraction, neither of which is central to this specific reading's interpretation of the veto's purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_function_ambiguity,
    'Is the P5 veto primarily a mechanism for preventing great-power war (coordination), or for entrenching geopolitical oligopoly (extraction), or for asserting state sovereignty (deontological principle)?',
    'Analysis of veto usage patterns over time: if vetoes consistently block actions that would lead to great-power confrontation, it supports the coordination reading. If they consistently block actions that would redistribute power or challenge P5 interests without direct war risk, it supports the oligopoly reading. If they consistently assert non-intervention, it supports the sovereignty reading.',
    'If resolved towards the oligopoly reading, the constraint would reclassify as a Snare or Tangled Rope with high extractiveness. If towards the sovereignty reading, it would remain a Rope but with a different foundational justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_function_ambiguity, conceptual, 'Ambiguity in the primary function of the P5 veto power.').

omega_variable(
    counterfactual_great_power_war,
    'How many great-power wars has the P5 veto actually prevented, and what is the probability of such a war if the veto were removed?',
    'Historical counterfactual analysis and expert geopolitical modeling. This is inherently difficult to quantify but can be estimated through scenario planning and historical comparison.',
    'A high number of prevented wars and a high probability of future war without the veto would strengthen the coordination reading and justify its low extractiveness. A low number would weaken it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_great_power_war, empirical, 'Empirical uncertainty regarding the veto''s effectiveness in preventing great-power war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__coordination_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__coordination_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__coordination_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(arti_tr_t2024, article_27_veto_power__coordination_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__coordination_reading, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__coordination_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__coordination_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(arti_be_t2024, article_27_veto_power__coordination_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__coordination_reading, suppression_requirement, 1965, 0.05).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__coordination_reading, suppression_requirement, 1985, 0.05).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__coordination_reading, suppression_requirement, 2005, 0.05).
narrative_ontology:measurement(arti_su_t2024, article_27_veto_power__coordination_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UN Security Council's Article 27 veto power. This 'coordination reading' emphasizes its role in preventing great-power war. The 'oligopoly reading' focuses on its extractive function, and the 'sovereignty reading' on its role in asserting state autonomy. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
