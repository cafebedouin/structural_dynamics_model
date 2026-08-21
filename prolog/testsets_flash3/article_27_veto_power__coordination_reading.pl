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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UNSC Article 27 Veto Power (Coordination Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story presents the 'coordination reading' of the UN
 *   Security Council's Article 27 veto power. In this interpretation, the
 *   veto is a crucial mechanism for preventing great-power war, particularly
 *   among nuclear states, by ensuring that no Security Council resolution can
 *   compel a permanent member into military confrontation it rejects. It is
 *   framed as a necessary evil that underpins global stability, even if it
 *   sometimes leads to inaction on other pressing issues. This reading
 *   emphasizes the collective benefit of avoiding catastrophic conflict over
 *   the costs of blocked resolutions.
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
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UNSC Article 27 Veto Power (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'b0430d53-e360-4ab3-ae25-da0c7b1ea41a').
narrative_ontology:cs_kernel_codification('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', fixed_text).
narrative_ontology:cs_authority_grounding('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', lineage).
narrative_ontology:cs_interpretation_layer_present('b0430d53-e360-4ab3-ae25-da0c7b1ea41a').
narrative_ontology:cs_reading_relation('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', foundational, great_power_unanimity_prevents_war).
narrative_ontology:cs_axiom_status(great_power_unanimity_prevents_war, holdable).
narrative_ontology:cs_axiom_grounding('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', great_power_unanimity_prevents_war, empirically_contingent).
narrative_ontology:cs_axiom('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', foundational, un_coercion_of_nuclear_state_is_escalatory).
narrative_ontology:cs_axiom_status(un_coercion_of_nuclear_state_is_escalatory, holdable).
narrative_ontology:cs_axiom_grounding('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', un_coercion_of_nuclear_state_is_escalatory, empirically_contingent).
narrative_ontology:cs_reference_frame('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', post_wwii_collective_security_design).
narrative_ontology:cs_drift_state('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', contemporary_international_system, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b0430d53-e360-4ab3-ae25-da0c7b1ea41a', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members of the UN Security Council, each holding the power to veto substantive resolutions. From this reading, their veto power is a necessary safeguard against great-power conflict, ensuring no resolution can compel them into military action against their vital interests. They benefit from the stability this mechanism provides, preventing escalation.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_states, agenda_setter,
    institutional, generational, constrained, global).

% All other member states of the UN. They benefit from the prevention of great-power war, which would have catastrophic global consequences. While they do not possess the veto, this reading argues they are net beneficiaries of the stability it provides, even if it sometimes blocks action they desire.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_states, beneficiary,
    organized, biographical, constrained, global).

% The abstract state of reduced risk of large-scale interstate conflict, particularly involving nuclear powers. This reading posits that the veto directly contributes to this stability by preventing the UN from becoming a mechanism for coercive action against powerful states.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_system_stability).

% The administrative body of the UN, responsible for implementing Security Council resolutions. They observe the veto's impact on the Council's ability to act, often experiencing frustration when resolutions are blocked, but acknowledge its role in maintaining the foundational consensus of the P5.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_secretariat, observer,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that no Security Council resolution can compel a nuclear state into military confrontation it rejects, thereby preventing direct military conflict between great powers and maintaining global strategic stability.
% TRANSFER_FUNCTION: Transfers the risk of great-power war from the international system to the P5 states, who bear the responsibility of exercising the veto judiciously to prevent escalation, in exchange for their consent to the UN framework.
% ABSENT_VOICES: States or groups advocating for a more interventionist UN, or those who believe the veto is an anachronism that paralyzes the Council, are often marginalized in discussions about the veto's foundational purpose. They would argue for reform or abolition of the veto.
% DISAPPEARANCE_RATIONALE: If the P5 veto vanished overnight, the UN Security Council could theoretically pass resolutions compelling nuclear states into military action. This would immediately raise the risk of great-power war, leading to a fundamental reorganization of international security alliances and potentially the collapse of the UN as a collective security mechanism.
% FOUNDING_PROBLEM: The problem of preventing a third world war, particularly one involving nuclear-armed great powers, by ensuring that the UN's collective security mechanism would not inadvertently trigger such a conflict.
% FOUNDING_PROBLEM_CORROBORATION: Many international relations scholars and diplomats, including those from non-P5 states, corroborate that the risk of great-power conflict remains a live problem, and that the veto, despite its flaws, serves as a crucial circuit-breaker. Historical analysis of Cold War crises also supports this view.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is low (0.15) because, from this reading, the veto primarily serves a coordination function that benefits all states by preventing great-power war. The 'cost' is inaction, not direct extraction from a victim class. Suppression is also low (0.05) as the veto is a structural feature of the UN Charter, not something actively enforced against dissenting parties in a coercive manner; rather, it is a 'right' of the P5. Theater ratio is low (0.1) because the function of preventing great-power war is considered genuinely active and vital, not merely performative. Accessibility collapse is high (0.85) because, without the veto, the alternative of a UN capable of coercing great powers is seen as leading to a collapse of the international system into conflict.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the veto (e.g., oligopoly or sovereignty) would classify it very differently, highlighting its extractive or entrenching aspects. This coordination reading focuses solely on its function as a circuit-breaker for great-power conflict, leading to a low extractiveness score and a 'rope' classification. The engine's per-seat classification would likely align with 'rope' for all seats under this reading, as all are considered beneficiaries of avoided war.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 states are agenda-setters and beneficiaries, as the veto protects their vital interests and ensures their participation in the UN framework. Non-P5 states are also beneficiaries, as they gain from the stability and avoided great-power conflict. International system stability is an abstract beneficiary. No identifiable victim class exists in this reading, as the primary 'cost' (UN inaction) is considered a necessary trade-off for the greater good of peace.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_as_coordination_vs_extraction,
    'Is the P5 veto primarily a coordination mechanism preventing great-power war, or a tool for great powers to extract authority rents and block inconvenient action?',
    'Empirical analysis of veto usage patterns: if vetoes are primarily used to block resolutions directly threatening P5 vital security interests, it supports the coordination reading. If used frequently to block humanitarian interventions or resolutions on internal conflicts, it supports the extraction reading.',
    'If primarily extractive, the constraint''s extractiveness would be significantly higher, and its classification would shift towards ''tangled_rope'' or ''snare'' for non-P5 states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_coordination_vs_extraction, empirical, 'Ambiguity between the veto''s coordination function and its potential for extraction.').

omega_variable(
    inaction_as_cost_vs_benefit,
    'Is the UN Security Council''s inaction due to the veto a necessary cost for global stability, or an unacceptable failure to protect vulnerable populations?',
    'Normative and ethical frameworks: this is a preference-based question, resolved by which values (e.g., state sovereignty vs. humanitarian intervention) are prioritized.',
    'If inaction is viewed as an unacceptable cost, the ''victims'' array would be populated with vulnerable populations, and the extractiveness would rise, reflecting the cost borne by those denied protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inaction_as_cost_vs_benefit, preference, 'Whether UN inaction due to the veto is a net benefit or a net cost.').

omega_variable(
    reading_framing_underdetermination,
    'Does the ''coordination reading'' accurately capture the structural reality of the veto, or is it a selective framing that downplays its extractive or oligopolistic aspects?',
    'Comparative analysis with ''oligopoly_reading'' and ''sovereignty_reading'': assess which reading''s structural claims are most consistent with the full empirical record of veto usage and its consequences for different state actors.',
    'If an alternative reading is adopted, the constraint''s classification, extractiveness, and beneficiary/victim structure would fundamentally change, reflecting a different interpretation of the veto''s primary function and effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'The choice of reading for the Article 27 veto power is underdetermined by the raw facts, leading to different structural classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__coordination_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__coordination_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__coordination_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(arti_tr_t50, article_27_veto_power__coordination_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__coordination_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(arti_tr_t70, article_27_veto_power__coordination_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement(arti_tr_t79, article_27_veto_power__coordination_reading, theater_ratio, 79, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__coordination_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__coordination_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__coordination_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__coordination_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(arti_be_t50, article_27_veto_power__coordination_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__coordination_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(arti_be_t70, article_27_veto_power__coordination_reading, base_extractiveness, 70, 0.15).
narrative_ontology:measurement(arti_be_t79, article_27_veto_power__coordination_reading, base_extractiveness, 79, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__coordination_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__coordination_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__coordination_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__coordination_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__coordination_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(arti_su_t50, article_27_veto_power__coordination_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__coordination_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(arti_su_t70, article_27_veto_power__coordination_reading, suppression_requirement, 70, 0.05).
narrative_ontology:measurement(arti_su_t79, article_27_veto_power__coordination_reading, suppression_requirement, 79, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the Article 27 veto power. The 'oligopoly_reading' (veto as power entrenchment) and 'sovereignty_reading' (veto as Westphalian principle) are sibling constraints, each with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
