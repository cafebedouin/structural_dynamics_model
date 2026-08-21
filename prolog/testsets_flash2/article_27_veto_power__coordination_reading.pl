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
 *   This constraint story analyzes the UN Security Council's Article 27 veto
 *   power from a 'coordination reading,' which posits the veto as a necessary
 *   mechanism to prevent great-power war. By ensuring no Security Council
 *   resolution can compel a nuclear state into military confrontation it
 *   rejects, the veto is seen as a safeguard for international stability.
 *   This reading emphasizes the collective benefit of avoiding catastrophic
 *   conflict, classifying the veto as a Rope due to its low extractiveness
 *   and high coordination function. The metrics reflect this view, showing
 *   minimal extraction and suppression, consistent with a coordination
 *   mechanism that is largely self-enforcing due to the high stakes of its
 *   failure.
 *
 * KEY AGENTS:
 *   - p5_nuclear_states: Agenda setter (institutional/constrained) — wields the veto, benefits from stability.
 *   - all_un_member_states: Beneficiary (organized/constrained) — benefits from avoided great-power war.
 *   - international_system_stability: Beneficiary (analytical/analytical) — abstract good, ultimate beneficiary.
 *   - un_secretariat: Observer (institutional/constrained) — administers the system, manages diplomatic fallout.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.1).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.05).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UNSC Article 27 Veto Power (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605').
narrative_ontology:cs_kernel_codification('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', fixed_text).
narrative_ontology:cs_authority_grounding('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', lineage).
narrative_ontology:cs_interpretation_layer_present('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605').
narrative_ontology:cs_reading_relation('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', foundational, great_power_unanimity_prevents_war).
narrative_ontology:cs_axiom_status(great_power_unanimity_prevents_war, holdable).
narrative_ontology:cs_axiom_grounding('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', great_power_unanimity_prevents_war, empirically_contingent).
narrative_ontology:cs_axiom('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', secondary, un_collapse_risk_without_veto).
narrative_ontology:cs_axiom_status(un_collapse_risk_without_veto, holdable).
narrative_ontology:cs_axiom_grounding('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', un_collapse_risk_without_veto, empirically_contingent).
narrative_ontology:cs_reference_frame('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', post_wwii_great_power_consensus).
narrative_ontology:cs_drift_state('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', contemporary_geopolitical_fragmentation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e8a27ac5-9f03-4ab5-8ed1-fc424c4e3605', '2024-07-30T12:00:00Z').
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

% As permanent members of the UN Security Council, these states hold the veto power, which they view as a necessary safeguard against being compelled into military action against their vital interests, thereby preventing direct great-power conflict. They benefit from the stability this mechanism provides.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_nuclear_states, agenda_setter,
    institutional, generational, constrained, global).

% All states, including non-P5 members, benefit from the veto power's function in preventing direct military confrontation between nuclear-armed great powers, which could escalate to global catastrophe. They accept the veto as a necessary evil for global stability.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, all_un_member_states, beneficiary,
    organized, generational, constrained, global).

% The overarching benefit of the veto, from this reading, is the preservation of the international system by preventing conflicts that could lead to its collapse. This is an abstract good that accrues to all actors.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_system_stability).

% Administers the UN system and observes the operation of the veto. While not directly benefiting or paying, it manages the diplomatic fallout and seeks consensus within the constraints of the veto power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_secretariat, observer,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that no UN Security Council resolution can force a nuclear-armed state into a military confrontation it deems against its core interests, thereby coordinating the avoidance of direct great-power war and maintaining global strategic stability.
% TRANSFER_FUNCTION: Transfers the power to block collective security action from the majority of the Security Council to any one of the P5 states, in exchange for the coordination benefit of preventing catastrophic great-power conflict.
% ABSENT_VOICES: States that are frequently targets of P5 vetoes (e.g., those seeking intervention in humanitarian crises blocked by a P5 member) would argue that the veto prioritizes great-power interests over human rights or international law. They are present in the UN General Assembly but lack Security Council voting power.
% DISAPPEARANCE_RATIONALE: If the veto power vanished overnight, the Security Council could theoretically compel P5 states into military action, leading to a high risk of direct great-power conflict, potentially involving nuclear weapons. The international security architecture would fundamentally destabilize, and the UN's role in maintaining peace would be radically altered.
% FOUNDING_PROBLEM: The primary problem was to create an international security organization that could prevent future world wars, specifically by ensuring that great powers (especially nuclear-armed ones) would remain engaged and not be forced into conflicts that would lead to the organization's collapse or global catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the UN Charter's drafting, international relations scholars focused on nuclear deterrence, and many non-P5 states (especially those reliant on the current security architecture) corroborate that the veto's original intent was to prevent great-power war, and that this problem remains live given the existence of nuclear weapons and geopolitical rivalries.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.1) and suppression (0.05) scores reflect the core premise of this reading: the veto is not primarily about extracting resources or coercing states, but about coordinating the avoidance of a worst-case scenario (great-power war). The 'cost' is the inability to act decisively against a P5 member's interests, which is seen as a necessary trade-off for global stability. Resistance is low (0.1) because, from this perspective, most states implicitly accept this trade-off. Accessibility collapse is high (0.8) because, without the veto, the alternative of a UN capable of compelling great powers is seen as leading to system collapse, not a viable alternative.
 *
 * PERSPECTIVAL GAP:
 *   From this coordination reading, all states are net beneficiaries of the veto's function in preventing great-power war. Other readings (oligopoly, sovereignty) would highlight different beneficiaries and victims, leading to different classifications. This story focuses solely on the coordination function, where the 'payer' is the collective inability to act against a P5 member, a cost deemed acceptable for the benefit of stability.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 nuclear states are agenda setters and beneficiaries, as they directly wield the veto and benefit from the stability it ensures. All other UN member states are also beneficiaries, as they are spared the consequences of great-power conflict. The international system stability itself is an abstract beneficiary. There are no direct 'victims' in this reading, as the constraint's primary function is seen as preventing a universally detrimental outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the veto as pure extraction by focusing on its original and ongoing function of preventing great-power war. If this function were to atrophy, and the veto became solely a tool for P5 states to block actions that genuinely serve global peace without risking great-power conflict, then the constraint would drift towards a Snare or Piton. However, as long as the risk of great-power war remains, this reading argues the mandate is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_as_coordination_vs_extraction,
    'Is the P5 veto primarily a coordination mechanism to prevent great-power war, or a tool for P5 states to extract geopolitical rents and entrench their power?',
    'Analysis of veto usage patterns: if vetoes consistently block actions that would genuinely lead to great-power conflict, it supports coordination. If vetoes primarily block actions that challenge P5 interests without risking direct conflict, it supports extraction.',
    'If resolved towards extraction, the constraint would reclassify as a Snare or Tangled Rope, with significantly higher extractiveness and identifiable victims (e.g., states affected by blocked humanitarian interventions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_coordination_vs_extraction, empirical, 'Ambiguity between the veto''s coordination function and its potential for power extraction.').

omega_variable(
    great_power_war_risk_assessment,
    'What is the actual, contemporary risk of direct military confrontation between nuclear-armed P5 states if the veto power were removed?',
    'Expert consensus from international security analysts, scenario modeling, and historical counterfactual analysis of past crises where the veto was used.',
    'If the risk is assessed as low, the coordination function''s justification weakens, potentially reclassifying the veto as a Piton (vestigial function) or Snare (pure extraction). If the risk is high, the Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_war_risk_assessment, empirical, 'Uncertainty about the ongoing necessity of the veto for preventing great-power war.').

omega_variable(
    coordination_vs_oligopoly_framing,
    'Is the P5 veto a legitimate coordination mechanism for global stability, or is it an illegitimate entrenchment of a geopolitical oligopoly?',
    'This is a conceptual and preference-based question, resolvable through normative debate and shifts in international legal and political consensus regarding the legitimacy of great-power privilege versus universal equality.',
    'A shift towards the oligopoly framing would fundamentally alter the perceived legitimacy and classification of the veto, likely moving it towards a Snare or Tangled Rope, even if the empirical function of preventing war remains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_oligopoly_framing, conceptual, 'The fundamental conceptual framing of the veto''s role in international governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__coordination_reading, theater_ratio, 1965, 0.06).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__coordination_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__coordination_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(arti_tr_t2024, article_27_veto_power__coordination_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__coordination_reading, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__coordination_reading, base_extractiveness, 1985, 0.08).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__coordination_reading, base_extractiveness, 2005, 0.1).
narrative_ontology:measurement(arti_be_t2024, article_27_veto_power__coordination_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__coordination_reading, suppression_requirement, 1965, 0.07).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__coordination_reading, suppression_requirement, 1985, 0.04).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__coordination_reading, suppression_requirement, 2005, 0.05).
narrative_ontology:measurement(arti_su_t2024, article_27_veto_power__coordination_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, un_security_council_resolutions).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, international_humanitarian_law_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
