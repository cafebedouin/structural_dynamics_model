% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Legitimacy: Founder Stewardship Reading
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint describes the 'founder_stewardship' reading of dual-class
 *   share structures, where concentrated founder control is seen as a
 *   legitimate and beneficial mechanism for all shareholders. From this
 *   perspective, the structure enables long-horizon mission execution,
 *   protects the company from short-term market pressures, and ultimately
 *   drives greater value creation than a single-class structure would. The
 *   founder is viewed as a fiduciary steward whose control aligns with the
 *   long-term interests of the enterprise and its public shareholders.
 *
 * KEY AGENTS:
 *   - founder_ceo: Primary agenda-setter and beneficiary (institutional/identity_locked) — bears responsibility for mission, benefits from control.
 *   - class_b_shareholders: Beneficiaries (powerful/identity_locked) — retain voting control, align with founder's long-term vision.
 *   - class_a_shareholders: Beneficiaries (moderate/constrained) — benefit indirectly from mission success and long-term value creation.
 *   - institutional_investors: Payers (organized/constrained) — forgo proportional governance rights in exchange for access to the company's growth potential.
 *   - securities_regulators: Observer (institutional/analytical) — monitor compliance and assess market fairness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.25).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.4).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.25).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Legitimacy: Founder Stewardship Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '3023296f-8ad4-4894-a1fc-34f16982daa8').
narrative_ontology:cs_kernel_codification('3023296f-8ad4-4894-a1fc-34f16982daa8', formalized).
narrative_ontology:cs_authority_grounding('3023296f-8ad4-4894-a1fc-34f16982daa8', lineage).
narrative_ontology:cs_interpretation_layer_present('3023296f-8ad4-4894-a1fc-34f16982daa8').
narrative_ontology:cs_reading_relation('3023296f-8ad4-4894-a1fc-34f16982daa8', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('3023296f-8ad4-4894-a1fc-34f16982daa8', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('3023296f-8ad4-4894-a1fc-34f16982daa8', foundational, founder_vision_creates_long_term_value).
narrative_ontology:cs_axiom_status(founder_vision_creates_long_term_value, holdable).
narrative_ontology:cs_axiom_grounding('3023296f-8ad4-4894-a1fc-34f16982daa8', founder_vision_creates_long_term_value, instrumental).
narrative_ontology:cs_axiom('3023296f-8ad4-4894-a1fc-34f16982daa8', foundational, control_stability_enables_mission_execution).
narrative_ontology:cs_axiom_status(control_stability_enables_mission_execution, holdable).
narrative_ontology:cs_axiom_grounding('3023296f-8ad4-4894-a1fc-34f16982daa8', control_stability_enables_mission_execution, instrumental).
narrative_ontology:cs_reference_frame('3023296f-8ad4-4894-a1fc-34f16982daa8', founder_led_innovation_model).
narrative_ontology:cs_drift_state('3023296f-8ad4-4894-a1fc-34f16982daa8', contemporary_corporate_governance_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3023296f-8ad4-4894-a1fc-34f16982daa8', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_ceo).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_b_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, employees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, institutional_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, long_term_value_creation_theory).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__founder_stewardship, stewardship_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The visionary leader who retains disproportionate voting control through Class B shares. They are responsible for setting and executing the long-term mission, believing this structure protects the company from short-term market pressures and enables greater value creation for all shareholders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_ceo, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Typically the founder and early insiders who hold shares with superior voting rights. They benefit from the ability to maintain strategic control and align the company's trajectory with its founding mission, insulated from external pressures.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_b_shareholders, beneficiary,
    powerful, generational, identity_locked, global).

% Public shareholders who hold shares with inferior or no voting rights. From this reading, they benefit indirectly from the long-term value creation and mission stability enabled by founder control, accepting the governance structure as a trade-off for growth potential.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders, beneficiary,
    moderate, biographical, constrained, global).

% Large investment funds that invest in dual-class companies, accepting the concentrated control structure. They 'pay' by forgoing proportional governance rights, but do so with the expectation of superior long-term returns driven by founder vision. Their exit is constrained by market liquidity and fiduciary duties.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, institutional_investors, payer,
    organized, immediate, constrained, global).

% Benefit from the stability and clear strategic direction provided by founder control, which can foster a strong corporate culture and long-term career opportunities. Their exit options are tied to the labor market.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, employees, beneficiary,
    moderate, biographical, constrained, local).

% Oversees the company's management and strategy. While legally bound to all shareholders, the board in a dual-class structure often aligns closely with the founder's vision, interpreting their role as supporting long-term mission execution.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, board_of_directors, agenda_setter,
    institutional, generational, constrained, global).

% Government bodies tasked with ensuring fair and orderly markets. They observe dual-class structures, assessing their impact on investor protection and market integrity, and can propose rule changes or enforcement actions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_ceo).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates long-term strategic vision and capital allocation by insulating the company's mission from short-term market fluctuations and activist pressures, ensuring consistent leadership and direction.
% TRANSFER_FUNCTION: Transfers disproportionate voting control and decision-making power from dispersed public shareholders to the founder and early insiders, in exchange for perceived long-term value creation and mission stability.
% ABSENT_VOICES: Advocates for 'one-share-one-vote' principles, short-term activist investors, and some governance advisory firms are structurally marginalized or excluded from influencing the core governance structure. They would argue for greater shareholder democracy and accountability.
% DISAPPEARANCE_RATIONALE: If concentrated founder control vanished overnight, the company would immediately become vulnerable to short-term market pressures, hostile takeovers, and activist campaigns. The long-term mission would likely be compromised, strategic direction would fragment, and the company's unique culture and innovation trajectory could be lost, leading to a significant reorganization of its governance and market strategy.
% FOUNDING_PROBLEM: The founding problem was how to raise significant capital for growth while preserving the founder's long-term vision and mission, protecting the company from the pressures of quarterly earnings and short-term market demands.
% FOUNDING_PROBLEM_CORROBORATION: Founders and many long-term institutional investors attest that the problem of short-term market pressure on long-horizon innovation remains live. Academic research on corporate governance also frequently discusses the tension between short-term market demands and long-term strategic execution, corroborating the persistence of this challenge from outside the immediate beneficiary group.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) reflects the view that the control premium is justified by the value created through long-term mission execution, benefiting all shareholders. Suppression (0.4) is moderate, representing the structural dampening of short-term market pressures and activist investor influence, which is seen as a feature, not a bug, from this perspective. Theater ratio (0.1) is low because the governance structure is considered genuinely functional in achieving its stated goals. Accessibility collapse (0.4) is moderate as alternative governance structures exist, but this one is chosen for specific strategic reasons. Resistance (0.2) is low because, from this reading, the structure is legitimate and accepted by many long-term investors.
 *
 * PERSPECTIVAL GAP:
 *   While this story presents the founder stewardship perspective, it acknowledges that other readings (e.g., minority extraction) exist and would yield different classifications. The engine's per-seat classification will reflect the differing experiences of shareholders based on their control rights and exit options, even within this 'beneficial' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The founder_ceo and class_b_shareholders are clear beneficiaries, as they retain control and guide the company's long-term trajectory. Class_a_shareholders and employees are also considered beneficiaries, as their interests are seen as aligned with the long-term mission and value creation enabled by founder control. Institutional_investors are payers in the sense that they accept reduced governance rights in exchange for investment opportunity, but are not 'victims' in this reading, as they are presumed to consent to the terms.
 *
 * MANDATROPHY ANALYSIS:
 *   From the founder stewardship perspective, the dual-class structure's mandate (protecting long-term mission) is very much alive. The classification as a Rope prevents mislabeling this coordination mechanism as pure extraction, recognizing its genuine function in aligning long-term interests and resisting short-termism, even while acknowledging the concentration of control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_extraction_ambiguity,
    'Is concentrated founder control primarily a mechanism for long-term stewardship and mission execution, or does it enable extraction from minority shareholders?',
    'Empirical analysis of long-term shareholder returns (Class A vs. Class B), founder compensation relative to performance, and governance decisions in dual-class vs. single-class firms over extended periods, controlling for industry and firm age.',
    'If primarily stewardship, the constraint is a Rope; if primarily extraction, it is a Snare. This reading assumes stewardship, but the alternative is a live contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_extraction_ambiguity, empirical, 'Ambiguity between genuine stewardship and disguised extraction in dual-class structures.').

omega_variable(
    long_term_value_definition,
    'How is ''long-term value'' defined and measured in practice, and whose interests does that definition primarily serve?',
    'Content analysis of corporate disclosures, founder statements, and board minutes; correlation of ''long-term'' initiatives with founder-specific benefits (e.g., control premium, specific strategic directions).',
    'If ''long-term value'' aligns broadly with all shareholder interests, the stewardship claim holds. If it disproportionately benefits founders or specific strategic paths at the expense of broader market returns, the extraction reading gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_value_definition, conceptual, 'The contested definition and beneficiaries of ''long-term value'' in founder-led firms.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''dual_class_legitimacy'' kernel. The specific reading instantiated here is ''founder_stewardship''. Sibling readings include ''minority_extraction'' and ''disclosure_consent''.',
    'Not resolvable within this story; this omega documents the kernel context.',
    'Different readings lead to different classifications and policy implications for dual-class structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents the kernel and reading context for dual-class legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.09).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.1).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.1).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, resource_allocation).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dual_class_legitimacy' kernel, focusing on the founder stewardship perspective. It is linked to sibling readings that offer alternative interpretations of dual-class share structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
