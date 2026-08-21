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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Founder Stewardship Justification for Dual-Class Shares
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'founder_stewardship' reading of
 *   the 'dual_class_legitimacy' kernel. From this perspective, concentrated
 *   founder control, enabled by dual-class share structures, is a legitimate
 *   and beneficial mechanism that serves all shareholders by protecting the
 *   company's long-horizon mission from short-term market pressures. The
 *   dual-class structure is seen as a coordination mechanism that aligns
 *   long-term vision with execution, with founders acting as fiduciary
 *   stewards.
 *
 * KEY AGENTS:
 *   - founder_controlling_shareholders: Primary agenda_setter (institutional/generational) — protects mission
 *   - class_a_minority_shareholders: Primary beneficiary (moderate/biographical) — benefits from mission-driven growth
 *   - institutional_investors: Beneficiary/Payer (powerful/biographical) — accepts control for growth, may push for reforms
 *   - securities_regulators: Analytical observer (institutional/generational) — oversees legality and disclosure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.2).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.15).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.2).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Founder Stewardship Justification for Dual-Class Shares").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '713d21d4-254f-404b-bd2a-a0fff1d62bde').
narrative_ontology:cs_kernel_codification('713d21d4-254f-404b-bd2a-a0fff1d62bde', formalized).
narrative_ontology:cs_authority_grounding('713d21d4-254f-404b-bd2a-a0fff1d62bde', lineage).
narrative_ontology:cs_interpretation_layer_present('713d21d4-254f-404b-bd2a-a0fff1d62bde').
narrative_ontology:cs_reading_relation('713d21d4-254f-404b-bd2a-a0fff1d62bde', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('713d21d4-254f-404b-bd2a-a0fff1d62bde', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('713d21d4-254f-404b-bd2a-a0fff1d62bde', foundational, founder_vision_creates_long_term_value).
narrative_ontology:cs_axiom_status(founder_vision_creates_long_term_value, holdable).
narrative_ontology:cs_axiom_grounding('713d21d4-254f-404b-bd2a-a0fff1d62bde', founder_vision_creates_long_term_value, instrumental).
narrative_ontology:cs_axiom('713d21d4-254f-404b-bd2a-a0fff1d62bde', foundational, insulation_from_short_termism_is_beneficial).
narrative_ontology:cs_axiom_status(insulation_from_short_termism_is_beneficial, holdable).
narrative_ontology:cs_axiom_grounding('713d21d4-254f-404b-bd2a-a0fff1d62bde', insulation_from_short_termism_is_beneficial, empirically_contingent).
narrative_ontology:cs_reference_frame('713d21d4-254f-404b-bd2a-a0fff1d62bde', founder_led_mission_execution).
narrative_ontology:cs_drift_state('713d21d4-254f-404b-bd2a-a0fff1d62bde', contemporary_market_conditions, gap(stable, minor, true)).
narrative_ontology:cs_created_at('713d21d4-254f-404b-bd2a-a0fff1d62bde', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_controlling_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_minority_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, institutional_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, institutional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds super-voting shares, enabling long-term strategic decisions and mission protection, ostensibly for the benefit of all shareholders. They set the company's strategic direction and are insulated from short-term market pressures.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_controlling_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Invest in the company, trusting the founder's vision and long-term value creation, accepting limited voting rights in exchange for potential mission-driven growth. They benefit from the stability and strategic focus enabled by founder control.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_minority_shareholders, beneficiary,
    moderate, biographical, constrained, global).

% Invest in dual-class companies, often accepting the founder's control for access to high-growth companies with a clear mission. While they forgo proportional governance rights (payer aspect), they benefit from the long-term value creation this structure is claimed to enable.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, institutional_investors, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, institutional_investors, payer).

% Oversee the legality and disclosure of dual-class structures, balancing investor protection with corporate flexibility. They analyze the claims of founder stewardship against market outcomes and investor sentiment.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_controlling_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns long-term strategic vision with corporate execution by insulating founders from short-term market pressures, ensuring mission continuity and fostering innovation that requires extended timelines.
% TRANSFER_FUNCTION: Transfers governance control from capital (Class A shares) to founder vision (super-voting shares), with the implicit promise of long-term value creation for all shareholders through mission-driven growth and strategic stability.
% ABSENT_VOICES: Advocates for 'one share, one vote' principles, who would argue that all capital providers should have proportional governance rights, are often excluded from the initial structuring negotiations and ongoing governance discussions, as their perspective directly challenges the premise of founder stewardship.
% DISAPPEARANCE_RATIONALE: If founder control vanished overnight, many mission-driven companies would face immediate pressure to prioritize short-term profits, potentially altering their strategic direction, R&D investments, and long-term value proposition. This would lead to a significant reorganization of capital allocation and corporate strategy, as companies would adapt to a more traditional governance model.
% FOUNDING_PROBLEM: Public companies often face short-term market pressures from activist investors and quarterly reporting cycles that force founders to compromise long-term vision and mission, leading to suboptimal strategic decisions and potential value destruction.
% FOUNDING_PROBLEM_CORROBORATION: Founders and long-term investors consistently attest to the ongoing challenge of short-termism in public markets. Academic research on corporate governance and organizational longevity from independent scholars also supports the existence of this problem, though its optimal solution and the efficacy of dual-class structures remain debated.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.20) and suppression (0.15) reflect the 'rope' claim of this reading, where the control structure is viewed as a necessary cost for coordination and long-term value creation, rather than extraction. Theater ratio is low (0.10) because the governance structure is genuinely functional in protecting the founder's vision. Accessibility collapse is moderate (0.45) as alternative single-class structures exist, but this model is chosen for its perceived benefits. Resistance is low (0.15) from this perspective, as shareholders are assumed to consent to the structure for long-term gains.
 *
 * PERSPECTIVAL GAP:
 *   While this story presents the 'founder_stewardship' reading, other perspectives (e.g., 'minority_extraction') would interpret the same structural facts as highly extractive. The engine's per-seat classification will highlight how different stakeholders experience this constraint differently, even under this 'rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder controlling shareholders are beneficiaries and agenda-setters, as they maintain control and execute their vision. Class A minority shareholders and institutional investors are also considered beneficiaries in this reading, as they are expected to benefit from the long-term value created by founder stewardship, despite their limited voting rights. Securities regulators act as observers, evaluating the system's fairness and compliance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'This constraint is the ''founder_stewardship'' reading of the ''dual_class_legitimacy'' kernel. Sibling readings include ''minority_extraction'' and ''disclosure_consent''.',
    'Comparison with alternative readings and their respective empirical and conceptual justifications.',
    'The classification of this constraint (rope) is specific to the ''founder_stewardship'' reading. Other readings would yield different classifications (e.g., ''minority_extraction'' would likely be a snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Contextualizes this constraint as one reading of a contested kernel.').

omega_variable(
    stewardship_vs_entrenchment_empirical,
    'Does founder control consistently lead to superior long-term performance and mission execution for all shareholders compared to single-class structures, or does it primarily serve to entrench founder power and potentially harm minority shareholders?',
    'Longitudinal empirical studies comparing the financial performance, innovation rates, and shareholder returns of dual-class companies versus single-class companies over extended periods, controlling for industry and market conditions.',
    'If empirical evidence strongly supports superior long-term performance, it reinforces the ''rope'' classification. If evidence suggests entrenchment or underperformance, it would shift the classification towards ''tangled_rope'' or ''snare'' by undermining the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_entrenchment_empirical, empirical, 'Empirical test of whether founder stewardship claims hold true in practice.').

omega_variable(
    stewardship_to_extraction_transition,
    'At what point does ''stewardship'' transition into ''entrenchment'' or ''extraction'' if company performance falters, the founder''s vision becomes misaligned with market realities, or the founder''s personal interests diverge from the company''s mission?',
    'Case studies of dual-class companies experiencing governance crises or founder succession issues, analyzed through the lens of shareholder value and mission integrity. Legal and ethical frameworks for fiduciary duty in concentrated control structures.',
    'Defining this transition point would provide clearer criteria for reclassifying the constraint from ''rope'' to a more extractive type (e.g., ''tangled_rope'' or ''snare'') when the stewardship function degrades.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stewardship_to_extraction_transition, conceptual, 'Conceptual boundary between legitimate stewardship and illegitimate extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dual_tr_t6, dual_class_legitimacy__founder_stewardship, theater_ratio, 6, 0.09).
narrative_ontology:measurement(dual_tr_t12, dual_class_legitimacy__founder_stewardship, theater_ratio, 12, 0.1).
narrative_ontology:measurement(dual_tr_t18, dual_class_legitimacy__founder_stewardship, theater_ratio, 18, 0.1).
narrative_ontology:measurement(dual_tr_t24, dual_class_legitimacy__founder_stewardship, theater_ratio, 24, 0.11).
narrative_ontology:measurement(dual_tr_t30, dual_class_legitimacy__founder_stewardship, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(dual_be_t6, dual_class_legitimacy__founder_stewardship, base_extractiveness, 6, 0.19).
narrative_ontology:measurement(dual_be_t12, dual_class_legitimacy__founder_stewardship, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(dual_be_t18, dual_class_legitimacy__founder_stewardship, base_extractiveness, 18, 0.2).
narrative_ontology:measurement(dual_be_t24, dual_class_legitimacy__founder_stewardship, base_extractiveness, 24, 0.21).
narrative_ontology:measurement(dual_be_t30, dual_class_legitimacy__founder_stewardship, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(dual_su_t6, dual_class_legitimacy__founder_stewardship, suppression_requirement, 6, 0.13).
narrative_ontology:measurement(dual_su_t12, dual_class_legitimacy__founder_stewardship, suppression_requirement, 12, 0.14).
narrative_ontology:measurement(dual_su_t18, dual_class_legitimacy__founder_stewardship, suppression_requirement, 18, 0.15).
narrative_ontology:measurement(dual_su_t24, dual_class_legitimacy__founder_stewardship, suppression_requirement, 24, 0.15).
narrative_ontology:measurement(dual_su_t30, dual_class_legitimacy__founder_stewardship, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
