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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Founder Stewardship Justification for Dual-Class Shares
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story represents the 'founder stewardship' reading of
 *   dual-class share structures, where concentrated founder control is seen
 *   as a legitimate mechanism to enable long-horizon mission execution,
 *   ultimately benefiting all shareholders. It is one reading of the
 *   'dual_class_legitimacy' kernel, distinct from readings focused on
 *   minority extraction or disclosure consent. The core claim is that the
 *   dual-class structure coordinates capital toward a shared, long-term
 *   vision by protecting the company from short-term market pressures.
 *
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
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Founder Stewardship Justification for Dual-Class Shares").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law/organizational_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '7cb27c0c-15e0-4a47-8802-8ed8173866fc').
narrative_ontology:cs_kernel_codification('7cb27c0c-15e0-4a47-8802-8ed8173866fc', formalized).
narrative_ontology:cs_authority_grounding('7cb27c0c-15e0-4a47-8802-8ed8173866fc', lineage).
narrative_ontology:cs_interpretation_layer_present('7cb27c0c-15e0-4a47-8802-8ed8173866fc').
narrative_ontology:cs_reading_relation('7cb27c0c-15e0-4a47-8802-8ed8173866fc', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('7cb27c0c-15e0-4a47-8802-8ed8173866fc', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('7cb27c0c-15e0-4a47-8802-8ed8173866fc', foundational, founder_vision_is_long_term_value).
narrative_ontology:cs_axiom_status(founder_vision_is_long_term_value, holdable).
narrative_ontology:cs_axiom_grounding('7cb27c0c-15e0-4a47-8802-8ed8173866fc', founder_vision_is_long_term_value, empirically_contingent).
narrative_ontology:cs_axiom('7cb27c0c-15e0-4a47-8802-8ed8173866fc', foundational, market_short_termism_threatens_mission).
narrative_ontology:cs_axiom_status(market_short_termism_threatens_mission, holdable).
narrative_ontology:cs_axiom_grounding('7cb27c0c-15e0-4a47-8802-8ed8173866fc', market_short_termism_threatens_mission, empirically_contingent).
narrative_ontology:cs_reference_frame('7cb27c0c-15e0-4a47-8802-8ed8173866fc', founder_led_mission_execution).
narrative_ontology:cs_drift_state('7cb27c0c-15e0-4a47-8802-8ed8173866fc', contemporary_governance_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7cb27c0c-15e0-4a47-8802-8ed8173866fc', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founding_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, long_term_mission_aligned_shareholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_a_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, institutional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting shares, enabling them to control strategic direction and resist short-term market pressures. They are seen as stewards of the company's long-term vision and mission, benefiting from its sustained success.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founding_shareholders, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold non-voting or low-voting shares. They benefit from the company's long-term value creation and mission execution, which is enabled by founder control, even if they lack direct governance input. Their benefit is indirect via mission success.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_a_shareholders, beneficiary,
    moderate, biographical, mobile, global).

% Invest in dual-class companies, accepting the concentrated control in exchange for potential long-term growth. They bear the cost of reduced governance influence but are compensated by the perceived stability and mission focus. They can exit by selling shares, but often face liquidity constraints.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, institutional_investors, payer,
    organized, immediate, constrained, global).

% Oversee the fairness and transparency of capital markets. They evaluate dual-class structures against investor protection mandates, balancing founder autonomy with minority shareholder rights.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns capital with a long-term mission by insulating strategic decisions from short-term market fluctuations and activist investor pressures, ensuring consistent execution of the founder's vision.
% TRANSFER_FUNCTION: Transfers governance control (voting power) from capital (Class A shareholders) to founding individuals, in exchange for perceived long-term value creation and mission stability.
% ABSENT_VOICES: Shareholder activists who prioritize immediate financial returns and proportional governance rights are often marginalized in dual-class structures. They would argue for 'one share, one vote' principles.
% DISAPPEARANCE_RATIONALE: If concentrated founder control vanished overnight, many mission-driven companies would face immediate pressure to prioritize short-term financial metrics, potentially altering their strategic direction, R&D investments, and long-term value proposition. The market for mission-driven companies would fundamentally reorganize.
% FOUNDING_PROBLEM: Companies with long-term, often disruptive, missions are vulnerable to short-term market pressures and hostile takeovers, which can derail their strategic vision and force premature exits or compromises.
% FOUNDING_PROBLEM_CORROBORATION: Founders and many long-term investors attest that the problem of short-term market pressure remains live, citing examples of companies forced to compromise their vision. Academic research on corporate governance also corroborates the existence of short-termism in public markets, supporting the need for mechanisms to protect long-term strategy.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is low (0.2) because, from this reading's perspective, the 'cost' of reduced voting power for Class A shareholders is offset by the 'benefit' of mission stability and long-term value creation. Suppression is low (0.15) as the structure is typically disclosed and accepted by investors, rather than coercively imposed. Theater ratio is low (0.1) because the mission-driven justification is considered genuine and actively pursued. The metrics reflect the internal logic of the founder stewardship claim.
 *
 * PERSPECTIVAL GAP:
 *   From the founder's perspective, this is a Rope, a necessary coordination mechanism. From a minority shareholder's perspective (as in the 'minority_extraction' reading), the same structure might compute as a Snare due to the lack of proportional governance and potential for self-dealing, even if the founder stewardship narrative is present. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding shareholders are clear beneficiaries and agenda-setters, as they retain control and guide the mission. Class A shareholders are also beneficiaries, as their investment is aligned with the long-term mission, and they are presumed to benefit from its success. Institutional investors, while accepting the terms, bear the cost of reduced governance influence, making them payers. Securities regulators are observers, evaluating the structure's impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues against mandatrophy by asserting that the founding problem (vulnerability to short-termism) remains live. The constraint's persistence is justified by its ongoing function in protecting long-term mission execution, preventing it from being mislabeled as a Piton or Snare where the original purpose has atrophied or was merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_stewardship_vs_self_interest,
    'To what extent does concentrated founder control genuinely serve long-term mission execution for all shareholders, versus enabling self-interested decisions that disproportionately benefit founders?',
    'Empirical studies comparing long-term performance and shareholder returns of dual-class vs. single-class companies, controlling for founder quality and industry. Analysis of specific founder decisions for evidence of self-dealing vs. mission alignment.',
    'If self-interest is dominant, the extractiveness metric for Class A shareholders would be higher, potentially reclassifying the constraint towards a Tangled Rope or Snare from their seat. If stewardship is consistently demonstrated, the Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_stewardship_vs_self_interest, empirical, 'Ambiguity between genuine stewardship and potential for self-enrichment under concentrated control.').

omega_variable(
    market_efficiency_vs_mission_protection,
    'Is the market''s ''short-termism'' a genuine threat to long-term mission, or an efficient mechanism for capital allocation that founders are resisting for personal control?',
    'Economic analysis of market efficiency in valuing long-term projects, and case studies of companies that thrived after founder control was diluted or removed.',
    'If market efficiency is robust, the justification for concentrated control weakens, increasing the perceived suppression and extractiveness for Class A shareholders. If short-termism is a significant problem, the coordination function of founder control is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_vs_mission_protection, conceptual, 'Contest over the nature of market pressures and the necessity of founder insulation.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''founder_stewardship'' reading of the ''dual_class_legitimacy'' kernel. How would the classification change under the ''minority_extraction'' or ''disclosure_consent'' readings?',
    'Generate separate constraint stories for each sibling reading, with their own metrics and stakeholder analyses, and compare the resulting classifications and per-seat types.',
    'The ''minority_extraction'' reading would likely yield significantly higher extractiveness and suppression, leading to a Snare classification for Class A shareholders. The ''disclosure_consent'' reading would focus on the adequacy of information, potentially shifting the constraint''s type based on transparency rather than control structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Documents the structural differences between this reading and its siblings within the ''dual_class_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t1980, dual_class_legitimacy__founder_stewardship, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(dual_tr_t1990, dual_class_legitimacy__founder_stewardship, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(dual_tr_t2000, dual_class_legitimacy__founder_stewardship, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(dual_tr_t2010, dual_class_legitimacy__founder_stewardship, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(dual_tr_t2024, dual_class_legitimacy__founder_stewardship, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(dual_be_t1980, dual_class_legitimacy__founder_stewardship, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(dual_be_t1990, dual_class_legitimacy__founder_stewardship, base_extractiveness, 1990, 0.17).
narrative_ontology:measurement(dual_be_t2000, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(dual_be_t2010, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2010, 0.19).
narrative_ontology:measurement(dual_be_t2024, dual_class_legitimacy__founder_stewardship, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t1980, dual_class_legitimacy__founder_stewardship, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(dual_su_t1990, dual_class_legitimacy__founder_stewardship, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(dual_su_t2000, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2000, 0.13).
narrative_ontology:measurement(dual_su_t2010, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(dual_su_t2024, dual_class_legitimacy__founder_stewardship, suppression_requirement, 2024, 0.15).


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
