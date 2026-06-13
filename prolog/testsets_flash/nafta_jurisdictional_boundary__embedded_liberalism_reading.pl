% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'embedded liberalism' reading of NAFTA's
 *   jurisdictional boundary, where the trade agreement is understood as a
 *   framework that facilitates market access while explicitly preserving
 *   legitimate domestic policy space for environmental, labor, and health
 *   standards, provided they are non-discriminatory. This reading
 *   acknowledges a partial jurisdictional overlap, where regulatory agencies
 *   retain defensive authority, but also recognizes the moderate extraction
 *   imposed through the costs of litigation and the chilling effect of
 *   potential trade challenges.
 *
 * KEY AGENTS:
 *   - exporting_industries: Primary beneficiary (institutional/arbitrage) — gain market access
 *   - domestic_regulatory_agencies: Primary beneficiary (institutional/constrained) — retain policy space
 *   - domestic_industries_facing_imports: Primary payer (organized/constrained) — face increased competition
 *   - environmental_advocacy_groups: Primary payer (organized/constrained) — face challenges to standards
 *   - trade_dispute_panels: Agenda setter (institutional/analytical) — adjudicate disputes
 *   - capital_supremacy_advocates: Excluded (institutional/analytical) — argue for trade primacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.35).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'cc78860a-d8c9-43f2-aedd-081d0f3a7b2d').
narrative_ontology:cs_kernel_codification('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', fixed_text).
narrative_ontology:cs_authority_grounding('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', lineage).
narrative_ontology:cs_interpretation_layer_present('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d').
narrative_ontology:cs_reading_relation('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', foundational, trade_liberalization_conditional_on_domestic_policy_space).
narrative_ontology:cs_axiom_status(trade_liberalization_conditional_on_domestic_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', trade_liberalization_conditional_on_domestic_policy_space, conventional).
narrative_ontology:cs_axiom('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', secondary, non_discriminatory_standards_are_legitimate).
narrative_ontology:cs_axiom_status(non_discriminatory_standards_are_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', non_discriminatory_standards_are_legitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', post_bretton_woods_compromise).
narrative_ontology:cs_drift_state('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', contemporary_globalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cc78860a-d8c9-43f2-aedd-081d0f3a7b2d', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_industries_facing_imports).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_advocacy_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced trade barriers and expanded market access in partner countries, leading to increased sales and profits. They actively lobby for interpretations that favor trade liberalization.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries, beneficiary,
    institutional, biographical, arbitrage, global).

% Retain the authority to set and enforce environmental, labor, and health standards, provided they are non-discriminatory and pursue legitimate policy objectives. They must, however, be prepared to defend these regulations in trade disputes.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Face increased competition from imported goods and services due to reduced tariffs and non-tariff barriers. They may need to adapt their business models or lobby for protection, bearing the costs of market adjustment.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_industries_facing_imports, payer,
    organized, biographical, constrained, national).

% Work to strengthen domestic environmental standards but face the risk of these standards being challenged as trade barriers. They bear the costs of legal defense and public advocacy to preserve policy space.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_advocacy_groups, payer,
    organized, generational, constrained, national).

% Adjudicate disputes between member states regarding trade obligations and domestic regulations. Their rulings interpret the balance between market access and policy space, effectively enforcing the constraint.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, immediate, analytical, global).

% Argue for the primacy of trade and investment liberalization over domestic regulatory autonomy, viewing environmental and labor standards as potential protectionist measures. While influential in policy debates, their more extreme positions are not fully accommodated by this 'embedded liberalism' reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, capital_supremacy_advocates, excluded,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate market access for goods and services across national borders while simultaneously preserving the ability of sovereign states to enact and enforce legitimate domestic environmental, labor, and health regulations.
% TRANSFER_FUNCTION: Transfers market access opportunities to exporting industries and regulatory stability to domestic agencies, while transferring litigation costs and competitive pressure to domestic industries and advocacy groups.
% ABSENT_VOICES: Advocates for a 'capital supremacy' reading, who would argue for the complete subordination of domestic regulation to trade liberalization, are structurally marginalized in this 'embedded liberalism' interpretation. Their arguments are heard in other forums but do not define the operating logic of this specific constraint.
% DISAPPEARANCE_RATIONALE: If this reading of the NAFTA jurisdictional boundary vanished, the balance between trade and domestic policy would collapse. Either trade barriers would rise significantly as states reasserted unconstrained sovereignty, or domestic regulations would be systematically challenged and dismantled in favor of pure market access, leading to a fundamental reorganization of international economic relations and domestic regulatory environments.
% FOUNDING_PROBLEM: The challenge of integrating national economies through trade agreements without completely eroding national sovereignty and the ability to pursue domestic social and environmental objectives.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and political economists, independent of specific industry or government interests, corroborate that the tension between trade liberalization and domestic policy space remains a live and complex problem in international relations, requiring ongoing negotiation and interpretation.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs of compliance, potential litigation, and the chilling effect on domestic regulation, but it is balanced by the preservation of policy space. Suppression (0.35) is also moderate, as domestic regulations can be defended, but the burden of proof and the threat of trade sanctions exert pressure. The theater ratio (0.20) is low, indicating that the stated coordination function (balancing trade with domestic policy) is largely genuine, though some performative defense of domestic policy may occur to avoid challenges. The metrics reflect a system that genuinely attempts to balance competing objectives, characteristic of a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   Exporting industries and domestic regulatory agencies experience this as a beneficial framework, enabling trade while protecting sovereignty. Domestic industries facing imports and environmental advocacy groups experience it as a cost, as it introduces new competitive pressures and legal challenges to their standards. Trade dispute panels, as agenda setters, navigate these competing claims, shaping the effective balance.
 *
 * DIRECTIONALITY LOGIC:
 *   Exporting industries benefit from expanded market access (low d). Domestic regulatory agencies benefit from the explicit recognition of their policy space (low d). Domestic industries facing imports bear costs from increased competition (high d). Environmental advocacy groups bear costs from the need to defend standards against trade challenges (high d). The trade dispute panels, while administering the system, are structurally positioned to balance these interests, making their directionality closer to symmetric, though their rulings can shift the balance.
 *
 * MANDATROPHY ANALYSIS:
 *   This 'embedded liberalism' reading prevents mislabeling the constraint as a pure Snare by acknowledging the genuine coordination function of balancing market access with domestic policy autonomy. It also prevents mislabeling it as a pure Rope by recognizing the inherent extraction through litigation costs and the pressure on domestic regulatory space. The classification as Tangled Rope accurately captures this hybrid nature, where coordination and extraction are intertwined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint truly an ''embedded liberalism'' reading of NAFTA''s jurisdictional boundary, or is it a ''capital supremacy'' reading in disguise?',
    'Analysis of dispute settlement panel rulings over time: if rulings consistently prioritize market access over legitimate domestic policy objectives, reclassify towards capital_supremacy_reading.',
    'If reclassified as capital_supremacy_reading, extractiveness and suppression would be higher, and the constraint would likely shift towards a Snare, as the coordination function (balancing trade with domestic policy) would be revealed as cover for pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between embedded liberalism and capital supremacy readings of NAFTA''s jurisdictional boundary.').

omega_variable(
    legitimate_objective_definition,
    'What constitutes a ''legitimate domestic policy objective'' in practice, and how consistently is this standard applied by dispute settlement bodies?',
    'Empirical study of all cases where domestic regulations were challenged under NAFTA''s investment or goods chapters, categorizing outcomes based on the stated objective and the panel''s deference to domestic policy.',
    'A narrow or inconsistent definition of ''legitimate objective'' would increase the effective suppression on domestic regulatory agencies, pushing the constraint towards a Snare by eroding the policy space it claims to protect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objective_definition, empirical, 'The practical scope and consistency of ''legitimate domestic policy objective'' interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 15, 0.33).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the NAFTA jurisdictional boundary kernel. Each reading has a different structural interpretation of the balance between trade obligations and domestic policy space, leading to different extractiveness and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
