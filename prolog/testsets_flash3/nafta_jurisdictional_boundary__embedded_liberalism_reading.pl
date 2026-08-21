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
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'embedded liberalism' reading of the NAFTA
 *   jurisdictional boundary, where trade liberalization is balanced with the
 *   preservation of domestic policy space for social and environmental
 *   objectives. It acknowledges that while trade agreements facilitate market
 *   access, they must also accommodate legitimate non-discriminatory domestic
 *   regulations. This reading is distinct from those prioritizing capital
 *   supremacy or absolute national sovereignty, focusing instead on a
 *   negotiated equilibrium. The constraint is classified as a Tangled Rope
 *   because it genuinely coordinates market access but involves asymmetric
 *   extraction through the costs of defending domestic regulations in trade
 *   disputes.
 *
 * KEY AGENTS:
 *   - multinational_corporations: Primary beneficiary (institutional/arbitrage)
 *   - exporting_industries: Secondary beneficiary (organized/mobile)
 *   - domestic_regulatory_agencies: Primary payer (institutional/constrained)
 *   - environmental_advocacy_groups: Payer (moderate/constrained)
 *   - labor_unions: Payer (organized/constrained)
 *   - trade_dispute_panels: Agenda setter (institutional/analytical)
 *   - domestic_consumers: Beneficiary (powerless/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.3).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '5713deae-0ed6-4ef7-8d4d-7665e61842e4').
narrative_ontology:cs_kernel_codification('5713deae-0ed6-4ef7-8d4d-7665e61842e4', fixed_text).
narrative_ontology:cs_authority_grounding('5713deae-0ed6-4ef7-8d4d-7665e61842e4', lineage).
narrative_ontology:cs_interpretation_layer_present('5713deae-0ed6-4ef7-8d4d-7665e61842e4').
narrative_ontology:cs_reading_relation('5713deae-0ed6-4ef7-8d4d-7665e61842e4', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5713deae-0ed6-4ef7-8d4d-7665e61842e4', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('5713deae-0ed6-4ef7-8d4d-7665e61842e4', foundational, domestic_policy_space_is_legitimate).
narrative_ontology:cs_axiom_status(domestic_policy_space_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('5713deae-0ed6-4ef7-8d4d-7665e61842e4', domestic_policy_space_is_legitimate, deontological).
narrative_ontology:cs_axiom('5713deae-0ed6-4ef7-8d4d-7665e61842e4', foundational, non_discriminatory_regulation_is_compatible_with_trade).
narrative_ontology:cs_axiom_status(non_discriminatory_regulation_is_compatible_with_trade, holdable).
narrative_ontology:cs_axiom_grounding('5713deae-0ed6-4ef7-8d4d-7665e61842e4', non_discriminatory_regulation_is_compatible_with_trade, conventional).
narrative_ontology:cs_reference_frame('5713deae-0ed6-4ef7-8d4d-7665e61842e4', post_bretton_woods_compromise).
narrative_ontology:cs_drift_state('5713deae-0ed6-4ef7-8d4d-7665e61842e4', contemporary_globalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5713deae-0ed6-4ef7-8d4d-7665e61842e4', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_unions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced trade barriers and access to new markets, while navigating a framework that generally respects non-discriminatory domestic regulations. They leverage the agreement for market access but face some limits on challenging legitimate domestic policy.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Gain from expanded market opportunities and a more predictable trade environment. They support the framework as long as it facilitates exports without imposing overly burdensome compliance costs from domestic regulations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, exporting_industries, beneficiary,
    organized, biographical, mobile, national).

% Operate within a framework that allows them to maintain environmental, labor, and health standards, provided these are non-discriminatory and pursue legitimate objectives. They bear the cost of defending these regulations against trade challenges, often through costly litigation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Advocate for strong domestic environmental standards, which are generally permitted under this reading. They face the ongoing challenge of ensuring regulations are not deemed 'disguised protectionism' and bear the burden of monitoring and defending policy space.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_advocacy_groups, payer,
    moderate, generational, constrained, national).

% Work to uphold domestic labor standards, which are recognized as legitimate policy objectives. They expend resources to ensure these standards are not undermined by trade liberalization and to counter arguments that they constitute non-tariff barriers.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, labor_unions, payer,
    organized, generational, constrained, national).

% Interpret the trade agreement text, balancing market access with domestic policy space. Their rulings shape the practical application of the jurisdictional boundary, often incurring costs on domestic regulators when challenges are upheld.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, immediate, analytical, regional).

% Benefit from a wider variety of goods at potentially lower prices due to increased market access. They indirectly bear the costs of any regulatory rollbacks or litigation expenses passed on by their governments or industries.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_consumers, beneficiary,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates market access rules among member states while preserving space for domestic regulatory autonomy on non-discriminatory environmental, labor, and health standards.
% TRANSFER_FUNCTION: Facilitates the transfer of goods and services across borders by reducing tariffs and non-tariff barriers, while allowing states to retain regulatory authority over domestic policy, subject to trade dispute challenges.
% ABSENT_VOICES: Small domestic businesses unable to compete with multinational corporations, and communities directly impacted by environmental degradation or labor exploitation that might occur at the margins of 'legitimate' policy space, often lack direct representation in trade negotiations or dispute resolution.
% DISAPPEARANCE_RATIONALE: If this reading of the NAFTA jurisdictional boundary vanished, the balance between trade and domestic policy would collapse. Either trade would become highly protectionist, or domestic regulations would be systematically challenged and dismantled, leading to a significant reorganization of economic and regulatory landscapes.
% FOUNDING_PROBLEM: To create a framework for free trade across North America that would stimulate economic growth, while acknowledging the sovereign right of nations to protect their environment, labor, and public health through non-discriminatory domestic regulations.
% FOUNDING_PROBLEM_CORROBORATION: Academics in international law and political economy, as well as some government officials and NGOs, corroborate that the tension between market access and domestic policy space remains a live and ongoing challenge, requiring continuous interpretation and negotiation.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.45) is moderate, reflecting the costs imposed on domestic regulatory agencies and advocacy groups to defend their policy space against trade challenges, even when those challenges are ultimately unsuccessful. Suppression (0.30) is relatively low, as this reading explicitly allows for domestic policy space, but the threat of costly litigation acts as a suppressive force on ambitious regulatory initiatives. Theater ratio (0.15) is low, as the coordination function of balancing trade and domestic policy is genuinely active, though the balance is constantly contested. The metrics reflect a system that is functional but not without significant friction and costs for certain parties.
 *
 * PERSPECTIVAL GAP:
 *   Multinational corporations and exporting industries perceive this as a beneficial framework that enables market access with reasonable regulatory predictability. Domestic regulatory agencies, environmental groups, and labor unions, while acknowledging the policy space, experience it as a constant battleground where they must expend significant resources to defend their legitimate objectives against trade challenges. Trade dispute panels act as agenda setters, shaping the interpretation of the boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations and exporting industries are beneficiaries, as the agreement facilitates their core business objectives (d near 0.0). Domestic regulatory agencies, environmental groups, and labor unions are payers, bearing the costs of defending domestic policy (d near 1.0). Domestic consumers are diffuse beneficiaries of increased choice and lower prices, but also indirectly bear some costs. Trade dispute panels, as interpreters and enforcers, act as agenda setters, influencing the direction of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by explicitly recognizing the legitimate coordination function of balancing market access with domestic policy space. It avoids the pitfall of assuming all trade-related costs are 'extraction' by acknowledging the genuine collective action problem of international trade governance. However, the ongoing costs of litigation for domestic policy defenders highlight the extractive component within this coordination, preventing it from being classified as a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_objective_ambiguity,
    'What constitutes a ''legitimate objective'' for domestic regulation, and how consistently is this interpreted by trade dispute panels?',
    'Analysis of a large corpus of trade dispute rulings, identifying patterns in how ''legitimate objective'' is defined and applied across different sectors and countries.',
    'If interpretation is consistently narrow, it increases effective extraction on domestic regulators; if broad and deferential, it reduces extraction. This would shift the extractiveness metric and potentially the classification towards Snare or Rope, respectively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objective_ambiguity, empirical, 'Ambiguity in the definition of ''legitimate objective'' for domestic policy.').

omega_variable(
    litigation_cost_asymmetry,
    'Are the costs of defending domestic regulations in trade disputes disproportionately borne by states with fewer resources, effectively chilling legitimate regulatory action?',
    'Comparative study of trade dispute outcomes and associated legal costs for states of varying economic power, correlated with the ambition of their domestic regulatory agenda.',
    'If costs are highly asymmetric and suppress regulatory action, the effective suppression and extractiveness are higher than measured, pushing the classification closer to Snare. If costs are manageable, the current Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(litigation_cost_asymmetry, empirical, 'Asymmetry in litigation costs for defending domestic regulations.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''embedded liberalism'' reading of the NAFTA jurisdictional boundary genuinely stable, or is it constantly under pressure from ''capital supremacy'' or ''sovereignty primacy'' readings?',
    'Longitudinal analysis of judicial decisions, policy debates, and academic discourse to track the prevalence and influence of each reading over time, and how they interact in specific cases.',
    'If this reading is consistently undermined by others, its effective stability and coordination function are lower, increasing its perceived extractiveness and suppression. If it holds its ground, the current classification is robust. This omega documents the core kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The stability and contestation of the ''embedded liberalism'' reading against alternative interpretations of the NAFTA jurisdictional boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 25, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 25, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the NAFTA jurisdictional boundary kernel. Each reading represents a different structural claim about the balance between trade and domestic policy, with different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
