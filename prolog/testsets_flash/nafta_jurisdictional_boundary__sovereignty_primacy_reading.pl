% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primacy' reading of the NAFTA
 *   jurisdictional boundary, where the trade agreement functions primarily as
 *   a coordination mechanism subordinate to the full regulatory authority of
 *   sovereign domestic law. Under this reading, states retain the right to
 *   set and enforce their own labor, environmental, and health standards
 *   without these being treated as non-tariff barriers or subject to
 *   overriding treaty obligations. Treaty obligations enter the
 *   compliance-cost set but do not act as overriding constraints on domestic
 *   regulatory agencies. Extraction is limited to the voluntary compliance
 *   costs associated with participating in the coordinated trade framework.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary beneficiary (institutional/arbitrage) — retains regulatory autonomy
 *   - domestic_regulatory_agencies: Primary beneficiary (institutional/mobile) — exercises full jurisdictional authority
 *   - multinational_corporations: Payer (powerful/mobile) — bears compliance costs with diverse national regulations
 *   - trade_dispute_panels: Agenda setter (institutional/analytical) — adjudicates disputes, but with deference to domestic law
 *   - labor_unions: Beneficiary (organized/constrained) — benefits from protected domestic labor standards
 *   - environmental_advocates: Beneficiary (organized/constrained) — benefits from protected domestic environmental standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.25).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '4634f87e-4351-4373-bc73-7a7fc6dc20f4').
narrative_ontology:cs_kernel_codification('4634f87e-4351-4373-bc73-7a7fc6dc20f4', fixed_text).
narrative_ontology:cs_authority_grounding('4634f87e-4351-4373-bc73-7a7fc6dc20f4', lineage).
narrative_ontology:cs_interpretation_layer_present('4634f87e-4351-4373-bc73-7a7fc6dc20f4').
narrative_ontology:cs_reading_relation('4634f87e-4351-4373-bc73-7a7fc6dc20f4', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('4634f87e-4351-4373-bc73-7a7fc6dc20f4', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('4634f87e-4351-4373-bc73-7a7fc6dc20f4', foundational, domestic_regulatory_autonomy_is_supreme).
narrative_ontology:cs_axiom_status(domestic_regulatory_autonomy_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('4634f87e-4351-4373-bc73-7a7fc6dc20f4', domestic_regulatory_autonomy_is_supreme, deontological).
narrative_ontology:cs_axiom('4634f87e-4351-4373-bc73-7a7fc6dc20f4', foundational, trade_agreements_are_subordinate_to_sovereign_law).
narrative_ontology:cs_axiom_status(trade_agreements_are_subordinate_to_sovereign_law, holdable).
narrative_ontology:cs_axiom_grounding('4634f87e-4351-4373-bc73-7a7fc6dc20f4', trade_agreements_are_subordinate_to_sovereign_law, conventional).
narrative_ontology:cs_reference_frame('4634f87e-4351-4373-bc73-7a7fc6dc20f4', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('4634f87e-4351-4373-bc73-7a7fc6dc20f4', contemporary_globalization_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4634f87e-4351-4373-bc73-7a7fc6dc20f4', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_unions).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_advocates).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, principle_of_national_sovereignty).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulatory_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full authority to set and enforce domestic labor, environmental, and health standards, treating trade obligations as compliance costs rather than overriding constraints. They benefit from market access without sacrificing regulatory autonomy.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states, beneficiary,
    institutional, generational, arbitrage, national).

% Exercise full jurisdictional authority over their respective policy areas, implementing national standards without direct challenge from trade agreements. They benefit from a clear mandate and stable regulatory environment.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, mobile, national).

% Operate across diverse national regulatory regimes, bearing the costs of complying with varied labor, environmental, and health standards. While they seek market access, this reading means they cannot easily leverage trade agreements to lower domestic standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations, payer,
    powerful, biographical, mobile, global).

% Adjudicate disputes arising from the trade agreement, but with a clear mandate to defer to sovereign domestic law in areas of legitimate public policy. Their role is to ensure fair trade within the bounds of national regulatory autonomy, not to override it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels, agenda_setter,
    institutional, biographical, analytical, regional).

% Benefit from the protection of domestic labor standards, which are not undermined by trade agreements. This reading supports their advocacy for strong national worker protections.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_unions, beneficiary,
    organized, generational, constrained, national).

% Benefit from the ability of states to maintain and strengthen domestic environmental regulations without fear of trade-related challenges. This reading supports their efforts to promote national ecological protection.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_advocates, beneficiary,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__sovereignty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate international trade and market access among member states while explicitly preserving the sovereign right of each state to regulate domestic labor, environmental, and health standards.
% TRANSFER_FUNCTION: The constraint transfers the 'cost of regulatory harmonization' from sovereign states (who retain autonomy) to multinational corporations (who bear compliance costs with diverse national laws). It also transfers 'legitimacy' to domestic regulatory frameworks.
% ABSENT_VOICES: Advocates for 'deep integration' or 'regulatory harmonization' (often associated with capital interests) would object, arguing that diverse national standards create non-tariff barriers and impede efficient capital flow. Their voices are present in other readings of this kernel, but not prioritized here.
% DISAPPEARANCE_RATIONALE: If this reading of the jurisdictional boundary vanished, trade agreements would likely be reinterpreted to grant greater power to international panels to challenge domestic regulations, leading to a significant shift in national policy autonomy and potentially a 'race to the bottom' in standards. The balance of power between national governments and international capital would fundamentally rearrange.
% FOUNDING_PROBLEM: The problem was how to expand trade and economic integration without eroding national sovereignty and the ability of democratic states to protect their citizens through domestic regulation.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and political scientists (outside the direct beneficiaries of this reading) corroborate that balancing trade liberalization with national sovereignty remains a live and contested problem in international law and political economy. Public opinion in many states also consistently supports national regulatory autonomy over unfettered trade.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the primary function is coordination of trade rules, not extraction of regulatory authority. Suppression is low (0.15) as states are not coerced into altering their domestic laws; compliance is voluntary for market access. Theater ratio is low (0.1) as the stated function of respecting sovereignty is largely upheld. The metrics reflect a system where trade facilitates market access while genuinely respecting national regulatory space.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states and domestic regulatory agencies, this constraint is a Rope, enabling trade while preserving essential autonomy. From the perspective of multinational corporations seeking regulatory harmonization, it might appear as a minor Snare due to varied compliance costs, but this reading explicitly prioritizes national regulatory diversity.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and domestic regulatory agencies are clear beneficiaries (d near 0.0) as their authority is preserved. Multinational corporations are payers (d near 1.0) as they bear the costs of complying with diverse national regulations. Labor unions and environmental advocates are beneficiaries (d near 0.0) as their policy goals are protected. Trade dispute panels act as agenda setters, mediating disputes within the framework of national sovereignty.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the mislabeling of legitimate coordination (trade facilitation) as extraction by clearly delineating the boundaries of treaty authority. It ensures that the 'mandate' of trade liberalization does not atrophy into a mechanism for overriding democratic regulatory processes, thus avoiding a Snare classification by maintaining the primacy of domestic law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of sovereignty primacy, or is it merely a rhetorical cover for deeper capital supremacy?',
    'Analysis of dispute settlement outcomes: if domestic regulatory decisions are consistently upheld against trade challenges, it supports sovereignty primacy. If not, it suggests a different reading is operative.',
    'If it is a genuine sovereignty primacy reading, the constraint functions as a Rope, coordinating trade with minimal extraction. If it''s a cover for capital supremacy, the effective extraction is much higher, reclassifying it as a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''sovereignty_primacy_reading'' of the ''nafta_jurisdictional_boundary'' kernel. Sibling readings (''capital_supremacy_reading'', ''embedded_liberalism_reading'') would shift the balance of power and extraction significantly.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural or internalized?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 10, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 10, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nafta_jurisdictional_boundary' kernel. This 'sovereignty_primacy_reading' emphasizes the subordination of trade law to domestic regulatory authority, contrasting with the 'capital_supremacy_reading' (which prioritizes capital mobility and regulatory harmonization) and the 'embedded_liberalism_reading' (which seeks a balance between market access and legitimate domestic policy space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
