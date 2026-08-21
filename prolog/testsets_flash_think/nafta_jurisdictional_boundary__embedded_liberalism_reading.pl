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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   This constraint story represents the 'embedded liberalism' reading of the
 *   NAFTA jurisdictional boundary, which interprets the trade agreement as a
 *   framework balancing market access with the preservation of legitimate
 *   domestic policy space for environmental and labor standards. This reading
 *   acknowledges the coordinating function of trade agreements but also
 *   recognizes the potential for extraction through litigation costs and
 *   pressure on domestic regulations. The metrics reflect a system that is
 *   actively enforced and contested, with moderate extraction and
 *   suppression, as domestic actors frequently defend their policy space.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.62).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'b66d7db0-bc04-4d88-a8f3-e17767d0880e').
narrative_ontology:cs_kernel_codification('b66d7db0-bc04-4d88-a8f3-e17767d0880e', fixed_text).
narrative_ontology:cs_authority_grounding('b66d7db0-bc04-4d88-a8f3-e17767d0880e', lineage).
narrative_ontology:cs_interpretation_layer_present('b66d7db0-bc04-4d88-a8f3-e17767d0880e').
narrative_ontology:cs_reading_relation('b66d7db0-bc04-4d88-a8f3-e17767d0880e', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b66d7db0-bc04-4d88-a8f3-e17767d0880e', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('b66d7db0-bc04-4d88-a8f3-e17767d0880e', foundational, domestic_policy_space_legitimate).
narrative_ontology:cs_axiom_status(domestic_policy_space_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('b66d7db0-bc04-4d88-a8f3-e17767d0880e', domestic_policy_space_legitimate, deontological).
narrative_ontology:cs_axiom('b66d7db0-bc04-4d88-a8f3-e17767d0880e', foundational, non_discriminatory_standards_compatible).
narrative_ontology:cs_axiom_status(non_discriminatory_standards_compatible, holdable).
narrative_ontology:cs_axiom_grounding('b66d7db0-bc04-4d88-a8f3-e17767d0880e', non_discriminatory_standards_compatible, conventional).
narrative_ontology:cs_reference_frame('b66d7db0-bc04-4d88-a8f3-e17767d0880e', post_bretton_woods_compromise).
narrative_ontology:cs_drift_state('b66d7db0-bc04-4d88-a8f3-e17767d0880e', contemporary_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b66d7db0-bc04-4d88-a8f3-e17767d0880e', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_industries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_import_competing_industries).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, civil_society_groups_env_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with implementing domestic environmental, labor, and health standards. Under this reading, they retain significant policy space to pursue legitimate objectives, but must ensure measures are non-discriminatory and not disguised protectionism. They defend domestic regulations against trade challenges.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced tariffs and non-tariff barriers, gaining access to foreign markets. They advocate for interpretations that prioritize market access and challenge domestic regulations perceived as trade barriers.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_industries, beneficiary,
    organized, biographical, mobile, global).

% Advocate for strong domestic environmental and labor standards. They bear the costs when these standards are challenged as trade barriers and are often excluded from the direct trade dispute settlement process, relying on domestic political action.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, civil_society_groups_env_labor, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, civil_society_groups_env_labor, excluded).

% Interpret the trade agreement text and adjudicate disputes between member states. Under this reading, they are expected to balance market access with deference to legitimate domestic policy objectives, but their rulings can still impose significant costs.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, biographical, analytical, regional).

% Benefit from a wider variety of goods and potentially lower prices due to increased competition and market access. They may indirectly bear costs if domestic standards are weakened, but this reading emphasizes the preservation of those standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumers, beneficiary,
    moderate, immediate, mobile, national).

% Face increased competition from foreign imports due to reduced trade barriers. They bear the costs of adjusting to new market conditions and may advocate for stronger domestic regulations that could be challenged under the trade agreement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_import_competing_industries, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, rules-based framework for cross-border trade and investment, reducing uncertainty and transaction costs for businesses, while attempting to coordinate the recognition of legitimate domestic regulatory autonomy.
% TRANSFER_FUNCTION: Transfers market access opportunities and investment flows to export-oriented industries and consumers. It transfers the burden of defending domestic regulations to national governments and the costs of increased competition to import-competing industries. Litigation costs are transferred to disputing parties.
% ABSENT_VOICES: Indigenous communities, small local businesses, and future generations (whose long-term environmental and social interests are often not directly represented in trade negotiations or dispute panels) would advocate for stronger protections for non-economic values.
% DISAPPEARANCE_RATIONALE: If the NAFTA framework (or its successor, USMCA) vanished overnight, trade and investment flows between North American countries would face significant disruption, higher tariffs, and increased non-tariff barriers. Supply chains would reorganize, and economic relationships would revert to less integrated, bilateral arrangements, causing substantial economic upheaval.
% FOUNDING_PROBLEM: To reduce trade barriers and facilitate investment across North America, while simultaneously addressing concerns about the potential erosion of domestic environmental, labor, and health standards.
% FOUNDING_PROBLEM_CORROBORATION: International trade organizations, academic experts in international law and economics, and various government agencies continue to corroborate the ongoing challenge of balancing market integration with the preservation of domestic policy space. The tension between these objectives remains a central feature of contemporary trade policy debates.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.58) is moderate, reflecting the costs of compliance and dispute settlement, but tempered by the successful defense of some domestic policy measures. Suppression (0.62) is also moderate, as the trade regime exerts pressure on domestic regulations, but does not completely collapse alternatives for policy-making. The theater ratio (0.20) is low, indicating that the system is genuinely functional and actively used for both market access and regulatory defense, rather than being purely performative. The claimed type is Tangled Rope because it clearly serves a coordination function (market access) but also involves asymmetric extraction (litigation costs, pressure on standards) and requires active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of export-oriented industries, the constraint is a beneficial Rope, facilitating trade. From the perspective of civil society groups, it can feel more like a Snare, as domestic standards are constantly under threat. This reading attempts to capture the 'Tangled Rope' nature, where both coordination and extraction are present and actively contested, leading to different experiences for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Export-oriented industries and consumers are beneficiaries due to market access and lower prices. Domestic regulatory agencies are beneficiaries in that they retain policy space, but also payers when they must defend regulations. Domestic import-competing industries and civil society groups are payers, bearing the costs of increased competition or challenged standards. Trade dispute panels act as agenda-setters, interpreting the balance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''embedded liberalism'' reading of the NAFTA jurisdictional boundary, or is it a strategic framing to mask a more extractive ''capital supremacy'' outcome?',
    'Longitudinal analysis of trade dispute panel rulings: if rulings consistently prioritize market access over legitimate domestic policy objectives, it would suggest a drift towards the ''capital supremacy'' reading.',
    'If it''s a strategic framing, the effective extractiveness and suppression would be higher, pushing the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between embedded liberalism and capital supremacy readings of NAFTA.').

omega_variable(
    legitimate_objectives_definition,
    'How is ''legitimate domestic policy objective'' defined and applied by trade dispute panels, and does this definition genuinely protect environmental/labor standards?',
    'Content analysis of dispute panel reports and legal scholarship on the interpretation of ''legitimate objectives'' clauses in trade agreements.',
    'A narrow or inconsistently applied definition would increase effective extraction and suppression on domestic policy, pushing the classification towards a Snare. A broad and consistently applied definition would reinforce the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_definition, empirical, 'Ambiguity in the interpretation of ''legitimate domestic policy objectives'' in trade law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(naft_tr_t2004, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(naft_tr_t2014, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.5).
narrative_ontology:measurement(naft_be_t2004, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(naft_be_t2014, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(naft_su_t2004, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2004, 0.6).
narrative_ontology:measurement(naft_su_t2014, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2014, 0.62).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'NAFTA jurisdictional boundary' kernel. This 'embedded liberalism' reading emphasizes balancing market access with domestic policy space, contrasting with the 'capital supremacy' reading (prioritizing capital mobility) and the 'sovereignty primacy' reading (prioritizing state regulatory authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
