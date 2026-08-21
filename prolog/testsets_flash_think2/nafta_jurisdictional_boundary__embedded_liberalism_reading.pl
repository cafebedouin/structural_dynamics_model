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
 *   jurisdictional boundary, where the trade agreement text is interpreted as
 *   a framework that balances market access with legitimate domestic policy
 *   space. Environmental and labor standards are considered compatible with
 *   trade obligations as long as they are non-discriminatory. This reading
 *   acknowledges the tension but seeks a workable compromise, leading to
 *   moderate extraction primarily through litigation costs and policy
 *   chilling effects, rather than outright suppression of domestic
 *   regulation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.58).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.45).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "NAFTA Jurisdictional Boundary (Embedded Liberalism Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__embedded_liberalism_reading, '25b29ae6-4c42-4270-bc2b-e4dc8070a279').
narrative_ontology:cs_kernel_codification('25b29ae6-4c42-4270-bc2b-e4dc8070a279', fixed_text).
narrative_ontology:cs_authority_grounding('25b29ae6-4c42-4270-bc2b-e4dc8070a279', lineage).
narrative_ontology:cs_interpretation_layer_present('25b29ae6-4c42-4270-bc2b-e4dc8070a279').
narrative_ontology:cs_reading_relation('25b29ae6-4c42-4270-bc2b-e4dc8070a279', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('25b29ae6-4c42-4270-bc2b-e4dc8070a279', nafta_jurisdictional_boundary__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('25b29ae6-4c42-4270-bc2b-e4dc8070a279', foundational, trade_liberalization_with_social_safeguards).
narrative_ontology:cs_axiom_status(trade_liberalization_with_social_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('25b29ae6-4c42-4270-bc2b-e4dc8070a279', trade_liberalization_with_social_safeguards, deontological).
narrative_ontology:cs_axiom('25b29ae6-4c42-4270-bc2b-e4dc8070a279', foundational, non_discriminatory_regulation_is_legitimate).
narrative_ontology:cs_axiom_status(non_discriminatory_regulation_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('25b29ae6-4c42-4270-bc2b-e4dc8070a279', non_discriminatory_regulation_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('25b29ae6-4c42-4270-bc2b-e4dc8070a279', post_bretton_woods_compromise).
narrative_ontology:cs_drift_state('25b29ae6-4c42-4270-bc2b-e4dc8070a279', post_globalization_backlash_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('25b29ae6-4c42-4270-bc2b-e4dc8070a279', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_industries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, import_competing_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from predictable market access and reduced trade barriers, allowing them to expand operations and sales across borders. They actively lobby for interpretations that favor trade liberalization.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_industries, beneficiary,
    powerful, biographical, mobile, global).

% Operate within a framework where their environmental, labor, or health regulations can be challenged as non-tariff barriers. They bear the costs of defending policies in dispute settlement and may face pressure to harmonize standards or dilute regulations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% Benefit from a wider variety of goods and potentially lower prices due to increased competition and reduced import costs. Their benefits are diffuse and often indirect.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, consumers, beneficiary,
    moderate, immediate, mobile, national).

% Face increased competition from foreign goods due to reduced tariffs and non-tariff barriers. They may experience job losses or reduced market share, leading to calls for protectionist measures.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, import_competing_industries, payer,
    organized, biographical, constrained, national).

% Interpret the trade agreement text, adjudicating disputes between member states regarding market access and regulatory compatibility. Their rulings shape the practical application of the jurisdictional boundary.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_panels, agenda_setter,
    institutional, biographical, analytical, regional).

% Often find their concerns about environmental standards subordinated to trade liberalization goals. While the embedded liberalism reading acknowledges legitimate objectives, the burden of proof and litigation costs often make effective environmental protection difficult within the trade framework.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate cross-border trade and investment by establishing a predictable framework for market access, while simultaneously acknowledging and attempting to preserve legitimate domestic policy space for social and environmental regulation.
% TRANSFER_FUNCTION: Transfers market access and economic efficiency gains to export-oriented industries and consumers, while transferring litigation costs, policy constraints, and competitive pressure to domestic regulatory agencies and import-competing industries.
% ABSENT_VOICES: Those advocating for a pure capital supremacy model (where trade rules always override domestic policy) or a pure sovereignty primacy model (where domestic policy is entirely immune from trade challenge) are structurally excluded from the 'balanced' framing. Also, civil society groups whose policy goals are consistently subordinated to trade concerns.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the delicate balance between trade and domestic policy would collapse. Trade flows would become less predictable, potentially leading to increased protectionism or, conversely, a race to the bottom in regulatory standards without any international framework for balancing. The entire North American economic integration model would need to be re-negotiated or would fragment.
% FOUNDING_PROBLEM: How to liberalize trade and integrate economies across North America without completely eroding national sovereignty or the ability of states to pursue legitimate domestic social, environmental, and labor policy objectives.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, political economists, and former trade negotiators from various countries (not solely those benefiting from the agreement) consistently identify this tension as a core, ongoing challenge in international economic law.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.58) is moderate because while market access is prioritized, the 'legitimate objectives' clause provides some defensive space for domestic policy. Suppression (0.45) is also moderate, reflecting the constraint on domestic policy autonomy without outright elimination. The theater ratio (0.20) is low, as the effort to balance these objectives is generally genuine, though often contentious. Resistance (0.60) is high, reflecting ongoing challenges from both trade advocates (seeking more liberalization) and domestic policy advocates (seeking more autonomy). Accessibility collapse (0.50) is moderate, as alternatives (pure protectionism or pure free trade) are constrained but not entirely foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of export-oriented industries, this is a beneficial rope, providing stable market access. From the perspective of domestic regulatory agencies, it's a tangled rope, forcing them to defend legitimate policies against trade challenges. The engine's per-seat classification will reflect these divergences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Export-oriented industries and consumers are beneficiaries, gaining from market access and lower prices. Domestic regulatory agencies and import-competing industries are payers, bearing the costs of policy defense and increased competition. Trade dispute panels act as agenda-setters, interpreting and enforcing the balance. Environmental advocates are often excluded, as their concerns are filtered through the 'legitimate objectives' lens, which can be difficult to navigate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''nafta_jurisdictional_boundary'' kernel, or merely a nuanced interpretation within a broader ''capital_supremacy'' framework?',
    'Analysis of dispute settlement outcomes: if rulings consistently uphold ''legitimate objectives'' even when they constrain trade, it supports a distinct ''embedded liberalism'' reading. If rulings consistently favor trade liberalization, it suggests the ''embedded liberalism'' framing is largely rhetorical cover for capital supremacy.',
    'If confirmed as a distinct reading, it validates the conceptual space for balancing trade and domestic policy. If reclassified as a subset of ''capital_supremacy'', the effective extractiveness and suppression would be higher, and the claimed coordination function would be largely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing ''embedded liberalism'' as a distinct reading versus a sub-interpretation.').

omega_variable(
    legitimate_objectives_scope,
    'What is the actual scope and effectiveness of the ''legitimate objectives'' clause in protecting domestic environmental and labor standards from trade challenges?',
    'Empirical study of all dispute settlement cases involving environmental/labor standards: quantify the success rate of domestic regulations, the costs of defense, and the frequency of policy chilling effects.',
    'If the clause is found to be consistently weak or costly to invoke, the effective suppression and extractiveness for domestic regulatory agencies would be higher, pushing the constraint closer to a Snare. If robust, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_scope, empirical, 'Empirical effectiveness of ''legitimate objectives'' clause.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal/economic barriers) or internalized (domestic agencies preemptively weakening regulations to avoid trade challenges)?',
    'Comparative analysis of regulatory changes in trade-exposed vs. non-trade-exposed sectors/jurisdictions, combined with interviews with regulatory officials regarding perceived trade risks.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as agencies carry the suppression with them even without direct challenge. This would push the constraint closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in domestic policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 1994, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 1994, 0.45).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2006, 0.55).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2012, 0.57).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2006, 0.43).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nafta_jurisdictional_boundary' kernel, alongside 'capital_supremacy_reading' and 'sovereignty_primacy_reading'. Each reading represents a distinct interpretation of the trade agreement's balance between market access and domestic policy space, leading to different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
