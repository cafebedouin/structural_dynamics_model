% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Capital Supremacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the 'capital supremacy' reading of
 *   NAFTA's jurisdictional boundary, where the trade agreement text is
 *   interpreted as supreme law overriding domestic regulatory standards.
 *   Capital mobility and regulatory harmonization are treated as mandatory
 *   treaty obligations, effectively subordinating national policy space to
 *   the demands of transnational capital. This reading is characterized by
 *   high extraction from domestic regulatory autonomy and civil society, and
 *   high suppression of alternative policy paths.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.85).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.9).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, snare).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary: Capital Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '480c8952-2d12-4f19-8519-e8bebdaaf4de').
narrative_ontology:cs_kernel_codification('480c8952-2d12-4f19-8519-e8bebdaaf4de', fixed_text).
narrative_ontology:cs_authority_grounding('480c8952-2d12-4f19-8519-e8bebdaaf4de', extraction).
narrative_ontology:cs_interpretation_layer_present('480c8952-2d12-4f19-8519-e8bebdaaf4de').
narrative_ontology:cs_reading_relation('480c8952-2d12-4f19-8519-e8bebdaaf4de', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('480c8952-2d12-4f19-8519-e8bebdaaf4de', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('480c8952-2d12-4f19-8519-e8bebdaaf4de', foundational, capital_mobility_as_supreme_right).
narrative_ontology:cs_axiom_status(capital_mobility_as_supreme_right, holdable).
narrative_ontology:cs_axiom_grounding('480c8952-2d12-4f19-8519-e8bebdaaf4de', capital_mobility_as_supreme_right, deontological).
narrative_ontology:cs_axiom('480c8952-2d12-4f19-8519-e8bebdaaf4de', secondary, regulatory_harmonization_as_efficiency).
narrative_ontology:cs_axiom_status(regulatory_harmonization_as_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('480c8952-2d12-4f19-8519-e8bebdaaf4de', regulatory_harmonization_as_efficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('480c8952-2d12-4f19-8519-e8bebdaaf4de', unfettered_capital_flow_paradigm).
narrative_ontology:cs_drift_state('480c8952-2d12-4f19-8519-e8bebdaaf4de', contemporary_trade_disputes_and_renegotiations, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('480c8952-2d12-4f19-8519-e8bebdaaf4de', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, transnational_capital).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, export_oriented_corporations).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_unions).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocates).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, national_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from reduced regulatory friction and increased policy certainty across borders, leading to higher profits and greater investment flexibility. Actively lobbies for interpretations that prioritize capital mobility.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, transnational_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Gains competitive advantage from lower production costs due to relaxed labor and environmental standards in host countries, and from easier market access. Supports the treaty's enforcement mechanisms.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, export_oriented_corporations, beneficiary,
    powerful, biographical, mobile, global).

% Experiences a loss of jurisdictional authority and faces 'regulatory chill' where new or existing standards are challenged as trade barriers, leading to a weakening of domestic protections.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Faces downward pressure on wages and working conditions due to increased capital mobility and the threat of production relocation to lower-standard jurisdictions. Actively resists the constraint's effects.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_unions, payer,
    organized, biographical, constrained, national).

% Struggles to implement and enforce robust environmental protections, as these are often challenged as non-tariff barriers to trade, leading to environmental degradation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocates, payer,
    organized, biographical, constrained, national).

% Bears the direct social and environmental costs of industrial relocation, weakened regulatory oversight, and the erosion of local democratic control over economic development.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities, payer,
    powerless, immediate, trapped, local).

% Adjudicates disputes between investors and states, often interpreting treaty provisions in ways that prioritize investor rights and capital mobility over domestic regulatory autonomy. Their rulings reinforce the constraint.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Are bound by treaty obligations, limiting their ability to enact or maintain domestic regulatory standards that might impede trade or investment. They are also the enforcers of the treaty, but bear political costs from their citizens.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, national_governments, payer).

% Argue that trade agreements should be subordinate to national sovereignty and democratic control over domestic policy. Their perspective is systematically marginalized in the interpretation and enforcement of the treaty.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, transnational_capital).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for cross-border trade and investment by reducing regulatory divergence and providing dispute resolution mechanisms, aiming to foster economic integration and efficiency.
% TRANSFER_FUNCTION: Transfers significant regulatory authority and policy space from national and sub-national governments to international trade tribunals and transnational capital, enabling capital to operate with fewer constraints and externalizing social and environmental costs.
% ABSENT_VOICES: Domestic citizens, local communities, and non-trade-oriented regulatory bodies (e.g., public health, environmental protection) whose interests are subordinated to trade and investment liberalization. Sovereignty advocates are also structurally excluded from the interpretive frame that prioritizes capital.
% DISAPPEARANCE_RATIONALE: If this reading of NAFTA vanished, domestic regulatory autonomy would reassert itself, capital flows would face new friction, and the balance of power between trade and non-trade policy objectives would shift dramatically, reorganizing global economic governance and potentially leading to a re-localization of production.
% FOUNDING_PROBLEM: Fragmented national regulations and policy uncertainty created barriers to cross-border trade and investment, hindering economic growth and efficiency across North America.
% FOUNDING_PROBLEM_CORROBORATION: Transnational corporations and pro-trade economists attest the problem is still live, citing ongoing needs for regulatory harmonization. Labor, environmental, and public health groups, along with critical legal scholars, argue the founding problem is substantially solved or was a pretext for rent-seeking, citing evidence of regulatory chill and social dumping. Independent economic analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the interpretation systematically prioritizes investor rights and capital mobility, leading to a significant transfer of regulatory power and wealth to transnational corporations. Suppression is very high (0.90) due to the binding nature of treaty obligations, the threat of investor-state dispute settlement (ISDS), and the 'regulatory chill' effect, which actively discourages states from enacting policies that might be challenged. Theater ratio is low (0.15) as the enforcement mechanisms (ISDS tribunals, trade sanctions) are highly effective and genuinely alter state behavior, not merely performative. Resistance is high (0.70) reflecting ongoing protests and political challenges from labor, environmental, and social justice groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of transnational capital, this reading represents efficient coordination and necessary market liberalization. From the perspective of domestic regulatory agencies and civil society, it is a mechanism of enforced extraction that undermines democratic sovereignty and public welfare. The engine's classification will highlight this divergence, showing a Snare from the victim seats and a more Rope-like (though still extractive) structure from the beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Transnational capital and export-oriented corporations are clear beneficiaries, gaining from reduced regulatory costs and increased market access. Domestic regulatory agencies, labor unions, environmental advocates, local communities, and national governments are victims, bearing the costs of diminished policy space and social/environmental externalities. Trade tribunals act as agenda-setters, enforcing the treaty's interpretation. The high extractiveness and suppression are directly linked to the structural advantage conferred upon beneficiaries at the expense of victims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_chill_quantification,
    'To what extent does the threat of investor-state dispute settlement (ISDS) actually cause ''regulatory chill'' (i.e., states refraining from enacting legitimate public interest regulations)?',
    'Empirical studies comparing regulatory output in states with and without ISDS exposure, or detailed case studies of specific policy decisions influenced by ISDS threats.',
    'If regulatory chill is widespread and significant, it strengthens the ''suppression'' metric and the Snare classification. If it is rare or negligible, the suppression is less effective, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_quantification, empirical, 'Quantifying the impact of ISDS on domestic regulatory behavior.').

omega_variable(
    coordination_vs_extraction_of_harmonization,
    'Is ''regulatory harmonization'' primarily a genuine coordination function that reduces transaction costs for all, or a mechanism for a ''race to the bottom'' that extracts regulatory concessions from states?',
    'Analysis of specific harmonized standards: do they converge upwards (raising all standards) or downwards (to the lowest common denominator)?',
    'If harmonization consistently leads to a ''race to the bottom,'' it reinforces the high ''extractiveness'' and Snare classification. If it genuinely raises standards or creates symmetric benefits, it would suggest a stronger coordination function, potentially moving towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_of_harmonization, conceptual, 'Distinguishing genuine regulatory coordination from extractive harmonization.').

omega_variable(
    economic_growth_attribution,
    'To what extent can observed economic growth be directly attributed to the capital supremacy interpretation of NAFTA, as opposed to other economic factors?',
    'Counterfactual economic modeling and econometric analysis isolating the specific impact of treaty provisions related to capital mobility and regulatory override.',
    'If the growth benefits are minimal or non-existent, it undermines the primary justification for the constraint, further exposing its extractive nature. If benefits are substantial and widely distributed, it could lend more credence to a coordination narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_growth_attribution, empirical, 'Attributing economic growth to specific treaty interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2006, 0.14).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 1994, 0.65).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2006, 0.78).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2012, 0.82).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2018, 0.84).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 1994, 0.7).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2006, 0.84).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2012, 0.88).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2018, 0.89).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulations).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_laws).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, usmca_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, ceta_jurisdictional_boundary__capital_supremacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nafta_jurisdictional_boundary' kernel. Its high extractiveness and suppression contrast with the 'embedded_liberalism_reading' (more balanced) and 'sovereignty_primacy_reading' (subordinate to domestic law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
