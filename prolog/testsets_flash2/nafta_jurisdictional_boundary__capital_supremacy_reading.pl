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
 *   This constraint represents the 'capital supremacy' reading of the NAFTA
 *   (or similar) jurisdictional boundary, where the trade agreement's text is
 *   interpreted as supreme law overriding domestic regulatory standards, and
 *   capital mobility and regulatory harmonization are mandatory treaty
 *   obligations. This reading emphasizes the agreement's role in facilitating
 *   corporate investment and profit maximization by minimizing regulatory
 *   friction, often at the expense of national sovereignty and domestic
 *   social/environmental protections. The claimed type is 'snare' because the
 *   coordination story (efficient trade) is seen as cover for substantial,
 *   actively enforced extraction from domestic regulatory bodies and
 *   vulnerable populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.85).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.9).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, snare).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary: Capital Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '36e08257-1eb3-4637-8c89-d6a1346c7d75').
narrative_ontology:cs_kernel_codification('36e08257-1eb3-4637-8c89-d6a1346c7d75', fixed_text).
narrative_ontology:cs_authority_grounding('36e08257-1eb3-4637-8c89-d6a1346c7d75', extraction).
narrative_ontology:cs_interpretation_layer_present('36e08257-1eb3-4637-8c89-d6a1346c7d75').
narrative_ontology:cs_reading_relation('36e08257-1eb3-4637-8c89-d6a1346c7d75', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('36e08257-1eb3-4637-8c89-d6a1346c7d75', nafta_jurisdictional_boundary__sovereignty_primacy_reading, influences).
narrative_ontology:cs_axiom('36e08257-1eb3-4637-8c89-d6a1346c7d75', foundational, trade_liberalization_as_supreme_good).
narrative_ontology:cs_axiom_status(trade_liberalization_as_supreme_good, holdable).
narrative_ontology:cs_axiom_grounding('36e08257-1eb3-4637-8c89-d6a1346c7d75', trade_liberalization_as_supreme_good, instrumental).
narrative_ontology:cs_axiom('36e08257-1eb3-4637-8c89-d6a1346c7d75', foundational, capital_mobility_as_economic_imperative).
narrative_ontology:cs_axiom_status(capital_mobility_as_economic_imperative, holdable).
narrative_ontology:cs_axiom_grounding('36e08257-1eb3-4637-8c89-d6a1346c7d75', capital_mobility_as_economic_imperative, empirically_contingent).
narrative_ontology:cs_reference_frame('36e08257-1eb3-4637-8c89-d6a1346c7d75', unfettered_market_access_framework).
narrative_ontology:cs_drift_state('36e08257-1eb3-4637-8c89-d6a1346c7d75', contemporary_era_of_trade_skepticism, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('36e08257-1eb3-4637-8c89-d6a1346c7d75', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_capital_holders).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_unions).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereign_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced regulatory hurdles and the ability to challenge domestic standards as trade barriers. They leverage the agreement's provisions to optimize supply chains and investment across borders, often at the expense of local regulations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from the free movement of capital and the harmonization of financial regulations, which reduces transaction costs and increases investment opportunities. They are insulated from local regulatory risks by the treaty's supremacy.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_capital_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the costs of downward pressure on wages and working conditions as companies seek lower-cost production sites in signatory countries. Their ability to advocate for stronger domestic labor laws is constrained by the threat of capital flight.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_unions, payer,
    organized, biographical, constrained, national).

% Face challenges to domestic environmental protections, which are often deemed non-tariff trade barriers under the agreement. Their efforts to strengthen local environmental standards are suppressed by the treaty's overriding authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocacy_groups, payer,
    moderate, generational, constrained, national).

% Lose jurisdictional authority over areas covered by the trade agreement, as their domestic standards can be challenged and overridden by treaty obligations. Their mandate to protect public welfare is subordinated to trade liberalization.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Experience the direct impacts of environmental degradation or job losses when industries relocate or domestic standards are weakened. They have minimal recourse against the treaty's provisions, which operate at a higher legal plane.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Interpret and enforce the trade agreement's provisions, often prioritizing trade liberalization over domestic regulatory autonomy. Their rulings effectively set the boundaries of permissible domestic regulation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_tribunals, agenda_setter,
    institutional, generational, analytical, regional).

% Are bound by the treaty's obligations, which limit their ability to enact or maintain domestic laws that might impede trade or capital mobility. While they are signatories, their regulatory sovereignty is diminished.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereign_states, payer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereign_states, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common legal framework for trade and investment across signatory nations, reducing transaction costs and legal uncertainty for cross-border economic activity.
% TRANSFER_FUNCTION: Transfers regulatory authority from domestic legislatures and agencies to the international trade agreement, and economic benefits from domestic labor and environmental protections to multinational corporations and financial capital holders.
% ABSENT_VOICES: Indigenous communities, small and medium-sized enterprises (SMEs) without international operations, and future generations (who will inherit environmental consequences) are largely excluded from the negotiation and enforcement processes, and would advocate for stronger domestic protections and a rebalancing of trade priorities.
% DISAPPEARANCE_RATIONALE: If the trade agreement's supremacy over domestic law vanished, signatory states would immediately reassert full regulatory authority, domestic standards would likely strengthen, and multinational corporations would face increased compliance costs, leading to a significant reorganization of global supply chains and investment patterns.
% FOUNDING_PROBLEM: The problem of fragmented national markets, high tariffs, and non-tariff barriers impeding efficient cross-border trade and investment, leading to economic inefficiencies and reduced growth.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading (multinational corporations, financial capital holders) argue the problem of trade barriers remains live. Critics (labor unions, environmental groups, some national regulatory agencies) argue the original problem has been largely solved, and the agreement now primarily serves to entrench corporate power and suppress domestic regulatory autonomy; academic legal scholars and economists outside the benefiting parties corroborate the shift in function.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is very high (0.85) because the agreement, under this reading, systematically transfers wealth and power from domestic regulatory bodies and labor/environmental interests to multinational capital. Suppression is also very high (0.90) due to the legal supremacy of treaty obligations, the enforcement power of international trade tribunals, and the economic leverage of capital flight, which effectively suppresses domestic attempts to reassert regulatory autonomy. Theater ratio is low (0.10) because the 'coordination' function is largely genuine for capital, but the primary function, from this reading's perspective, is extraction, not a performative maintenance of an atrophied function. The increasing extractiveness and suppression over time reflect the 'ratchet effect' where trade agreements are progressively interpreted to expand capital's freedoms and constrain state regulatory capacity.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (capital) perceive this as a 'rope' or even 'mountain' (natural law of efficient markets), essential for global prosperity. The victims (labor, environment, domestic regulators) perceive it as a 'snare' – a coercive mechanism that systematically extracts from them. The engine's classification will reflect this divergence based on the high extractiveness and suppression metrics, despite the 'rope' framing by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations and financial capital holders are clear beneficiaries (d near 0.0) as the constraint directly subsidizes their operations by reducing regulatory costs and opening markets. Domestic labor unions, environmental groups, national regulatory agencies, and local communities are clear targets (d near 1.0) as they bear the direct costs of weakened protections and loss of jurisdictional control. Sovereign states, while signatories, are also targets in their capacity to regulate, as their authority is subordinated to the treaty. Trade tribunals act as agenda-setters, enforcing the capital supremacy interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a snare as a rope by highlighting the active enforcement and suppression required to maintain the jurisdictional boundary. The 'coordination' of capital mobility is achieved through the suppression of domestic regulatory alternatives, which is a hallmark of extraction, not pure coordination. The persistence of the constraint is not due to its original coordination function for all parties, but due to the concentrated benefits for capital and the high costs of exit for states and domestic actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_regulatory_harmonization,
    'What is the true economic and social cost of regulatory harmonization and the suppression of domestic standards, beyond the direct financial transfers?',
    'Comprehensive, independent socio-economic impact assessments that quantify the long-term effects on public health, environmental quality, labor conditions, and democratic accountability, not just trade volumes.',
    'A higher quantified cost would further solidify the ''snare'' classification by demonstrating the full scope of extraction, potentially shifting the ''claimed_type'' for some observers from ''rope'' to ''snare'' even in their own framing. It would also inform policy debates on compensatory mechanisms or re-negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_regulatory_harmonization, empirical, 'Quantifying the full societal cost of regulatory harmonization under trade agreements.').

omega_variable(
    legitimacy_of_trade_tribunals,
    'Is the authority of international trade tribunals, in overriding domestic law, derived from genuine consent of the governed or from a structural power imbalance?',
    'Analysis of public opinion and democratic processes in signatory states regarding the delegation of sovereignty to trade tribunals, and comparison with constitutional mechanisms for amending domestic law.',
    'If derived from structural power imbalance, the ''suppression'' metric would be further amplified, and the ''claimed_type'' as ''rope'' (coordination) would be strongly challenged, even by some institutional observers. If genuine consent is demonstrated, it would slightly dampen the suppression, but the high extractiveness would remain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_trade_tribunals, conceptual, 'Source of legitimacy for trade tribunals'' power over domestic law.').

omega_variable(
    capital_mobility_as_natural_law,
    'Is the free movement of capital a ''natural law'' of economic efficiency (a Mountain), or a constructed policy choice (a Snare) that benefits specific actors?',
    'Historical and comparative analysis of capital controls and their economic effects in different eras and political systems, demonstrating that capital mobility is a policy variable, not an immutable economic force.',
    'If capital mobility is shown to be a constructed choice, the ''emerges_naturally'' claim (often implicit in this reading) would be falsified, and the ''snare'' classification would be reinforced by removing any ''mountain'' cover story. This would shift the debate from inevitability to policy choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_mobility_as_natural_law, conceptual, 'Whether capital mobility is a natural economic law or a policy construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(naft_tr_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(naft_tr_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(naft_tr_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 18, 0.12).
narrative_ontology:measurement(naft_tr_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(naft_be_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(naft_be_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(naft_be_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(naft_be_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(naft_su_t6, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(naft_su_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(naft_su_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(naft_su_t24, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulations).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, national_labor_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'NAFTA jurisdictional boundary' kernel. This 'capital supremacy' reading emphasizes the trade agreement as supreme law overriding domestic regulations, prioritizing capital mobility. It influences and coexists with the 'embedded liberalism' and 'sovereignty primacy' readings, which offer alternative interpretations of the balance between trade and domestic policy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
