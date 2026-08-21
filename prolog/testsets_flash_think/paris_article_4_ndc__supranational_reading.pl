% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Agreement Article 4 NDCs (Supranational Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'supranational reading' of
 *   Nationally Determined Contributions (NDCs) under Article 4 of the Paris
 *   Agreement. In this reading, NDCs are understood as binding international
 *   commitments, subject to a ratcheting mechanism towards net-zero
 *   emissions, and enforced through international accountability, including
 *   reputational and financial sanctions. This interpretation leads to
 *   significant wealth transfers and regulatory pressure on carbon-intensive
 *   sectors, reflecting a high-epsilon constraint system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.85).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.75).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDCs (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, 'e877e8bd-b52f-4f03-85d3-43c3142ec4b4').
narrative_ontology:cs_kernel_codification('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', formalized).
narrative_ontology:cs_authority_grounding('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', lineage).
narrative_ontology:cs_interpretation_layer_present('e877e8bd-b52f-4f03-85d3-43c3142ec4b4').
narrative_ontology:cs_reading_relation('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', foundational, climate_action_is_supranational_imperative).
narrative_ontology:cs_axiom_status(climate_action_is_supranational_imperative, holdable).
narrative_ontology:cs_axiom_grounding('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', climate_action_is_supranational_imperative, deontological).
narrative_ontology:cs_axiom('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', foundational, states_must_cede_sovereignty_for_climate).
narrative_ontology:cs_axiom_status(states_must_cede_sovereignty_for_climate, holdable).
narrative_ontology:cs_axiom_grounding('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', states_must_cede_sovereignty_for_climate, conventional).
narrative_ontology:cs_reference_frame('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', paris_agreement_2015_framework).
narrative_ontology:cs_drift_state('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', contemporary_climate_crisis, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e877e8bd-b52f-4f03-85d3-43c3142ec4b4', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_climate_institutions).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, high_emitting_developed_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., UNFCCC, UNEP) are tasked with facilitating, monitoring, and enforcing the Paris Agreement's mechanisms, including NDCs. They interpret NDCs as binding, ratcheting commitments and advocate for stronger international accountability and compliance mechanisms. They benefit from the expansion of their mandate and influence.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_climate_institutions, agenda_setter,
    institutional, generational, analytical, global).

% These nations, often small island developing states or least developed countries, are disproportionately affected by climate change. They benefit from the binding nature of NDCs, the ratcheting ambition, and the institutionalization of wealth transfers (e.g., climate finance) from developed nations, which are crucial for adaptation and mitigation efforts.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations, beneficiary,
    organized, generational, constrained, global).

% This sector benefits significantly from policies driven by binding NDCs, such as carbon pricing, subsidies for renewables, and regulations phasing out fossil fuels. The ratcheting ambition creates a predictable, growing market for their technologies and services, leading to substantial economic gains.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_sector, beneficiary,
    organized, biographical, arbitrage, global).

% These nations bear significant costs associated with decarbonizing their economies, investing in renewable energy, and providing climate finance to developing countries. They face reputational and potential financial sanctions for non-compliance with their NDCs, and their industries are subject to stringent regulations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, high_emitting_developed_nations, payer,
    institutional, biographical, constrained, global).

% Nations heavily reliant on fossil fuel exports face existential threats from the global transition to net-zero. Binding NDCs and ratcheting ambition lead to declining demand for their primary exports, risking stranded assets and economic instability. Their options for diversification are limited and costly.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations, payer,
    institutional, biographical, trapped, global).

% Industries such as heavy manufacturing, aviation, and agriculture face increasing regulatory pressure, carbon taxes, and the threat of 'regulatory extinction' if they cannot decarbonize rapidly. They bear direct costs of compliance, technological upgrades, or reduced market share, with limited options for avoiding these pressures.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, immediate, constrained, global).

% States that prioritize national sovereignty and resist supranational authority are structurally marginalized by this reading. While they may be parties to the Paris Agreement, their interpretation of NDCs as voluntary pledges is overridden by the enforcement mechanisms of the supranational reading, leading to political and economic isolation if they do not comply.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, sovereigntist_states, excluded,
    institutional, biographical, constrained, national).

% These advocates emphasize Common But Differentiated Responsibilities and Respective Capabilities (CBDR-RC), arguing for greater historical responsibility and financial transfers from developed to developing nations. While this reading incorporates some transfers, equity advocates often push for more robust and explicit mechanisms, observing the current system's limitations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, equity_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts to reduce greenhouse gas emissions, preventing a collective action problem where individual states free-ride on others' mitigation efforts, thereby limiting global warming to well below 2°C, preferably to 1.5°C.
% TRANSFER_FUNCTION: Moves regulatory burden, financial resources, and technological innovation from high-emitting developed nations and carbon-intensive industries towards climate-vulnerable nations and the renewable energy sector, enforced through international accountability mechanisms and market signals.
% ABSENT_VOICES: States and industries that reject the premise of binding supranational climate governance, or those who prioritize immediate national economic growth and energy sovereignty above all else, are marginalized in the discourse and enforcement mechanisms of this reading. Their arguments for voluntary action or slower transitions are overridden by the imperative of international accountability.
% DISAPPEARANCE_RATIONALE: If the binding nature, ratcheting ambition, and international accountability of NDCs vanished overnight, global emissions would likely rebound significantly. Climate targets would be missed, leading to accelerated climate change impacts. The international framework for climate action would collapse, resulting in severe geopolitical instability, increased climate-induced migration, and widespread ecological and economic damage.
% FOUNDING_PROBLEM: Uncoordinated national efforts and voluntary pledges under previous climate agreements (e.g., Kyoto Protocol) were insufficient to address the global climate crisis, leading to insufficient ambition, free-riding, and a trajectory towards catastrophic warming.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, scientific consensus on the urgency of climate action, and ongoing climate impacts (e.g., extreme weather events, sea-level rise) from outside the benefiting parties consistently corroborate that the founding problem of insufficient global climate action remains live and critical.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the substantial costs imposed on high-emitting nations and industries through decarbonization mandates, carbon pricing, and climate finance transfers. Suppression (0.75) is high due to the active enforcement mechanisms, including international pressure, trade implications, and the threat of sanctions for non-compliance. The low theater ratio (0.15) indicates that the commitments are genuinely binding and actively pursued, with little performative maintenance. Resistance (0.7) is high due to the significant economic and political shifts required, while accessibility collapse (0.65) is moderate, as withdrawal from the Paris Agreement is technically possible but carries immense political and economic costs.
 *
 * PERSPECTIVAL GAP:
 *   The supranational reading fundamentally diverges from the sovereigntist reading, which views NDCs as voluntary pledges, and the equity reading, which prioritizes differentiated responsibilities above all else. From the perspective of the beneficiaries in this reading, the constraint is a necessary and effective mechanism for global survival. From the payer seats, it is a coercive imposition on national economies and industries. The engine's per-seat classification will highlight these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   International climate institutions, climate-vulnerable nations, and the renewable energy sector are beneficiaries, gaining mandate, protection, and market growth, respectively. High-emitting developed nations, fossil fuel exporters, and carbon-intensive industries are targets, bearing the costs of decarbonization, stranded assets, and regulatory extinction. Sovereigntist states are excluded, as their preferred interpretation of voluntary pledges is overridden by the supranational enforcement mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is actively evolving and intensifying, with a clear and present mandate to address the climate crisis. The ratcheting mechanism ensures that the constraint's function is continually updated and strengthened, preventing mandatrophy. The high extractiveness and suppression are direct consequences of this live and urgent mandate, not signs of atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_robustness_ambiguity,
    'Are the international accountability mechanisms and potential sanctions sufficiently robust to ensure compliance with binding NDCs, or do they remain primarily reputational?',
    'Observation of actual implementation of trade-related carbon border adjustments, financial penalties, or other coercive measures against non-compliant states over time.',
    'If enforcement proves weak, the effective suppression and extractiveness of the constraint would be lower than currently assessed, potentially reclassifying it closer to a Rope or even a Piton if the binding nature becomes purely theatrical. If robust, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_robustness_ambiguity, empirical, 'The actual coercive power of international climate governance.').

omega_variable(
    equity_vs_binding_priority,
    'To what extent does the emphasis on binding commitments and international accountability in this reading genuinely integrate principles of equity and common but differentiated responsibilities, versus subordinating them to a universal decarbonization mandate?',
    'Analysis of climate finance flows, technology transfer mechanisms, and differentiated compliance pathways for developing nations, assessed against the demands of the equity reading.',
    'If equity principles are found to be substantially subordinated, the constraint''s legitimacy and long-term stability could be undermined, increasing resistance from developing nations. If well-integrated, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_vs_binding_priority, conceptual, 'The balance between universal binding commitments and differentiated responsibilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2045).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(pari_tr_t2020, paris_article_4_ndc__supranational_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(pari_tr_t2025, paris_article_4_ndc__supranational_reading, theater_ratio, 2025, 0.16).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__supranational_reading, theater_ratio, 2030, 0.15).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__supranational_reading, theater_ratio, 2035, 0.15).
narrative_ontology:measurement(pari_tr_t2040, paris_article_4_ndc__supranational_reading, theater_ratio, 2040, 0.15).
narrative_ontology:measurement(pari_tr_t2045, paris_article_4_ndc__supranational_reading, theater_ratio, 2045, 0.15).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(pari_be_t2020, paris_article_4_ndc__supranational_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(pari_be_t2025, paris_article_4_ndc__supranational_reading, base_extractiveness, 2025, 0.81).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__supranational_reading, base_extractiveness, 2030, 0.83).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__supranational_reading, base_extractiveness, 2035, 0.84).
narrative_ontology:measurement(pari_be_t2040, paris_article_4_ndc__supranational_reading, base_extractiveness, 2040, 0.85).
narrative_ontology:measurement(pari_be_t2045, paris_article_4_ndc__supranational_reading, base_extractiveness, 2045, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(pari_su_t2020, paris_article_4_ndc__supranational_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(pari_su_t2025, paris_article_4_ndc__supranational_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__supranational_reading, suppression_requirement, 2030, 0.73).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__supranational_reading, suppression_requirement, 2035, 0.74).
narrative_ontology:measurement(pari_su_t2040, paris_article_4_ndc__supranational_reading, suppression_requirement, 2040, 0.75).
narrative_ontology:measurement(pari_su_t2045, paris_article_4_ndc__supranational_reading, suppression_requirement, 2045, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, global_carbon_markets).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, national_climate_legislation).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, climate_finance_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'paris_article_4_ndc' kernel. This 'supranational_reading' emphasizes binding commitments and international accountability, leading to high extraction and suppression. It is linked to the 'sovereigntist_reading' and 'equity_reading' which offer alternative interpretations of NDCs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
