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
 *   This constraint represents the 'supranational reading' of the Paris
 *   Agreement's Article 4, which interprets Nationally Determined
 *   Contributions (NDCs) as binding commitments on a ratcheting trajectory
 *   towards net-zero, enforced by international accountability mechanisms.
 *   This reading emphasizes the legal and moral imperative for states to
 *   comply, with significant reputational and financial sanctions for
 *   non-compliance, and institutionalized wealth transfers from North to
 *   South. It posits a high-epsilon constraint system where carbon-intensive
 *   industries face regulatory extinction.
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
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDCs (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '9525e2ed-8bf7-46ba-96a1-d714b6efd962').
narrative_ontology:cs_kernel_codification('9525e2ed-8bf7-46ba-96a1-d714b6efd962', formalized).
narrative_ontology:cs_authority_grounding('9525e2ed-8bf7-46ba-96a1-d714b6efd962', extraction).
narrative_ontology:cs_interpretation_layer_present('9525e2ed-8bf7-46ba-96a1-d714b6efd962').
narrative_ontology:cs_reading_relation('9525e2ed-8bf7-46ba-96a1-d714b6efd962', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('9525e2ed-8bf7-46ba-96a1-d714b6efd962', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('9525e2ed-8bf7-46ba-96a1-d714b6efd962', foundational, global_climate_imperative_binding).
narrative_ontology:cs_axiom_status(global_climate_imperative_binding, holdable).
narrative_ontology:cs_axiom_grounding('9525e2ed-8bf7-46ba-96a1-d714b6efd962', global_climate_imperative_binding, deontological).
narrative_ontology:cs_axiom('9525e2ed-8bf7-46ba-96a1-d714b6efd962', foundational, state_sovereignty_subordinate_to_climate_emergency).
narrative_ontology:cs_axiom_status(state_sovereignty_subordinate_to_climate_emergency, holdable).
narrative_ontology:cs_axiom_grounding('9525e2ed-8bf7-46ba-96a1-d714b6efd962', state_sovereignty_subordinate_to_climate_emergency, conventional).
narrative_ontology:cs_reference_frame('9525e2ed-8bf7-46ba-96a1-d714b6efd962', global_climate_emergency_response).
narrative_ontology:cs_drift_state('9525e2ed-8bf7-46ba-96a1-d714b6efd962', contemporary_geopolitical_realities, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9525e2ed-8bf7-46ba-96a1-d714b6efd962', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, global_climate_regime).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, vulnerable_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, high_emitting_states).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, global_environmental_stewardship).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, intergenerational_equity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The overarching framework of international climate law and institutions (e.g., UNFCCC, IPCC) that sets the agenda for global climate action, defines the ratcheting mechanism, and benefits from the enforcement of binding commitments.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, global_climate_regime, agenda_setter,
    institutional, civilizational, analytical, global).

% Nations most susceptible to climate change impacts, who benefit from ambitious, binding NDCs and the associated wealth transfers for adaptation and mitigation. Their options are limited by their vulnerability and dependence on global cooperation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, vulnerable_nations, beneficiary,
    organized, generational, constrained, global).

% Developed and rapidly industrializing nations with significant historical and current greenhouse gas emissions. They bear the primary costs of emissions reductions, technology transfers, and financial contributions, facing reputational and potential financial sanctions for non-compliance.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, high_emitting_states, payer,
    institutional, generational, constrained, global).

% Industries heavily reliant on fossil fuels or high-emission processes (e.g., coal, oil & gas, heavy manufacturing). They face regulatory extinction, carbon pricing, and stranded asset risks as NDCs ratchet towards net-zero, with limited options for continued operation in their current form.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    organized, biographical, trapped, global).

% Bodies and processes within the UNFCCC framework responsible for reviewing NDC implementation, assessing progress, and facilitating compliance. They enforce the binding nature of commitments through transparency, reporting, and non-compliance procedures.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_accountability_mechanisms, agenda_setter,
    institutional, generational, analytical, global).

% States that prioritize national sovereignty and self-determination, viewing NDCs as voluntary pledges rather than binding commitments. They resist international oversight and sanctions, often seeking to minimize their obligations or exit the accountability framework.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, sovereigntist_states, excluded,
    institutional, generational, constrained, global).

% Civil society organizations, academic groups, and some developing nations that advocate for a stronger interpretation of 'Common But Differentiated Responsibilities and Respective Capabilities' (CBDR-RC), pushing for greater historical responsibility and financial transfers from developed nations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, equity_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, global_climate_regime).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts to reduce greenhouse gas emissions and limit global warming to well below 2°C, preferably to 1.5°C, through a universally adopted, ratcheting mechanism of nationally determined contributions.
% TRANSFER_FUNCTION: Moves financial resources, technology, and capacity-building support from developed to developing nations for climate adaptation and mitigation. It also imposes regulatory and financial costs on high-emitting states and carbon-intensive industries through emissions reduction targets and potential sanctions.
% ABSENT_VOICES: States prioritizing national sovereignty over international obligations, and those demanding more radical equity-based differentiation, are often marginalized in the enforcement discourse. They would argue for less binding or more differentiated commitments, but their positions are often overridden by the supranational imperative.
% DISAPPEARANCE_RATIONALE: If the binding nature of NDCs and international accountability vanished overnight, global climate action would likely collapse into voluntary, insufficient efforts. This would lead to catastrophic climate change, massive geopolitical instability, and severe economic disruption as climate impacts accelerate unchecked.
% FOUNDING_PROBLEM: The failure of previous international climate agreements (e.g., Kyoto Protocol) to secure universal participation and sufficiently ambitious, legally binding commitments to address the existential threat of anthropogenic climate change, leading to insufficient global emissions reductions.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on climate change (IPCC reports), the increasing frequency and intensity of extreme weather events, and the ongoing negotiations at COPs all corroborate the urgency and live status of the founding problem. This corroboration comes from independent scientific bodies and international organizations, not solely from the benefiting parties.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) reflects the significant economic restructuring, wealth transfers, and potential sanctions imposed on high-emitting states and industries. Suppression (0.75) is high due to the active enforcement by international bodies and the pressure to conform to global climate goals, limiting alternatives for non-compliance. The low theater ratio (0.15) indicates that, from this reading's perspective, the mechanisms are genuinely functional and not merely performative, with real consequences for non-adherence. Accessibility collapse is moderate-high (0.7) as states face strong pressure to align their policies, and resistance is high (0.8) due to the profound economic and political shifts required.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of high-emitting states and carbon-intensive industries, this constraint is highly extractive and suppressive, limiting their economic freedom. However, from the global climate regime and vulnerable nations' perspective, it is a necessary coordination mechanism to avert catastrophic climate change, with extraction being a justified cost of global public good provision. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The global climate regime and vulnerable nations are primary beneficiaries (low d) as they gain from a stable climate and financial transfers. High-emitting states and carbon-intensive industries are clear targets (high d) due to the costs, regulations, and sanctions they face. International accountability mechanisms act as agenda-setters, enforcing the constraint. Sovereigntist states are excluded, as their voluntary interpretation is rejected by this reading's binding framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This supranational reading inherently resists mandatrophy by emphasizing binding commitments, a ratcheting ambition mechanism, and active international accountability. The constraint's persistence is tied to the ongoing, live problem of climate change, and its enforcement ensures that its function does not atrophy into mere performance. The high extractiveness and suppression are seen as necessary to prevent the mandate from becoming inert.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_voluntary_status,
    'Are NDCs truly binding commitments under international law, or are they primarily voluntary pledges as asserted by sovereigntist readings?',
    'Decisions by international courts (e.g., ICJ) on state obligations regarding NDCs, or the consistent application of sanctions and enforcement mechanisms against non-compliant states.',
    'If NDCs are found to be purely voluntary, the constraint''s extractiveness and suppression would be significantly lower, reclassifying it closer to a Rope or even a Piton. If binding, the high extraction and suppression are justified by the legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_voluntary_status, conceptual, 'Ambiguity regarding the legal enforceability of NDCs.').

omega_variable(
    efficacy_of_sanctions_and_transfers,
    'Are the international accountability mechanisms and wealth transfers genuinely effective in driving compliance and achieving climate goals, or are they largely symbolic?',
    'Empirical analysis of emissions trajectories, financial flows, and compliance rates in relation to the application of sanctions or incentives over time.',
    'If found ineffective or symbolic, the measured extractiveness and suppression might be inflated relative to actual impact, suggesting a higher theater_ratio and a reclassification towards Piton or a less effective Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_sanctions_and_transfers, empirical, 'Effectiveness of enforcement and financial mechanisms.').

omega_variable(
    equity_vs_ambition_tradeoff,
    'Does the ratcheting ambition trajectory adequately incorporate principles of equity and common but differentiated responsibilities, or does it disproportionately burden developing nations?',
    'Analysis of the distribution of mitigation and adaptation burdens and benefits across different national income levels, and the extent to which historical responsibility is factored into commitments.',
    'If equity is found to be severely lacking, the constraint''s legitimacy could erode, increasing resistance and potentially leading to a breakdown of the coordination function, pushing it towards a Snare for developing nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_vs_ambition_tradeoff, preference, 'Whether the ambition mechanism is equitable in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2045).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(pari_tr_t2020, paris_article_4_ndc__supranational_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(pari_tr_t2025, paris_article_4_ndc__supranational_reading, theater_ratio, 2025, 0.13).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__supranational_reading, theater_ratio, 2030, 0.14).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__supranational_reading, theater_ratio, 2035, 0.14).
narrative_ontology:measurement(pari_tr_t2040, paris_article_4_ndc__supranational_reading, theater_ratio, 2040, 0.15).
narrative_ontology:measurement(pari_tr_t2045, paris_article_4_ndc__supranational_reading, theater_ratio, 2045, 0.15).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(pari_be_t2020, paris_article_4_ndc__supranational_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(pari_be_t2025, paris_article_4_ndc__supranational_reading, base_extractiveness, 2025, 0.75).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__supranational_reading, base_extractiveness, 2030, 0.8).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__supranational_reading, base_extractiveness, 2035, 0.83).
narrative_ontology:measurement(pari_be_t2040, paris_article_4_ndc__supranational_reading, base_extractiveness, 2040, 0.84).
narrative_ontology:measurement(pari_be_t2045, paris_article_4_ndc__supranational_reading, base_extractiveness, 2045, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(pari_su_t2020, paris_article_4_ndc__supranational_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(pari_su_t2025, paris_article_4_ndc__supranational_reading, suppression_requirement, 2025, 0.65).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__supranational_reading, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__supranational_reading, suppression_requirement, 2035, 0.73).
narrative_ontology:measurement(pari_su_t2040, paris_article_4_ndc__supranational_reading, suppression_requirement, 2040, 0.74).
narrative_ontology:measurement(pari_su_t2045, paris_article_4_ndc__supranational_reading, suppression_requirement, 2045, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, global_carbon_markets).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, climate_finance_mechanisms).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, national_climate_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'paris_article_4_ndc' kernel, emphasizing its binding, supranational character. It is structurally distinct from the 'sovereigntist_reading' (voluntary pledges) and 'equity_reading' (CBDR-RC focus), which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
