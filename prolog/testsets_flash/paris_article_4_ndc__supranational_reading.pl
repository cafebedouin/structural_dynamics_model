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
 *   This constraint represents the 'supranational reading' of Nationally
 *   Determined Contributions (NDCs) under Article 4 of the Paris Agreement.
 *   In this reading, NDCs are understood as binding international commitments
 *   that ratchet towards net-zero emissions, enforced through international
 *   accountability mechanisms. This interpretation implies significant
 *   extraction from high-emitting states and carbon-intensive industries,
 *   with corresponding benefits for the global climate regime and
 *   climate-vulnerable nations. It stands in contrast to sovereigntist or
 *   equity-focused readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.85).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.75).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDCs (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, 'f3fb31ed-66f5-448d-91df-faf4fc519d13').
narrative_ontology:cs_kernel_codification('f3fb31ed-66f5-448d-91df-faf4fc519d13', formalized).
narrative_ontology:cs_authority_grounding('f3fb31ed-66f5-448d-91df-faf4fc519d13', lineage).
narrative_ontology:cs_interpretation_layer_present('f3fb31ed-66f5-448d-91df-faf4fc519d13').
narrative_ontology:cs_reading_relation('f3fb31ed-66f5-448d-91df-faf4fc519d13', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('f3fb31ed-66f5-448d-91df-faf4fc519d13', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('f3fb31ed-66f5-448d-91df-faf4fc519d13', foundational, ndcs_are_legally_binding).
narrative_ontology:cs_axiom_status(ndcs_are_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('f3fb31ed-66f5-448d-91df-faf4fc519d13', ndcs_are_legally_binding, conventional).
narrative_ontology:cs_axiom('f3fb31ed-66f5-448d-91df-faf4fc519d13', foundational, global_climate_security_trumps_national_sovereignty).
narrative_ontology:cs_axiom_status(global_climate_security_trumps_national_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f3fb31ed-66f5-448d-91df-faf4fc519d13', global_climate_security_trumps_national_sovereignty, deontological).
narrative_ontology:cs_reference_frame('f3fb31ed-66f5-448d-91df-faf4fc519d13', ratcheting_ambition_framework).
narrative_ontology:cs_drift_state('f3fb31ed-66f5-448d-91df-faf4fc519d13', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f3fb31ed-66f5-448d-91df-faf4fc519d13', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, global_climate_regime).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, high_emitting_developed_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Paris Agreement, facilitates NDC submission and review, and seeks to enforce compliance through reputational pressure and potential sanctions. Benefits from the perceived legitimacy and effectiveness of the international climate framework.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, global_climate_regime, agenda_setter,
    institutional, generational, constrained, global).

% Are bound by increasingly stringent NDC targets, requiring significant economic restructuring, investment in decarbonization, and potential financial transfers to developing nations. Face reputational and economic penalties for non-compliance.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, high_emitting_developed_nations, payer,
    powerful, generational, constrained, global).

% Face existential threats to their economic models as global demand for fossil fuels declines due to NDC implementation. Their national identity and geopolitical power are often tied to hydrocarbon exports, making exit from this economic model extremely difficult.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations, payer,
    powerful, biographical, identity_locked, global).

% Are direct targets of domestic policies enacted to meet NDCs, facing carbon pricing, regulatory phase-outs, and stranded asset risks. Their business models are fundamentally challenged, with limited options for continued operation without radical transformation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    organized, immediate, trapped, global).

% Benefits immensely from policies driven by NDCs, including subsidies, mandates, and carbon pricing that make renewables more competitive. Experiences rapid growth and market expansion as the global economy decarbonizes.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_sector, beneficiary,
    organized, generational, arbitrage, global).

% Benefit from the collective action to mitigate climate change, which is essential for their long-term survival and development. They advocate for stronger NDCs and robust accountability mechanisms, but remain highly vulnerable to climate impacts regardless of their own emissions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations, beneficiary,
    moderate, civilizational, trapped, global).

% Oppose the supranational interpretation of NDCs, viewing them as infringements on national sovereignty and economic self-determination. They would argue for voluntary pledges without international enforcement, but are currently marginalized in the dominant discourse of this reading.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, sovereigntist_political_factions, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to reduce greenhouse gas emissions by establishing a common framework for national climate action, enabling collective progress towards a shared temperature goal.
% TRANSFER_FUNCTION: Transfers economic costs and regulatory burdens from the global climate system (and future generations) to current carbon-intensive industries and high-emitting nations, while transferring wealth and technological capacity to the renewable energy sector and climate-vulnerable nations.
% ABSENT_VOICES: Sovereigntist political factions and industries heavily reliant on fossil fuels are actively resisting or excluded from shaping the supranational interpretation. They would argue for national self-determination over international mandates and prioritize short-term economic growth over climate targets.
% DISAPPEARANCE_RATIONALE: If the supranational interpretation of NDCs vanished, the international climate regime would lose its primary enforcement mechanism. Nations would likely revert to less ambitious, voluntary pledges, leading to a significant increase in global emissions and a failure to meet temperature targets, fundamentally altering global economic and geopolitical landscapes.
% FOUNDING_PROBLEM: The problem of global climate change requires collective action, but previous international agreements lacked sufficient ambition, binding commitments, and a mechanism for ratcheting up efforts over time.
% FOUNDING_PROBLEM_CORROBORATION: The scientific community, UN bodies, and climate-vulnerable nations consistently corroborate that the problem of insufficient collective climate action remains live, and that NDCs, under a strong interpretation, are essential to address it. This is attested by IPCC reports and UN climate summits, which are outside the direct beneficiaries of the current regime.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).

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
 *   Extractiveness is high (0.85) because this reading implies substantial economic restructuring and wealth transfers, imposing significant costs on high-emitting entities. Suppression (0.75) is also high, reflecting the active enforcement of international norms and potential sanctions against non-compliant states, as well as the regulatory pressure on industries. The theater ratio (0.20) is relatively low, as this reading emphasizes genuine, impactful action over mere symbolic gestures, though some performative elements in international diplomacy remain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the global climate regime and climate-vulnerable nations, this constraint is a necessary and legitimate mechanism for collective survival. From the perspective of carbon-intensive industries and fossil-fuel exporting nations, it is a highly extractive and suppressive imposition on their sovereignty and economic viability. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The global climate regime and renewable energy sector are clear beneficiaries (low directionality) as they gain legitimacy, resources, and market share. High-emitting developed nations, fossil-fuel exporting nations, and carbon-intensive industries are targets (high directionality) as they bear the primary costs of decarbonization and face significant economic disruption. Climate-vulnerable nations are beneficiaries of the overall effort, but remain trapped by their inherent vulnerability.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine, albeit costly, collective action as pure extraction. While highly extractive for some, the coordination function of addressing climate change is central. The high extractiveness is a feature, not a bug, of this reading, as it aims to force a global transition. The 'mandate' is to achieve net-zero, which is far from resolved, hence no mandatrophy is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_voluntary_ambiguity,
    'Are NDCs truly binding international commitments with enforceable accountability, or are they voluntary pledges that states can unilaterally adjust without significant penalty?',
    'Observation of state behavior in response to non-compliance (e.g., imposition of trade sanctions, diplomatic isolation, or financial penalties) and rulings by international tribunals on the legal status of NDCs.',
    'If NDCs are found to be purely voluntary, the extractiveness and suppression metrics of this constraint would drop significantly, reclassifying it from a Tangled Rope to a Rope or even a Piton, as its enforcement mechanism would be largely theatrical. If binding, the current high extractiveness and suppression are justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_vs_voluntary_ambiguity, empirical, 'Ambiguity regarding the legal enforceability of NDCs under international law.').

omega_variable(
    supranational_vs_sovereigntist_framing,
    'Is the Paris Agreement fundamentally a supranational legal instrument that constrains state sovereignty, or does it primarily preserve national self-determination in climate action?',
    'Analysis of state practice, treaty interpretations by international legal scholars, and the outcomes of international disputes. This is a conceptual choice about the nature of international law.',
    'Adopting a sovereigntist framing would drastically reduce the perceived extractiveness and suppression, as states would be seen as freely choosing their commitments, not being coerced. This would shift the classification towards a Rope or even a Mountain (if the ''choice'' is seen as natural).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supranational_vs_sovereigntist_framing, conceptual, 'The fundamental conceptual framing of the Paris Agreement''s authority.').

omega_variable(
    equity_vs_universalism_tradeoff,
    'Does the universal application of ratcheting NDCs adequately address historical responsibilities and differentiated capabilities between developed and developing nations, or does it perpetuate an inequitable burden?',
    'Empirical analysis of financial flows, technology transfers, and climate impacts on developing nations, alongside normative debates on climate justice and ''common but differentiated responsibilities''.',
    'If the current framework is deemed inequitable, the ''beneficiary'' status of climate-vulnerable nations might be re-evaluated as ''constrained payers'' or ''victims'' of an unjust system, even if the overall climate goal is pursued. This would increase the effective extraction from these nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_vs_universalism_tradeoff, preference, 'The normative tension between universal climate action and historical equity concerns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(pari_tr_t2018, paris_article_4_ndc__supranational_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__supranational_reading, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__supranational_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__supranational_reading, theater_ratio, 2027, 0.21).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__supranational_reading, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(pari_be_t2018, paris_article_4_ndc__supranational_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__supranational_reading, base_extractiveness, 2021, 0.75).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__supranational_reading, base_extractiveness, 2024, 0.8).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__supranational_reading, base_extractiveness, 2027, 0.83).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__supranational_reading, base_extractiveness, 2030, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(pari_su_t2018, paris_article_4_ndc__supranational_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__supranational_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__supranational_reading, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__supranational_reading, suppression_requirement, 2027, 0.73).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__supranational_reading, suppression_requirement, 2030, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, domestic_carbon_pricing_schemes).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, international_carbon_markets).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Paris Agreement Article 4 NDCs' kernel. This 'supranational_reading' emphasizes binding commitments and international accountability, contrasting with the 'sovereigntist_reading' (voluntary pledges) and the 'equity_reading' (differentiated responsibilities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
