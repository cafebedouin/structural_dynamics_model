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
 *   This constraint represents the 'supranational reading' of Nationally
 *   Determined Contributions (NDCs) under Article 4 of the Paris Agreement.
 *   In this reading, NDCs are interpreted as binding international
 *   commitments, subject to robust international accountability mechanisms,
 *   driving a ratcheting ambition towards net-zero emissions. This
 *   interpretation implies significant shifts in national sovereignty, wealth
 *   transfers, and the eventual regulatory extinction of carbon-intensive
 *   industries. The high extractiveness and suppression reflect the coercive
 *   force required to achieve these outcomes against national and industrial
 *   resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.85).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.78).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDCs (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '801b00a3-5fe6-42fa-a503-633940c1c4fc').
narrative_ontology:cs_kernel_codification('801b00a3-5fe6-42fa-a503-633940c1c4fc', formalized).
narrative_ontology:cs_authority_grounding('801b00a3-5fe6-42fa-a503-633940c1c4fc', lineage).
narrative_ontology:cs_interpretation_layer_present('801b00a3-5fe6-42fa-a503-633940c1c4fc').
narrative_ontology:cs_reading_relation('801b00a3-5fe6-42fa-a503-633940c1c4fc', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('801b00a3-5fe6-42fa-a503-633940c1c4fc', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('801b00a3-5fe6-42fa-a503-633940c1c4fc', foundational, ndcs_are_binding_international_law).
narrative_ontology:cs_axiom_status(ndcs_are_binding_international_law, holdable).
narrative_ontology:cs_axiom_grounding('801b00a3-5fe6-42fa-a503-633940c1c4fc', ndcs_are_binding_international_law, conventional).
narrative_ontology:cs_axiom('801b00a3-5fe6-42fa-a503-633940c1c4fc', foundational, international_accountability_is_paramount).
narrative_ontology:cs_axiom_status(international_accountability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('801b00a3-5fe6-42fa-a503-633940c1c4fc', international_accountability_is_paramount, deontological).
narrative_ontology:cs_reference_frame('801b00a3-5fe6-42fa-a503-633940c1c4fc', post_paris_agreement_international_legal_order).
narrative_ontology:cs_drift_state('801b00a3-5fe6-42fa-a503-633940c1c4fc', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('801b00a3-5fe6-42fa-a503-633940c1c4fc', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_climate_regime).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developing_nations_reliant_on_fossil_fuels).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The UNFCCC and associated bodies (e.g., IPCC) that interpret and enforce the Paris Agreement. They push for stronger, more binding commitments and accountability mechanisms, benefiting from the expansion of their mandate and influence.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_climate_regime, agenda_setter,
    institutional, generational, constrained, global).

% Nations most susceptible to climate change impacts. They benefit from the binding nature and ratcheting ambition of NDCs, as it offers a pathway to mitigate existential threats, and from institutionalized wealth transfers for adaptation and mitigation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations, beneficiary,
    organized, immediate, trapped, global).

% Industries and investors in renewable energy technologies. They benefit from policies and market signals driven by binding NDC commitments, which accelerate the transition away from fossil fuels and create new market opportunities.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_sector, beneficiary,
    powerful, biographical, mobile, global).

% Sectors like heavy manufacturing, transportation, and fossil fuel extraction. They bear the costs of stringent regulations, carbon pricing, and eventual phase-out mandated by binding NDCs, facing potential extinction or costly transitions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Nations whose economies are heavily reliant on the export of oil, gas, or coal. They face significant economic disruption and loss of revenue as binding NDCs drive down global demand for fossil fuels, necessitating costly economic diversification.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations, payer,
    institutional, generational, constrained, global).

% Nations with growing energy demands that currently rely on fossil fuels for economic development. They face the challenge of meeting development goals while being constrained by binding NDCs to transition to cleaner energy, often without sufficient financial or technological support.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developing_nations_reliant_on_fossil_fuels, payer,
    moderate, biographical, identity_locked, national).

% Nations that prioritize national sovereignty over international obligations, viewing NDCs as voluntary pledges. They resist external accountability and sanctions, but are increasingly isolated by the supranational interpretation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, sovereigntist_nations, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to limit global warming by establishing a universal framework for national climate action, ensuring collective progress towards a shared temperature goal through binding, ratcheting commitments.
% TRANSFER_FUNCTION: Transfers regulatory authority and financial resources from individual nations and carbon-intensive industries to international climate governance bodies and climate-vulnerable nations, facilitating a global energy transition.
% ABSENT_VOICES: Nations and industries advocating for a purely voluntary, sovereignty-preserving approach to NDCs are actively marginalized or excluded from the core interpretive process, their arguments for national self-determination overridden by the imperative of international accountability.
% DISAPPEARANCE_RATIONALE: If the supranational interpretation of NDCs vanished, the international climate regime would lose its primary enforcement mechanism, leading to a rapid decline in ambition, a resurgence of carbon-intensive development, and a catastrophic failure to meet climate targets. Global economic and political structures would reorient around national self-interest rather than collective climate action.
% FOUNDING_PROBLEM: The problem of global collective action failure on climate change, where individual national interests led to insufficient emissions reductions, threatening planetary stability and disproportionately impacting vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC and UN Environment Programme consistently corroborate the ongoing and escalating nature of the climate crisis, demonstrating that the founding problem remains critically live. Scientific consensus and observed climate impacts provide external validation for the continued necessity of a strong, internationally accountable climate regime.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.85) because this reading demands significant economic restructuring and resource reallocation from nations and industries, enforced by international pressure and potential sanctions. Suppression is also high (0.78) as it requires overcoming strong national sovereignty claims and powerful industrial lobbies that resist such binding commitments. The theater ratio is low (0.15) because, in this reading, the commitment is genuine and actively enforced, with little room for performative compliance without real action. The increasing extractiveness and suppression over time reflect the ratcheting ambition and hardening enforcement mechanisms envisioned by this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the international climate regime and climate-vulnerable nations, this reading is a necessary and just coordination mechanism. From the perspective of carbon-intensive industries and fossil fuel-exporting nations, it is a highly extractive snare that undermines their economic foundations. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The international climate regime and climate-vulnerable nations are primary beneficiaries, gaining authority, resources, and protection from climate impacts. The renewable energy sector also benefits from the mandated transition. Carbon-intensive industries, fossil fuel-exporting nations, and developing nations reliant on fossil fuels are the primary targets, facing significant costs, regulatory burdens, and economic disruption. Sovereigntist nations are excluded, as their preferred interpretation is actively suppressed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_strength,
    'Are the international accountability mechanisms for NDCs truly binding and enforceable, or do they remain largely reputational and voluntary in practice?',
    'Analysis of actual compliance rates, imposition of sanctions for non-compliance, and legal precedents set by international tribunals regarding NDC obligations.',
    'If enforcement is weak, the constraint''s effective suppression and extractiveness would be lower, potentially reclassifying it closer to a Rope or even a Piton. If strong, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_strength, empirical, 'Uncertainty regarding the actual coercive power of international climate governance.').

omega_variable(
    wealth_transfer_implementation,
    'Will the institutionalized wealth transfers from North to South for climate action be fully realized and effectively deployed, or will they remain largely aspirational?',
    'Tracking of financial flows, disbursement rates, and impact assessments of climate finance mechanisms (e.g., Green Climate Fund).',
    'If transfers are insufficient, the burden on developing nations (victims in this reading) would increase, potentially raising their effective extraction and resistance. If fully realized, it could mitigate some of the extractive pressure on them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_transfer_implementation, empirical, 'Uncertainty about the practical implementation of climate finance commitments.').

omega_variable(
    sovereignty_vs_supranationality,
    'Is the concept of national sovereignty fundamentally compatible with a truly binding and internationally accountable NDC regime, or does one necessarily foreclose the other?',
    'Conceptual analysis of international law and political philosophy, and observation of how states reconcile (or fail to reconcile) these principles in practice.',
    'If sovereignty is deemed to foreclose supranationality, this reading would be conceptually unstable or require a fundamental redefinition of statehood. If compatible, it strengthens the coherence of this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_supranationality, conceptual, 'Conceptual tension between national sovereignty and supranational climate governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__supranational_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__supranational_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__supranational_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__supranational_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__supranational_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__supranational_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__supranational_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__supranational_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__supranational_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is the 'supranational_reading' of the 'paris_article_4_ndc' kernel. It is one of three distinct readings, alongside 'sovereigntist_reading' and 'equity_reading', each representing a different structural interpretation of NDCs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
