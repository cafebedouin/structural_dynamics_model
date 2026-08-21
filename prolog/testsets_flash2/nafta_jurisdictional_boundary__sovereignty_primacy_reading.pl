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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary (Sovereignty Primacy Reading)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primacy' reading of the NAFTA
 *   jurisdictional boundary, where the trade agreement functions as a
 *   coordination mechanism subordinate to domestic law. Under this reading,
 *   member states retain full regulatory authority over internal standards,
 *   and treaty obligations do not override national sovereignty. Extraction
 *   is limited to the voluntary compliance costs of participating in a
 *   coordinated trade regime, not from a loss of regulatory power. This is
 *   one reading of the 'nafta_jurisdictional_boundary' kernel.
 *
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
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary (Sovereignty Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '62e697b3-8938-4073-86fb-6e99ad570be1').
narrative_ontology:cs_kernel_codification('62e697b3-8938-4073-86fb-6e99ad570be1', fixed_text).
narrative_ontology:cs_authority_grounding('62e697b3-8938-4073-86fb-6e99ad570be1', lineage).
narrative_ontology:cs_interpretation_layer_present('62e697b3-8938-4073-86fb-6e99ad570be1').
narrative_ontology:cs_reading_relation('62e697b3-8938-4073-86fb-6e99ad570be1', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('62e697b3-8938-4073-86fb-6e99ad570be1', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('62e697b3-8938-4073-86fb-6e99ad570be1', foundational, sovereign_regulatory_authority_is_supreme).
narrative_ontology:cs_axiom_status(sovereign_regulatory_authority_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('62e697b3-8938-4073-86fb-6e99ad570be1', sovereign_regulatory_authority_is_supreme, deontological).
narrative_ontology:cs_axiom('62e697b3-8938-4073-86fb-6e99ad570be1', secondary, treaty_obligations_are_voluntary_commitments).
narrative_ontology:cs_axiom_status(treaty_obligations_are_voluntary_commitments, holdable).
narrative_ontology:cs_axiom_grounding('62e697b3-8938-4073-86fb-6e99ad570be1', treaty_obligations_are_voluntary_commitments, conventional).
narrative_ontology:cs_reference_frame('62e697b3-8938-4073-86fb-6e99ad570be1', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('62e697b3-8938-4073-86fb-6e99ad570be1', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('62e697b3-8938-4073-86fb-6e99ad570be1', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, member_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full sovereign authority over domestic regulatory standards (labor, environmental, health). Treaty obligations are interpreted as voluntary commitments that do not override national law, but rather coordinate trade practices. They benefit from stable trade relations without ceding regulatory autonomy.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, member_states, agenda_setter,
    institutional, generational, mobile, national).

% Maintain their full jurisdictional authority to set and enforce standards within their territory. They benefit from the clarity that trade agreements do not diminish their power, allowing them to pursue national policy objectives without direct challenge from treaty provisions.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Must comply with the full range of domestic regulatory standards in each member state, even if these standards vary or impose higher costs than trade agreement provisions. They bear the costs of adapting to diverse national regulations, limiting their ability to arbitrage regulatory differences.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations, payer,
    powerful, biographical, constrained, global).

% Adjudicate disputes based on the treaty text, but under this reading, they must defer to national sovereignty in regulatory matters. Their role is to ensure non-discriminatory application of domestic law, not to override it. They observe the limits of treaty authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels, observer,
    institutional, immediate, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for predictable trade relations and market access among member states, allowing for economic integration while explicitly preserving national regulatory autonomy.
% TRANSFER_FUNCTION: Facilitates the flow of goods and services by reducing tariffs and non-tariff barriers, while ensuring that regulatory authority (and associated compliance costs) remains with national governments, not transferred to a supranational body.
% ABSENT_VOICES: Advocates for deeper economic integration and regulatory harmonization, particularly those representing capital interests seeking to minimize compliance costs across borders, would argue for a stronger, more preemptive role for trade agreements.
% DISAPPEARANCE_RATIONALE: If this understanding of the jurisdictional boundary vanished, member states would likely face increased challenges to their domestic regulatory authority, leading to a re-evaluation of their participation in trade agreements or a push for new, more explicit sovereignty-preserving clauses. Trade flows would become less predictable as the balance shifted.
% FOUNDING_PROBLEM: The need to facilitate cross-border trade and economic cooperation while respecting the sovereign right of nations to govern their internal affairs and protect their citizens through domestic regulation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in international law and constitutional law, as well as national legislative bodies, consistently affirm the principle of sovereign regulatory authority as a foundational element of international agreements, corroborating that this problem remains central to treaty interpretation.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.25) because the agreement primarily coordinates trade without imposing significant regulatory costs beyond what states voluntarily accept for market access. Suppression is low (0.15) as states are not coerced into ceding regulatory authority; compliance is based on mutual benefit. Theater ratio is low (0.1) as the stated function of coordinating trade while preserving sovereignty is genuinely pursued. The metrics reflect a stable, non-extractive coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member states, this reading ensures a balanced approach to trade and sovereignty. From the perspective of multinational corporations, it represents a 'cost of doing business' due to fragmented regulatory landscapes, which they would prefer to see harmonized or overridden by trade law. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states and their domestic regulatory agencies are beneficiaries, as they achieve trade benefits without sacrificing sovereignty. Multinational corporations are payers, as they must navigate diverse national regulations, which limits their ability to externalize costs or arbitrage regulatory differences. Trade dispute panels act as observers, interpreting the treaty within the bounds of national sovereignty.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_risk,
    'How resilient is this ''sovereignty primacy'' reading to interpretive drift by trade dispute panels or future treaty amendments?',
    'Analysis of dispute panel rulings over time, particularly those involving regulatory challenges, and examination of subsequent treaty renegotiations for explicit sovereignty-preserving language.',
    'If the reading proves fragile to drift, the constraint could shift towards a ''tangled_rope'' or ''snare'' as regulatory authority is subtly eroded, increasing extractiveness for member states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_risk, empirical, 'Risk of interpretive shift undermining sovereignty primacy.').

omega_variable(
    implicit_regulatory_chill,
    'Does the mere existence of trade agreements, even under this reading, create an implicit ''regulatory chill'' where states avoid certain domestic regulations to prevent potential trade disputes?',
    'Empirical studies comparing regulatory output in trade-exposed sectors versus non-exposed sectors, or surveys of regulatory agencies regarding perceived constraints from trade agreements.',
    'If a significant ''regulatory chill'' is present, the effective suppression and extractiveness for member states would be higher than measured, pushing the constraint towards a ''tangled_rope'' by subtly limiting policy space.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_regulatory_chill, empirical, 'Unmeasured impact of trade agreements on domestic regulatory ambition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nafta_jurisdictional_boundary' kernel. This 'sovereignty_primacy_reading' emphasizes the subordination of trade law to domestic regulatory authority, contrasting with 'capital_supremacy_reading' (trade law overrides domestic standards) and 'embedded_liberalism_reading' (trade law balances market access with policy space).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
