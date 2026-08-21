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
 *   This constraint is the 'sovereignty primacy' reading of the
 *   'nafta_jurisdictional_boundary' kernel. It interprets trade agreement
 *   text (like NAFTA/USMCA) as a coordination mechanism subordinate to
 *   sovereign domestic law, asserting that states retain full regulatory
 *   authority over labor, environmental, and health standards within their
 *   territory. This reading emphasizes the protection of national policy
 *   space against challenges based on international trade obligations.
 *   Sibling readings include 'capital_supremacy_reading' and
 *   'embedded_liberalism_reading'.
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
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary (Sovereignty Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d').
narrative_ontology:cs_kernel_codification('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', fixed_text).
narrative_ontology:cs_authority_grounding('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', lineage).
narrative_ontology:cs_interpretation_layer_present('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d').
narrative_ontology:cs_reading_relation('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', foundational, state_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(state_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', state_sovereignty_is_primary, deontological).
narrative_ontology:cs_axiom('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', foundational, treaty_is_contract_not_constitution).
narrative_ontology:cs_axiom_status(treaty_is_contract_not_constitution, holdable).
narrative_ontology:cs_axiom_grounding('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', treaty_is_contract_not_constitution, conventional).
narrative_ontology:cs_reference_frame('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', contemporary_globalization_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f3c1ccb0-c66d-4c12-93b6-3a862c93ea3d', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, civil_society_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full regulatory authority over labor, environmental, and health standards within their territory. They use the trade agreement as a coordination mechanism for market access, but not as an overriding constraint on domestic law.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states, agenda_setter,
    institutional, generational, mobile, national).

% Benefit from clear jurisdictional boundaries, allowing them to implement and enforce domestic laws without being overridden by treaty obligations. Their mandate to protect public interest is preserved.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Bear the compliance costs of diverse domestic regulations across different sovereign territories. They cannot easily arbitrage regulatory standards by appealing to treaty supremacy under this reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations, payer,
    powerful, biographical, arbitrage, global).

% Administer the trade agreement text and dispute resolution mechanisms, but their authority is explicitly limited by the primacy of sovereign domestic law in this reading. They facilitate coordination rather than impose harmonization.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_secretariats, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the protection of strong domestic environmental, labor, and health standards, which are safeguarded from challenges based on trade liberalization under this reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, civil_society_organizations, beneficiary,
    organized, generational, mobile, national).

% Interpret the treaty text and its interaction with domestic law, observing the balance of power between international trade obligations and national sovereignty. They advise states and corporations on compliance within this framework.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, international_trade_lawyers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates predictable international trade relations and market access by coordinating expectations among sovereign states, while explicitly preserving their domestic regulatory autonomy in areas like labor, environment, and health.
% TRANSFER_FUNCTION: Primarily transfers compliance costs to multinational corporations (who must adapt to diverse domestic laws) and provides regulatory stability and policy space to sovereign states and their domestic agencies.
% ABSENT_VOICES: Proponents of deeper economic integration, regulatory harmonization, or capital supremacy would object, arguing this reading unduly restricts trade and capital mobility by prioritizing national regulatory diversity over international market efficiency.
% DISAPPEARANCE_RATIONALE: If this understanding of jurisdictional boundaries vanished, states would likely face constant challenges to their domestic laws based on trade obligations, leading to widespread trade disputes, regulatory uncertainty, and potential withdrawal from international agreements, fundamentally reorganizing global trade governance.
% FOUNDING_PROBLEM: To enable international trade and economic cooperation while safeguarding the inherent right of sovereign nations to regulate in the public interest (e.g., labor, environment, health) without fear of treaty override or undue challenge.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, public interest groups, and many national governments attest to the ongoing need for this balance, citing historical instances of trade disputes over regulatory differences and the persistent demand for national policy space in a globalized economy. This corroboration comes from outside the direct beneficiaries of the trade agreement itself.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.25) reflects that, under this reading, the treaty primarily imposes legitimate compliance costs on firms operating across borders, rather than extracting rents from states or their citizens. Suppression (0.15) is low because domestic regulatory authority is preserved, and alternatives (national policy choices) are not suppressed. Theater ratio (0.10) is minimal, as the emphasis is on genuine state authority and functional coordination. The claimed type 'Rope' aligns with this interpretation, as it describes a coordination mechanism that is net beneficial for its participants (sovereign states) and imposes minimal coercive overhead.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between this reading and those that prioritize capital mobility or regulatory harmonization. While this reading sees the arrangement as a beneficial coordination for states, other readings (e.g., 'capital_supremacy_reading') would view it as an impediment to economic efficiency and a source of 'regulatory burden' on capital. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and their domestic regulatory agencies are clear beneficiaries, as their authority is affirmed and protected. Civil society organizations also benefit from the preservation of strong domestic standards. Multinational corporations are payers, bearing the costs of complying with diverse national regulations, but are not 'victims' of the constraint itself, which is seen as a legitimate exercise of state power. Trade secretariats act as agenda-setters, administering the treaty within these defined limits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_practice_fidelity,
    'To what extent does the ''sovereignty primacy'' reading genuinely reflect the actual practice and outcomes of trade dispute resolution and regulatory challenges under NAFTA/USMCA?',
    'Empirical analysis of trade dispute panel rulings, investor-state dispute settlement (ISDS) outcomes, and national court decisions regarding the enforceability of domestic regulations challenged under trade agreements.',
    'If practice frequently overrides domestic law in favor of trade obligations, the effective extractiveness and suppression of this constraint would be higher, potentially reclassifying it towards a Tangled Rope or Snare, indicating a gap between the stated reading and operational reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_practice_fidelity, empirical, 'Assesses the fidelity of the ''sovereignty primacy'' reading to real-world legal and regulatory outcomes.').

omega_variable(
    legitimacy_of_compliance_costs,
    'Are the compliance costs borne by multinational corporations under this reading genuinely ''voluntary'' costs of doing business in diverse regulatory environments, or do they represent a form of implicit extraction due to the sheer scale and complexity of global operations?',
    'Comparative economic analysis of compliance costs in different regulatory regimes, and surveys of multinational corporations regarding the perceived legitimacy and proportionality of these costs versus the benefits of market access.',
    'If compliance costs are found to be disproportionately high or perceived as illegitimate burdens rather than fair costs, the effective extractiveness of the constraint could be re-evaluated upwards, even within this reading''s framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_compliance_costs, conceptual, 'Examines the normative status of compliance costs for multinational corporations under this reading.').

omega_variable(
    structural_delta_sovereignty_primacy,
    'Does this reading''s emphasis on treaty obligations entering the compliance-cost set, rather than overriding constraints, accurately capture the structural delta from other readings?',
    'Comparison with legal scholarship and policy documents advocating for ''capital_supremacy_reading'' and ''embedded_liberalism_reading'' to identify specific legal mechanisms or interpretive principles that would lead to different outcomes regarding regulatory authority.',
    'If the structural delta is less pronounced in practice, the distinction between this ''Rope'' reading and a ''Tangled Rope'' (like ''embedded_liberalism_reading'') might blur, indicating a more complex hybrid function than pure coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_sovereignty_primacy, conceptual, 'Verifies the distinct structural implications of the sovereignty primacy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2006, 0.1).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.2).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.21).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.22).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2012, 0.23).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.24).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.1).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2000, 0.11).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2006, 0.12).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2012, 0.13).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2018, 0.14).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_environmental_regulations).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_standards_enforcement).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, public_health_policy).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'nafta_jurisdictional_boundary' kernel, alongside 'capital_supremacy_reading' and 'embedded_liberalism_reading'. Each reading represents a structurally different constraint with its own ε value and classification, linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
