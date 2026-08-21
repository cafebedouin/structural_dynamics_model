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
 *   This constraint story represents the 'sovereignty primacy' reading of the
 *   NAFTA jurisdictional boundary, where the trade agreement functions as a
 *   coordination mechanism subordinate to sovereign domestic law. Under this
 *   reading, states retain full regulatory authority over labor,
 *   environmental, and health standards within their territory. Treaty
 *   obligations are understood as entering the compliance-cost set for states
 *   and corporations, but not as overriding constraints on national
 *   regulatory agencies. Extraction is limited to voluntary compliance costs
 *   incurred by parties seeking trade benefits, rather than coercive
 *   imposition of harmonized standards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.12).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary (Sovereignty Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'cb71a8d5-a056-4e36-b055-a68b2e2d36dc').
narrative_ontology:cs_kernel_codification('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', fixed_text).
narrative_ontology:cs_authority_grounding('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', lineage).
narrative_ontology:cs_interpretation_layer_present('cb71a8d5-a056-4e36-b055-a68b2e2d36dc').
narrative_ontology:cs_reading_relation('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', foundational, domestic_law_supremacy).
narrative_ontology:cs_axiom_status(domestic_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', domestic_law_supremacy, conventional).
narrative_ontology:cs_axiom('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', foundational, regulatory_autonomy_of_states).
narrative_ontology:cs_axiom_status(regulatory_autonomy_of_states, holdable).
narrative_ontology:cs_axiom_grounding('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', regulatory_autonomy_of_states, conventional).
narrative_ontology:cs_reference_frame('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cb71a8d5-a056-4e36-b055-a68b2e2d36dc', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, citizens_public).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations_seeking_harmonization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full regulatory authority over domestic labor, environmental, and health standards. They benefit from trade access while preserving policy space. They voluntarily incur compliance costs for trade benefits, but are not compelled to alter core domestic law.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states, agenda_setter,
    institutional, generational, mobile, national).

% Maintain their mandates and jurisdictional authority without being overridden by international trade tribunals. They continue to set and enforce standards in the public interest.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    organized, biographical, constrained, national).

% Benefit from the protection of domestic labor, environmental, and health standards, which are not diluted by trade agreements. They also benefit from increased trade and economic activity.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, citizens_public, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of complying with diverse national regulatory standards, which are not harmonized or overridden by the trade agreement. They would prefer a regime where trade law is supreme, reducing their compliance burden across borders.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations_seeking_harmonization, payer,
    powerful, biographical, constrained, global).

% Facilitate trade agreements and monitor compliance, but under this reading, they respect the primacy of national sovereignty in regulatory matters. Their role is to mediate, not to impose overriding standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, international_trade_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate international trade and economic cooperation among member states by establishing common rules and reducing tariffs, while explicitly preserving each state's sovereign right to regulate within its borders.
% TRANSFER_FUNCTION: Primarily transfers market access and economic opportunities among member states. It also transfers compliance costs to multinational corporations who must adapt to diverse national regulations, rather than a harmonized, lower standard.
% ABSENT_VOICES: Advocates for 'regulatory harmonization' or 'investor-state dispute settlement' mechanisms that would override domestic law are structurally marginalized by this reading. They would argue for greater efficiency and predictability for capital, but their claims are subordinated to national policy space.
% DISAPPEARANCE_RATIONALE: If this reading vanished, trade agreements would likely be interpreted as having supremacy over domestic law, leading to a fundamental shift in national regulatory authority, increased investor-state litigation, and a race to the bottom in standards. The global regulatory landscape would be profoundly altered.
% FOUNDING_PROBLEM: The challenge of fostering economic integration and reducing trade barriers without eroding national sovereignty and the ability of states to protect their citizens and environments through domestic regulation.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, constitutional lawyers, and public interest groups consistently attest to the ongoing tension between trade liberalization and sovereign regulatory authority. This corroboration comes from outside the direct beneficiaries of trade agreements.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness (0.18) and suppression (0.12) reflect the core premise of this reading: states voluntarily enter the agreement for mutual benefit, retaining their fundamental regulatory autonomy. The costs incurred are primarily compliance costs, not rents extracted through coercion. Theater ratio is low (0.08) as the coordination function is genuine and not primarily performative. Accessibility collapse is low (0.25) because states retain the option to withdraw or renegotiate, and domestic regulatory alternatives are not suppressed. Resistance is low (0.10) because this reading aligns with the interests of sovereign states and their publics.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states and their citizens, this reading of NAFTA is a successful coordination mechanism that balances trade with national interests. From the perspective of multinational corporations seeking regulatory harmonization, it represents a missed opportunity for efficiency and a continued burden of diverse compliance costs. The engine's per-seat classification would reflect this divergence, with states computing as beneficiaries of a Rope, and corporations as payers of a low-extraction Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states, domestic regulatory agencies, and citizens are beneficiaries (low d) as they gain trade access while preserving regulatory autonomy and public protections. Multinational corporations seeking harmonization are payers (higher d) as they bear the costs of complying with diverse national standards, which this reading prevents from being overridden. International trade organizations are observers, facilitating the framework without being direct beneficiaries or targets of extraction under this specific reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine coordination (trade facilitation) as pure extraction by emphasizing the voluntary nature of compliance and the retained sovereign authority. It highlights that any 'extraction' is primarily the cost of doing business across diverse regulatory regimes, not a coercive imposition. The 'live' status of the founding problem (balancing trade with sovereignty) further supports that the mandate has not atrophied under this interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_prevalence_ambiguity,
    'To what extent is the ''sovereignty primacy'' reading actually dominant in practice, versus being a normative ideal or a contested interpretation?',
    'Empirical analysis of judicial decisions, regulatory agency actions, and legislative debates in member states; content analysis of official statements and legal scholarship.',
    'If this reading is less prevalent in practice, the effective extractiveness and suppression of the actual operating constraint would be higher, potentially shifting its classification towards a Tangled Rope or Snare, as domestic regulatory space is de facto eroded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_prevalence_ambiguity, empirical, 'The actual operational dominance of the sovereignty primacy reading.').

omega_variable(
    voluntary_compliance_vs_structural_pressure,
    'Are the ''voluntary compliance costs'' truly voluntary, or do they represent structural pressure on states to avoid trade disputes or capital flight, even when asserting sovereignty?',
    'Case studies of states'' decisions regarding regulatory changes in the face of potential trade challenges; analysis of the economic impact of non-compliance with treaty-implied standards.',
    'If ''voluntary'' compliance is driven by significant structural pressure, the effective extractiveness and suppression would be higher, as states face implicit coercion to align regulations, even if not explicitly overridden by treaty text. This would push the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_vs_structural_pressure, conceptual, 'The true nature of ''voluntary'' compliance costs.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''nafta_jurisdictional_boundary'' kernel best framed as a legal text, or as a set of evolving practices and power relations?',
    'Comparative analysis of legal-positivist vs. critical legal studies interpretations of international trade law, assessing which framing better explains observed outcomes and contestation dynamics.',
    'If framed as evolving practices and power relations, the ''sovereignty primacy'' reading might appear less stable or more vulnerable to ''practice_drift'' than when framed as a fixed legal text, potentially altering its long-term classification trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the NAFTA jurisdictional boundary kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.2).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.19).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.18).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2012, 0.17).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.17).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.15).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2006, 0.13).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2012, 0.12).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2018, 0.12).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
