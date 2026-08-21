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
 *   NAFTA jurisdictional boundary, where trade agreement text is understood
 *   as a coordination mechanism subordinate to sovereign domestic law. Under
 *   this reading, states explicitly retain full regulatory authority over
 *   labor, environmental, and health standards within their territory. Treaty
 *   obligations are seen as entering the compliance-cost set for businesses
 *   but not as overriding constraints on national regulatory agencies, which
 *   retain full jurisdictional authority. Extraction is limited to voluntary
 *   compliance costs associated with participating in the trade regime,
 *   rather than a forced surrender of regulatory space.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.1).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary (Sovereignty Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '33e85ede-ee97-497b-88ec-c1c22de87a84').
narrative_ontology:cs_kernel_codification('33e85ede-ee97-497b-88ec-c1c22de87a84', fixed_text).
narrative_ontology:cs_authority_grounding('33e85ede-ee97-497b-88ec-c1c22de87a84', lineage).
narrative_ontology:cs_interpretation_layer_present('33e85ede-ee97-497b-88ec-c1c22de87a84').
narrative_ontology:cs_reading_relation('33e85ede-ee97-497b-88ec-c1c22de87a84', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('33e85ede-ee97-497b-88ec-c1c22de87a84', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('33e85ede-ee97-497b-88ec-c1c22de87a84', foundational, domestic_regulatory_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(domestic_regulatory_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('33e85ede-ee97-497b-88ec-c1c22de87a84', domestic_regulatory_autonomy_is_paramount, deontological).
narrative_ontology:cs_axiom('33e85ede-ee97-497b-88ec-c1c22de87a84', foundational, trade_agreements_are_subordinate_to_national_law).
narrative_ontology:cs_axiom_status(trade_agreements_are_subordinate_to_national_law, holdable).
narrative_ontology:cs_axiom_grounding('33e85ede-ee97-497b-88ec-c1c22de87a84', trade_agreements_are_subordinate_to_national_law, conventional).
narrative_ontology:cs_reference_frame('33e85ede-ee97-497b-88ec-c1c22de87a84', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('33e85ede-ee97-497b-88ec-c1c22de87a84', contemporary_globalization_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('33e85ede-ee97-497b-88ec-c1c22de87a84', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states).
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

% Retain full authority to set and enforce domestic labor, environmental, and health standards, viewing trade agreements as voluntary coordination mechanisms that do not diminish their regulatory space. They benefit from market access without ceding policy autonomy.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_states, beneficiary,
    institutional, generational, mobile, national).

% Operate under the clear understanding that their mandates for public welfare, environmental protection, and labor rights are not superseded by international trade obligations. They face compliance costs but not jurisdictional challenges from trade agreements.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    organized, biographical, mobile, national).

% Must comply with diverse and potentially stringent domestic regulations in each sovereign state, incurring compliance costs that cannot be easily circumvented by invoking trade agreement provisions. They pay the cost of adapting to varied national standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_corporations, payer,
    powerful, biographical, constrained, global).

% Draft and interpret trade agreements with the explicit understanding that domestic sovereignty over non-trade regulatory matters is preserved. Their role is to facilitate trade, not to harmonize or override national laws.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_negotiators, agenda_setter,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate international trade by establishing predictable rules for market access, tariffs, and customs procedures, while explicitly preserving the regulatory autonomy of sovereign states over domestic policy areas.
% TRANSFER_FUNCTION: Primarily transfers market access and economic predictability among member states, with minimal transfer of regulatory authority or policy space from states to the international trade regime.
% ABSENT_VOICES: Advocates for deeper economic integration and regulatory harmonization, who would argue for trade agreements to have greater preemptive power over domestic law, are marginalized in this reading.
% DISAPPEARANCE_RATIONALE: If this understanding of jurisdictional boundaries vanished, states would likely revert to more protectionist trade policies, fearing erosion of sovereignty, leading to a fragmentation of global trade rules and increased friction in international commerce.
% FOUNDING_PROBLEM: To create a framework for stable and predictable trade relations between nations without infringing on their fundamental right to govern domestic affairs, particularly in areas of public welfare and environmental protection.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and public interest groups consistently corroborate the ongoing need to balance trade liberalization with sovereign regulatory space, citing numerous instances where this balance is challenged. National legislative bodies also frequently reaffirm this principle in their domestic lawmaking.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because this reading emphasizes the voluntary nature of trade agreement participation and the retention of domestic regulatory power. Suppression is also low (0.1) as states are not coerced into ceding sovereignty. The theater ratio is minimal (0.05) because the stated function of preserving sovereignty is genuinely upheld. Accessibility collapse is low (0.2) as states retain robust alternatives for domestic regulation. Resistance is low (0.05) because this reading aligns with the interests of states in maintaining their regulatory autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states and their regulatory agencies, this constraint is a pure Rope, facilitating trade while protecting essential domestic policy space. From the perspective of multinational corporations, it imposes compliance costs due to varied national regulations, but they acknowledge the legitimacy of national sovereignty in this reading. Other readings (e.g., capital_supremacy_reading) would present a much higher extractiveness and suppression from the state's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and domestic regulatory agencies are clear beneficiaries, as their authority is affirmed and protected. Multinational corporations are payers, bearing the costs of complying with diverse national regulations. Trade negotiators act as agenda-setters, crafting agreements that reflect this balance. There are no direct 'victims' in this reading, as the core principle is non-extraction of sovereignty.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine coordination (facilitating trade without overriding sovereignty) as extraction. By explicitly defining the jurisdictional boundary, it clarifies that the mandate is to coordinate trade, not to impose regulatory harmonization. The low extractiveness and suppression reflect a healthy, functional coordination mechanism where the mandate remains live and uncorrupted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_adherence_vs_practice,
    'To what extent does actual state practice and judicial interpretation consistently adhere to this ''sovereignty primacy'' reading, versus drifting towards other interpretations?',
    'Empirical analysis of investor-state dispute settlement (ISDS) outcomes, national court rulings on trade-related challenges to domestic law, and legislative debates on trade agreement implementation.',
    'If practice consistently deviates, this reading may be overridden by a ''capital supremacy'' or ''embedded liberalism'' reading, leading to higher effective extraction from states. If adherence is strong, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_adherence_vs_practice, empirical, 'Assesses the gap between the declared reading and real-world application.').

omega_variable(
    conceptual_framing_contest,
    'Is this ''sovereignty primacy'' reading a genuinely distinct and coherent interpretation, or is it a rhetorical position within a broader contest over the ''embedded liberalism'' or ''capital supremacy'' framings?',
    'Conceptual analysis of legal scholarship and policy discourse, identifying unique axiomatic foundations and consistent logical implications that distinguish it from sibling readings.',
    'If it is merely a rhetorical variant, its independent classification as a Rope might be unstable, and the underlying constraint could be reclassified as a Tangled Rope or Snare under a more dominant reading. If it is robustly distinct, its classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_framing_contest, conceptual, 'Examines the conceptual integrity and independence of this specific reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.03).
narrative_ontology:measurement(naft_tr_t2004, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2004, 0.04).
narrative_ontology:measurement(naft_tr_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2014, 0.05).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.1).
narrative_ontology:measurement(naft_be_t2004, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2004, 0.12).
narrative_ontology:measurement(naft_be_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2014, 0.14).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.08).
narrative_ontology:measurement(naft_su_t2004, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2004, 0.09).
narrative_ontology:measurement(naft_su_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2014, 0.1).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2024, 0.1).


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
