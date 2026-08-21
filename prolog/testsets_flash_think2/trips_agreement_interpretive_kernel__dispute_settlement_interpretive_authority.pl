% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Interpretive Authority over TRIPS
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint describes the binding interpretive authority of WTO
 *   dispute panels over the TRIPS Agreement text, enforced through trade
 *   retaliation mechanisms. It is instantiated as the
 *   'dispute_settlement_interpretive_authority' reading of the
 *   'trips_agreement_interpretive_kernel'. This reading emphasizes the role
 *   of formal adjudication in shaping IP norms, often leading to
 *   interpretations that favor strong IP protection, with significant
 *   implications for public health access. The collapse of the Appellate Body
 *   has introduced substantial drift, shifting enforcement dynamics towards
 *   bilateral power plays.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.85).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '7353cac5-6dbd-4c58-afb1-6419980a38ad').
narrative_ontology:cs_kernel_codification('7353cac5-6dbd-4c58-afb1-6419980a38ad', fixed_text).
narrative_ontology:cs_authority_grounding('7353cac5-6dbd-4c58-afb1-6419980a38ad', extraction).
narrative_ontology:cs_interpretation_layer_present('7353cac5-6dbd-4c58-afb1-6419980a38ad').
narrative_ontology:cs_reading_relation('7353cac5-6dbd-4c58-afb1-6419980a38ad', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('7353cac5-6dbd-4c58-afb1-6419980a38ad', trips_agreement_interpretive_kernel__public_health_flexibility_reading, forecloses).
narrative_ontology:cs_axiom('7353cac5-6dbd-4c58-afb1-6419980a38ad', foundational, binding_interpretive_precedent).
narrative_ontology:cs_axiom_status(binding_interpretive_precedent, holdable).
narrative_ontology:cs_axiom_grounding('7353cac5-6dbd-4c58-afb1-6419980a38ad', binding_interpretive_precedent, conventional).
narrative_ontology:cs_axiom('7353cac5-6dbd-4c58-afb1-6419980a38ad', secondary, trade_retaliation_legitimacy).
narrative_ontology:cs_axiom_status(trade_retaliation_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7353cac5-6dbd-4c58-afb1-6419980a38ad', trade_retaliation_legitimacy, conventional).
narrative_ontology:cs_reference_frame('7353cac5-6dbd-4c58-afb1-6419980a38ad', multilateral_rules_based_order).
narrative_ontology:cs_drift_state('7353cac5-6dbd-4c58-afb1-6419980a38ad', post_appellate_body_collapse, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7353cac5-6dbd-4c58-afb1-6419980a38ad', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_countries_pharma).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_panels).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_countries_public_health).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, local_generic_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multilateral_trading_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These panels interpret the TRIPS Agreement text and issue binding rulings. Their authority is central to the WTO's enforcement mechanism, but their legitimacy is increasingly challenged, especially after the Appellate Body's paralysis. They benefit from the system's continued operation and their role in shaping trade law.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_panels, agenda_setter,
    institutional, biographical, constrained, global).

% These entities benefit from strong, uniformly enforced intellectual property rights, which the dispute settlement system has historically upheld. They leverage panel rulings to protect market exclusivity and challenge compulsory licensing or parallel import schemes. They have significant influence in shaping trade policy and legal interpretations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_countries_pharma, beneficiary,
    organized, generational, mobile, global).

% These countries and advocates bear the costs of restrictive IP interpretations, facing trade retaliation if they implement public health flexibilities that are deemed non-compliant. Their ability to provide affordable medicines is constrained by these rulings, and their exit options are limited due to economic dependency on the global trading system.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_countries_public_health, payer,
    powerless, generational, trapped, global).

% These manufacturers face direct competition from patented drugs and are often the target of legal challenges initiated by developed country pharmaceutical companies. Their ability to produce and distribute affordable generic medicines is directly impacted by the interpretive authority of WTO panels, limiting their market access and operational scope.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, local_generic_manufacturers, payer,
    moderate, biographical, constrained, regional).

% Once the final arbiter of WTO disputes, the Appellate Body has been paralyzed since 2019 due to blockages in member appointments. Its absence means panel rulings are often appealed 'into the void,' undermining the multilateral system's binding nature and predictability. It is now a structurally excluded actor, unable to fulfill its intended function.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_appellate_body, excluded,
    institutional, generational, trapped, global).

% The system as a whole benefits from the perceived stability and predictability that a binding dispute settlement mechanism provides, even if its current state is contested. However, the paralysis of the Appellate Body and the rise of bilateral power dynamics threaten its long-term viability and legitimacy.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multilateral_trading_system, beneficiary,
    institutional, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for resolving disputes among WTO members regarding the interpretation and application of the TRIPS Agreement, aiming to ensure a predictable and rules-based international trade environment for intellectual property.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual member states to WTO dispute panels, and potentially transfers economic benefits (e.g., market exclusivity, higher drug prices) from developing countries and their populations to intellectual property holders in developed countries, enforced by the threat of trade sanctions.
% ABSENT_VOICES: Many public health organizations, patient advocacy groups, and civil society organizations are excluded from direct participation in WTO dispute settlement, though their concerns are sometimes represented by developing country governments. They would argue for interpretations that prioritize public health over strict IP enforcement.
% DISAPPEARANCE_RATIONALE: If the WTO dispute settlement's binding interpretive authority over TRIPS vanished, it would lead to a fragmentation of international IP enforcement. Developed countries would likely resort to more aggressive bilateral trade measures to protect IP, while developing countries would have greater (though still contested) freedom to implement public health flexibilities, leading to a significant reorganization of global IP governance and trade relations.
% FOUNDING_PROBLEM: The TRIPS Agreement was established to harmonize global intellectual property standards and provide a multilateral mechanism for resolving disputes, aiming to reduce trade friction caused by disparate national IP regimes and ensure predictable market access for IP-intensive industries.
% FOUNDING_PROBLEM_CORROBORATION: Developed countries and pharmaceutical industries attest that the problem of IP protection and enforcement remains live, requiring strong multilateral mechanisms. Developing countries and public health advocates, supported by independent legal scholars and economists, argue that while harmonization was achieved, the system's current operation has shifted to prioritize IP holder interests over public health, making the original problem's 'solution' itself a source of new problems.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) due to the significant economic costs imposed by trade retaliation and the market exclusivity granted by strong IP interpretations. Suppression is also high (0.85) because the binding nature of rulings and the threat of sanctions severely limit member states' policy space for alternative IP regimes. Theater ratio is low (0.1) as the dispute settlement process is genuinely functional, though its outcomes are contested. Accessibility collapse is high (0.75) as there are few effective alternatives to WTO dispute settlement for resolving TRIPS-related conflicts. Resistance is moderate (0.6) from developing countries and public health advocates, but often insufficient to alter core interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of developed countries and pharmaceutical companies, this constraint provides essential stability and predictability for IP rights, fostering innovation. From the perspective of developing countries and public health advocates, it functions as an extractive mechanism that prioritizes corporate profits over access to essential medicines, enforced through coercive trade measures. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed countries and pharmaceutical companies are primary beneficiaries, gaining from strong IP enforcement. WTO dispute panels, as institutional actors, also benefit from the system's continued operation and their central role. Developing countries and local generic manufacturers are victims, bearing the costs of restricted policy space and market access. The multilateral trading system itself is a beneficiary of the rules-based order, but also faces constraints from its own internal dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide a stable, rules-based system for IP has not atrophied, but its function has arguably drifted. While it still coordinates IP norms, the asymmetric enforcement and interpretive outcomes suggest it has become a Tangled Rope, where the coordination story covers substantial extraction. The paralysis of the Appellate Body further complicates this, as the system's ability to provide legitimate, multilateral interpretations is compromised, potentially leading to a more Snare-like operation through bilateral power dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint primarily an interpretive authority mechanism, or is it better understood as a direct enforcement of strong IP exclusivity?',
    'Analysis of panel rulings: if rulings consistently prioritize IP holder rights over public health flexibilities, it leans towards direct enforcement of exclusivity. If rulings show a balanced approach, it supports the interpretive authority framing.',
    'If primarily direct enforcement, the extractiveness and suppression metrics might be even higher, and the coordination function less salient, potentially reclassifying it closer to a Snare. If balanced, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifies the primary function of the dispute settlement mechanism within the TRIPS kernel.').

omega_variable(
    appellate_body_collapse_impact,
    'To what extent has the paralysis of the WTO Appellate Body shifted the effective interpretive authority from multilateral adjudication to bilateral power dynamics?',
    'Empirical study of post-2019 trade disputes: track the frequency and outcomes of bilateral trade threats/sanctions related to TRIPS, compared to formal WTO panel rulings.',
    'If bilateral power dynamics are now dominant, the constraint''s effective suppression and extractiveness for developing countries would increase, as they face direct pressure without multilateral recourse. This would push the classification closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_body_collapse_impact, empirical, 'Assesses the impact of the Appellate Body''s paralysis on TRIPS enforcement and interpretation.').

omega_variable(
    balance_of_ip_and_public_health,
    'Is the current interpretation of TRIPS by dispute panels achieving an appropriate balance between incentivizing innovation and ensuring access to essential medicines, as intended by the Doha Declaration on TRIPS and Public Health?',
    'Comprehensive review by an independent international commission, considering public health outcomes, innovation metrics, and economic impacts across diverse countries.',
    'If the balance is found to be skewed, it would strengthen calls for reform or alternative interpretations, potentially reducing the perceived legitimacy of the current constraint and increasing resistance. If balanced, it would reinforce the current structure''s perceived fairness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balance_of_ip_and_public_health, preference, 'Evaluates the normative balance struck by TRIPS interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(trip_tr_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2000, 0.06).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2005, 0.07).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2015, 0.09).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(trip_be_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(trip_su_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2005, 0.78).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement kernel, focusing on the binding interpretive authority of WTO dispute panels. It directly influences the practical application and contestation of other readings of the TRIPS text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
