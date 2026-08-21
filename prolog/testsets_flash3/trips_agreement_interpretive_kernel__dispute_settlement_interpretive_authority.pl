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
 *   dispute panels over the TRIPS Agreement, enforced through trade
 *   retaliation. It is one reading of the broader 'TRIPS Agreement
 *   Interpretive Kernel,' focusing on how the dispute settlement mechanism
 *   concretizes specific interpretations, often favoring strong IP
 *   protections. The collapse of the Appellate Body has further concentrated
 *   interpretive power at the panel level and increased the role of bilateral
 *   power dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.75).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'c1ddbd4b-999f-4af8-93ec-a4f5db986f08').
narrative_ontology:cs_kernel_codification('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', fixed_text).
narrative_ontology:cs_authority_grounding('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', lineage).
narrative_ontology:cs_interpretation_layer_present('c1ddbd4b-999f-4af8-93ec-a4f5db986f08').
narrative_ontology:cs_reading_relation('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', foundational, dispute_panel_rulings_are_binding_precedent).
narrative_ontology:cs_axiom_status(dispute_panel_rulings_are_binding_precedent, holdable).
narrative_ontology:cs_axiom_grounding('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', dispute_panel_rulings_are_binding_precedent, conventional).
narrative_ontology:cs_axiom('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', secondary, trade_retaliation_is_legitimate_enforcement).
narrative_ontology:cs_axiom_status(trade_retaliation_is_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', trade_retaliation_is_legitimate_enforcement, conventional).
narrative_ontology:cs_reference_frame('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', multilateral_rules_based_order).
narrative_ontology:cs_drift_state('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', post_appellate_body_collapse_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c1ddbd4b-999f-4af8-93ec-a4f5db986f08', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_ip_lobby_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates disputes over TRIPS, issuing binding rulings that interpret the agreement's text. Its rulings establish precedents that shape future trade law and can authorize trade retaliation against non-compliant members. The collapse of the Appellate Body has shifted power to panels and bilateral negotiations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from strong intellectual property protections enforced through the WTO system, which secures their market exclusivity for patented medicines. They actively lobby for interpretations that favor strong IP rights and challenge national policies that seek to use TRIPS flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_pharmaceutical_firms, beneficiary,
    organized, biographical, mobile, global).

% Bear the costs of high pharmaceutical prices due to strong IP protections. They seek to utilize TRIPS flexibilities (like compulsory licensing) to ensure access to essential medicines, but face legal challenges and potential trade retaliation if their interpretations are deemed non-compliant by dispute panels.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_countries, payer,
    moderate, generational, constrained, national).

% Work to expand access to medicines in developing countries. They are structurally disadvantaged in WTO dispute processes, which prioritize trade law over public health outcomes. Their 'exit' is often a shift to extra-WTO advocacy or direct negotiation with pharmaceutical firms.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_advocates, payer,
    powerless, generational, identity_locked, global).

% Represent the interests of industries that benefit from robust intellectual property rights. They influence the interpretive environment of TRIPS by funding legal analysis, engaging in public relations, and directly lobbying governments involved in disputes.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_ip_lobby_groups, beneficiary,
    organized, biographical, arbitrage, global).

% Advocate for public health priorities but have limited formal standing in WTO dispute settlement. Their influence is indirect, through advising developing countries or issuing reports that highlight the public health impact of TRIPS interpretations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, international_public_health_organizations, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a multilateral forum for resolving disputes over intellectual property rights in trade, aiming to ensure a predictable and enforceable global IP regime for all WTO members.
% TRANSFER_FUNCTION: Transfers interpretive authority over TRIPS text to WTO dispute panels, which can lead to the transfer of economic value (e.g., higher pharmaceutical prices) from developing countries to IP holders through enforcement mechanisms.
% ABSENT_VOICES: International public health organizations and civil society groups advocating for access to medicines are largely excluded from formal dispute proceedings, where their arguments for public health flexibilities are often marginalized in favor of trade law interpretations.
% DISAPPEARANCE_RATIONALE: If the WTO's interpretive authority over TRIPS vanished, countries would likely revert to bilateral IP negotiations, leading to a fragmented and less predictable global IP landscape. Developing countries might gain more flexibility, but also face increased pressure from powerful nations outside a multilateral framework.
% FOUNDING_PROBLEM: Before TRIPS, intellectual property rights were inconsistently protected across countries, leading to trade friction and uncertainty for innovators. The agreement aimed to establish minimum global standards and a mechanism for dispute resolution.
% FOUNDING_PROBLEM_CORROBORATION: Developed countries and pharmaceutical firms argue the problem of inconsistent IP protection remains live, necessitating strong interpretive authority. Developing countries and public health advocates argue the founding problem is largely solved, and the current interpretive regime has shifted to prioritize IP holder interests over public health, as evidenced by numerous disputes over access to medicines.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) reflects the economic costs borne by developing countries due to interpretations that limit public health flexibilities. Suppression (0.75) is high because the binding nature of panel rulings and the threat of trade retaliation severely constrain developing countries' policy space. Theater ratio (0.20) is relatively low, as the dispute settlement process is genuinely functional in resolving trade disputes, though its outcomes are often contested. The increasing extractiveness and suppression over time reflect the hardening of IP interpretations and the growing pressure on developing countries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of IP holders, the dispute settlement mechanism is a necessary coordination function for global trade, ensuring fair competition and incentivizing innovation. From the perspective of developing countries and public health advocates, it is an extractive mechanism that prioritizes corporate profits over public health, enforced through a biased interpretive framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed country pharmaceutical firms and IP lobby groups are clear beneficiaries, as the interpretive authority reinforces their market exclusivity. Developing countries and public health advocates are victims, bearing the costs of restricted access to medicines. The WTO Dispute Settlement Body acts as an agenda-setter, its institutional power shaping the interpretive landscape. International public health organizations are excluded, lacking direct standing in the process.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has drifted from its original intent of balancing IP protection with public health needs (as articulated in the Doha Declaration on TRIPS and Public Health). While the founding problem of inconsistent IP protection is largely addressed, the interpretive authority now primarily serves to enforce a strong IP regime, leading to a 'tangled rope' classification where coordination for some (IP holders) comes with significant extraction from others (developing countries).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appellate_body_collapse_impact,
    'How has the collapse of the WTO Appellate Body altered the interpretive authority and power dynamics within TRIPS dispute settlement?',
    'Empirical analysis of post-Appellate Body panel rulings and subsequent bilateral trade negotiations, comparing outcomes to pre-collapse trends.',
    'If the collapse has led to more fragmented, less consistent interpretations or increased bilateral pressure, the constraint''s effective suppression and extractiveness may be higher due to reduced multilateral oversight and increased power asymmetries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_body_collapse_impact, empirical, 'Impact of Appellate Body collapse on TRIPS interpretive authority.').

omega_variable(
    interpretive_bias_vs_neutrality,
    'Is the WTO dispute settlement body''s interpretation of TRIPS text genuinely neutral, or does it exhibit a systemic bias towards strong IP protection?',
    'Content analysis of all TRIPS-related panel reports, examining the legal reasoning, precedents cited, and outcomes in cases involving public health flexibilities versus IP enforcement.',
    'If a systemic bias is confirmed, the constraint''s ''tangled rope'' classification would lean more towards ''snare'' for developing countries, as the coordination function would be revealed as cover for extraction. If neutrality is upheld, the coordination aspect is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_bias_vs_neutrality, conceptual, 'Systemic bias in TRIPS dispute panel interpretations.').

omega_variable(
    bilateral_power_dynamics_influence,
    'To what extent do bilateral power dynamics (e.g., between a developed country and a developing country) influence the outcome of TRIPS disputes, even within the multilateral framework?',
    'Case studies of specific TRIPS disputes, analyzing the role of diplomatic pressure, trade threats outside the WTO, and economic leverage in shaping dispute outcomes or pre-empting formal challenges.',
    'If bilateral power is a dominant factor, the multilateral interpretive authority functions more as a legitimizing cover for pre-determined outcomes, increasing the effective suppression and extractiveness for weaker parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_power_dynamics_influence, empirical, 'Influence of bilateral power on TRIPS dispute outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(trip_tr_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(trip_be_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(trip_su_t2000, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, global_pharmaceutical_supply_chains).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, national_public_health_policy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'TRIPS Agreement Interpretive Kernel'. Its interpretive authority influences the viability and contestation of other readings, such as the 'strong_exclusivity_reading' and 'public_health_flexibility_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
