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
 *   retaliation. It is a specific reading of the 'TRIPS Agreement
 *   Interpretive Kernel,' where the dispute settlement mechanism is seen as
 *   the primary arbiter of meaning, effectively locking in interpretations
 *   through precedent. This reading emphasizes the formal legal power of the
 *   WTO system to shape intellectual property norms globally, particularly
 *   concerning public health flexibilities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.65).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.75).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.65).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '9bb0a550-ac7b-449d-b1bf-3bd350700fd6').
narrative_ontology:cs_kernel_codification('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', fixed_text).
narrative_ontology:cs_authority_grounding('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', lineage).
narrative_ontology:cs_interpretation_layer_present('9bb0a550-ac7b-449d-b1bf-3bd350700fd6').
narrative_ontology:cs_reading_relation('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', trips_agreement_interpretive_kernel__public_health_flexibility_reading, forecloses).
narrative_ontology:cs_axiom('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', foundational, dispute_panel_rulings_binding_precedent).
narrative_ontology:cs_axiom_status(dispute_panel_rulings_binding_precedent, holdable).
narrative_ontology:cs_axiom_grounding('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', dispute_panel_rulings_binding_precedent, conventional).
narrative_ontology:cs_axiom('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', secondary, trade_retaliation_legitimate_enforcement).
narrative_ontology:cs_axiom_status(trade_retaliation_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', trade_retaliation_legitimate_enforcement, conventional).
narrative_ontology:cs_reference_frame('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', rules_based_multilateral_adjudication).
narrative_ontology:cs_drift_state('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', post_appellate_body_collapse_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9bb0a550-ac7b-449d-b1bf-3bd350700fd6', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_ip_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dispute resolution process, including panel formation and ruling enforcement. Its interpretations become binding precedent, shaping the practical application of TRIPS. Faces internal challenges (e.g., Appellate Body collapse) but maintains formal authority.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from panel rulings that reinforce strong intellectual property protections, ensuring market exclusivity and higher profits for their pharmaceutical and technology products. They actively lobby their governments to initiate and support dispute cases.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_ip_holders, beneficiary,
    organized, biographical, arbitrage, global).

% Initiate dispute cases and leverage trade retaliation mechanisms to enforce panel rulings. They represent the interests of their domestic IP-holding industries and actively shape the interpretive environment of TRIPS.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_governments, agenda_setter,
    institutional, generational, mobile, global).

% Bear the costs of adverse panel rulings, which can restrict their ability to implement public health flexibilities (e.g., compulsory licensing, parallel imports). They face the threat of trade sanctions if they do not comply, severely limiting their policy autonomy.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_governments, payer,
    powerful, generational, constrained, global).

% Work to expand access to essential medicines in developing countries. They are negatively impacted by interpretations that strengthen IP exclusivity, as this directly hinders their efforts to secure affordable treatments. They engage in advocacy and legal challenges but have limited direct influence on WTO panel decisions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_advocates, payer,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_ip_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal, rules-based mechanism for resolving disputes between WTO member states regarding the TRIPS Agreement, aiming to ensure predictability and stability in international intellectual property law.
% TRANSFER_FUNCTION: Transfers interpretive authority over TRIPS from individual member states to the WTO dispute settlement body, and potentially transfers economic value (e.g., higher profits for IP holders) from developing countries to developed country IP holders through enforcement of exclusivity.
% ABSENT_VOICES: Many civil society organizations, patient groups, and smaller developing countries lack the resources and legal capacity to fully participate in or influence WTO dispute settlement processes, despite being directly impacted by the outcomes. They would advocate for interpretations that prioritize public health over strict IP enforcement.
% DISAPPEARANCE_RATIONALE: If the WTO dispute settlement body's binding interpretive authority over TRIPS vanished, the international IP regime would fragment. Countries would resort to bilateral negotiations or unilateral actions, leading to increased uncertainty, trade disputes, and a potential breakdown of multilateral IP norms. Developing countries might gain more policy space but face increased bilateral pressure.
% FOUNDING_PROBLEM: The lack of a consistent, enforceable framework for intellectual property rights in international trade led to disputes and uncertainty, hindering innovation and global commerce.
% FOUNDING_PROBLEM_CORROBORATION: Developed countries and IP-holding industries attest that the problem of inconsistent IP enforcement remains live, requiring a strong dispute settlement mechanism. Developing countries and public health advocates acknowledge the need for a framework but contest whether the current mechanism effectively addresses the problem or exacerbates it, citing ongoing access-to-medicines crises. Independent legal scholars and international organizations provide corroboration for both the original problem and the contested status of its resolution.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because panel rulings, often influenced by IP-holding nations, can narrow the scope of public health flexibilities, leading to higher drug prices and reduced access in developing countries. Suppression (0.75) is high due to the binding nature of rulings and the threat of trade retaliation, which severely constrains the policy space for developing countries. The theater ratio (0.20) is relatively low, as the dispute settlement process is a genuinely functional, albeit contested, mechanism for resolving trade disputes, not primarily performative. The increasing extractiveness and suppression over time reflect the hardening of interpretations and the growing pressure on developing countries.
 *
 * PERSPECTIVAL GAP:
 *   Developed country IP holders and the WTO dispute settlement body itself would likely perceive this as a legitimate and necessary coordination mechanism for global trade. Developing country governments and public health advocates, however, experience it as an extractive and suppressive force that prioritizes commercial interests over public health, limiting their sovereign policy choices. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed country IP holders are clear beneficiaries (d=0.0-0.1) as panel rulings often reinforce their exclusive rights. The WTO dispute settlement body also benefits (d=0.1-0.2) by asserting and maintaining its institutional authority. Developing country governments and public health advocates are targets (d=0.8-0.9) as they bear the costs of restricted policy space and higher drug prices. Their exit options are severely constrained by the threat of trade sanctions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to provide a stable, rules-based system for international trade, including IP. However, the 'dispute settlement interpretive authority' reading, particularly when it consistently favors IP exclusivity, risks mandatrophy by shifting from coordination to extraction. The classification as a Tangled Rope acknowledges both the genuine coordination function (providing a dispute resolution mechanism) and the asymmetric extraction (favoring IP holders and constraining developing countries). This prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the WTO dispute settlement body''s interpretive authority over TRIPS a neutral arbiter of a fixed text, or an active shaper of the agreement''s meaning, favoring certain readings?',
    'Analysis of dispute panel rulings over time, specifically examining how often public health flexibilities are upheld versus IP exclusivity claims, and the consistency of interpretation across cases.',
    'If the rulings consistently favor IP exclusivity, it strengthens the ''strong exclusivity'' reading and suggests the dispute settlement mechanism itself is a structural component of extraction. If rulings are balanced, it supports a more neutral ''dispute settlement interpretive authority'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the role of WTO dispute panels as interpreters of the TRIPS Agreement.').

omega_variable(
    bilateral_power_substitution,
    'To what extent have bilateral power dynamics and direct negotiations substituted for multilateral adjudication in resolving TRIPS-related disputes, particularly after the collapse of the Appellate Body?',
    'Empirical study of dispute resolution mechanisms used by developing countries facing IP challenges, comparing formal WTO panel usage with bilateral pressure and negotiation outcomes.',
    'If bilateral power dynamics are increasingly dominant, the effective ''interpretive authority'' of the WTO dispute settlement body diminishes, and the constraint shifts towards a more diffuse, power-based snare for developing countries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_power_substitution, empirical, 'Shift from multilateral adjudication to bilateral power dynamics in TRIPS enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trip_tr_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 10, 0.22).
narrative_ontology:measurement(trip_tr_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(trip_be_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(trip_be_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(trip_su_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(trip_su_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement Interpretive Kernel. Its binding interpretive authority influences and often forecloses other readings by establishing legal precedent and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
