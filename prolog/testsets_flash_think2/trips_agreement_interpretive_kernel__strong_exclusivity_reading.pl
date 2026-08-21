% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Agreement: Strong Exclusivity Reading
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint represents the 'strong exclusivity' reading of the TRIPS
 *   Agreement, which mandates high uniform patent protections globally with
 *   narrow interpretations of flexibilities for public health. This reading
 *   prioritizes the rights of pharmaceutical patent holders, viewing robust
 *   intellectual property as the primary driver for innovation. It stands in
 *   contrast to readings that emphasize public health flexibilities. The
 *   authored metrics reflect the operational reality of this interpretation,
 *   which is highly extractive and actively enforced, despite being claimed
 *   as a coordination mechanism for innovation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Agreement: Strong Exclusivity Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'a81de471-5c26-482b-b875-c6018efb5c8d').
narrative_ontology:cs_kernel_codification('a81de471-5c26-482b-b875-c6018efb5c8d', fixed_text).
narrative_ontology:cs_authority_grounding('a81de471-5c26-482b-b875-c6018efb5c8d', lineage).
narrative_ontology:cs_interpretation_layer_present('a81de471-5c26-482b-b875-c6018efb5c8d').
narrative_ontology:cs_reading_relation('a81de471-5c26-482b-b875-c6018efb5c8d', trips_agreement_interpretive_kernel__public_health_flexibility_reading, forecloses).
narrative_ontology:cs_reading_relation('a81de471-5c26-482b-b875-c6018efb5c8d', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('a81de471-5c26-482b-b875-c6018efb5c8d', foundational, ip_as_private_property_right).
narrative_ontology:cs_axiom_status(ip_as_private_property_right, holdable).
narrative_ontology:cs_axiom_grounding('a81de471-5c26-482b-b875-c6018efb5c8d', ip_as_private_property_right, deontological).
narrative_ontology:cs_axiom('a81de471-5c26-482b-b875-c6018efb5c8d', foundational, strong_ip_drives_innovation).
narrative_ontology:cs_axiom_status(strong_ip_drives_innovation, holdable).
narrative_ontology:cs_axiom_grounding('a81de471-5c26-482b-b875-c6018efb5c8d', strong_ip_drives_innovation, empirically_contingent).
narrative_ontology:cs_reference_frame('a81de471-5c26-482b-b875-c6018efb5c8d', unfettered_patent_exclusivity).
narrative_ontology:cs_drift_state('a81de471-5c26-482b-b875-c6018efb5c8d', post_doha_declaration_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a81de471-5c26-482b-b875-c6018efb5c8d', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and benefits from strong, uniform patent protections, ensuring market exclusivity for their products. They actively participate in shaping interpretations of TRIPS that favor their interests and leverage dispute settlement mechanisms to enforce these interpretations.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Their domestic industries, particularly pharmaceutical companies, benefit significantly from the global enforcement of strong IP rights. They support interpretations that maximize patent holder rights and minimize flexibilities for public health.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations, beneficiary,
    institutional, generational, mobile, global).

% Bear the cost of high drug prices due to patent monopolies, limiting access to essential medicines for their populations. They face significant pressure and potential trade sanctions if they attempt to broadly utilize TRIPS flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    organized, generational, trapped, global).

% Are the ultimate victims, often unable to afford life-saving patented medicines. Their access to affordable generic alternatives is severely restricted by the strong exclusivity interpretation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries, payer,
    powerless, immediate, trapped, global).

% Are prevented from producing and distributing affordable generic versions of patented drugs, losing market share and hindering their ability to contribute to public health. They operate under constant threat of legal challenge.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, payer,
    organized, biographical, constrained, global).

% Serve as the adjudicators of TRIPS disputes. This reading influences their interpretations, often leading to rulings that reinforce strong patent protections and narrowly construe public health flexibilities, backed by the threat of trade retaliation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, biographical, analytical, global).

% Actively campaign for broader interpretation of TRIPS flexibilities to prioritize public health. While they influence public opinion and policy debates, their direct voice in WTO dispute settlement is limited, and their concerns are often marginalized by the strong exclusivity reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global framework for intellectual property rights, aiming to incentivize pharmaceutical research and development by guaranteeing market exclusivity and preventing counterfeiting.
% TRANSFER_FUNCTION: Transfers significant economic value from healthcare systems and patients in low-income states to pharmaceutical patent holders, primarily located in developed nations, through high prices for patented medicines.
% ABSENT_VOICES: Public health advocates, patient groups, and generic drug manufacturers from developing countries are often marginalized in the formal interpretation and enforcement processes, despite being directly impacted. Their arguments for broader flexibilities are systematically downplayed by this reading.
% DISAPPEARANCE_RATIONALE: If the TRIPS agreement and its strong exclusivity interpretation vanished overnight, the global intellectual property regime would fragment. This would likely lead to diverse national patent laws, a rapid increase in generic drug production, significantly lower drug prices, and a fundamental reorganization of pharmaceutical R&D incentives and funding models, potentially shifting towards public funding or prize systems.
% FOUNDING_PROBLEM: The founding problem was perceived as a lack of uniform global intellectual property protection, which was argued to hinder innovation, encourage counterfeiting, and distort trade in knowledge-intensive goods, particularly pharmaceuticals.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical industry and developed nations assert that the problem of insufficient innovation incentive and counterfeiting remains live. However, public health groups and many developing nations argue that the original problem has largely been addressed, and the current strong exclusivity regime now creates new, more severe problems related to access to essential medicines. Independent economic analyses often support the latter view, indicating that the incentive structure primarily serves profitable markets, not global health needs.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading enables pharmaceutical companies to maintain high prices and market monopolies, transferring wealth from patients and healthcare systems. Suppression is also high (0.78) due to the active enforcement mechanisms of the WTO dispute settlement system, which can impose trade sanctions on states attempting to broadly utilize flexibilities. Accessibility collapse is severe (0.88) for generic alternatives. Resistance is substantial (0.70) from public health advocates and developing nations. Theater ratio is moderate (0.45) as the narrative of 'incentivizing innovation' often serves to justify rent-seeking behavior, though some genuine coordination for IP protection exists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pharmaceutical patent holders and developed nations, this reading of TRIPS is a necessary 'rope' for global innovation and fair trade. However, from the perspective of low-income states and patients, it operates as a 'snare' or 'tangled rope,' extracting resources and suppressing access to essential medicines. The engine's classification will highlight this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders and developed nations are the primary beneficiaries (low d), gaining from extended market exclusivity and high profits. Low-income states, patients in developing countries, and generic drug manufacturers are the clear targets (high d), bearing the costs of high drug prices and restricted access to affordable medicines. WTO dispute panels act as agenda-setters, enforcing this interpretation. Public health advocates are structurally excluded from direct influence on the enforcement of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_bias_of_trips_text,
    'Is the TRIPS Agreement text inherently biased towards strong exclusivity, or is this an interpretive choice by powerful actors?',
    'Comparative legal analysis of the text''s drafting history and subsequent interpretations by diverse legal scholars and international bodies, particularly those not directly involved in trade disputes.',
    'If the text is found to be inherently ambiguous, it strengthens the argument that the ''strong exclusivity'' reading is a constructed constraint serving specific interests. If the text is demonstrably clear, it shifts the focus to the legitimacy of the text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_bias_of_trips_text, conceptual, 'Ambiguity of TRIPS text regarding patent exclusivity vs. public health.').

omega_variable(
    innovation_incentive_efficacy,
    'Does the strong exclusivity interpretation of TRIPS genuinely lead to increased pharmaceutical innovation for global health needs, or primarily for profitable markets?',
    'Empirical studies analyzing R&D investment patterns, drug development pipelines, and access to essential medicines in different regulatory environments, particularly focusing on neglected tropical diseases versus lifestyle drugs.',
    'If strong IP is shown to primarily incentivize innovation for profitable markets, it undermines the ''coordination'' narrative and strengthens the ''extraction'' argument. If it demonstrably drives innovation for global health, it supports the claimed coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_efficacy, empirical, 'Effectiveness of strong IP in driving relevant pharmaceutical innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(trip_tr_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(trip_be_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(trip_su_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, global_pharmaceutical_supply_chains).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, access_to_essential_medicines).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement interpretive kernel. It is structurally linked to the 'public_health_flexibility_reading' (a sibling interpretation) and the 'dispute_settlement_interpretive_authority' (the enforcement mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
