% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Agreement: Public Health Flexibility Reading
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint represents the 'public health flexibility' reading of the
 *   TRIPS Agreement, which interprets the text as embedding broad
 *   flexibilities for compulsory licensing and parallel imports to protect
 *   public health access. This reading emerged strongly after the Doha
 *   Declaration on TRIPS and Public Health, which clarified the scope of
 *   these flexibilities. It stands in contrast to a 'strong exclusivity'
 *   reading that prioritizes patent holder rights. The metrics reflect a
 *   constraint that, while still requiring active enforcement against patent
 *   holders, aims to reduce extraction for public health beneficiaries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.3).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.4).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Agreement: Public Health Flexibility Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'ca74ea70-44c8-45ae-80bd-017be67dd89c').
narrative_ontology:cs_kernel_codification('ca74ea70-44c8-45ae-80bd-017be67dd89c', fixed_text).
narrative_ontology:cs_authority_grounding('ca74ea70-44c8-45ae-80bd-017be67dd89c', lineage).
narrative_ontology:cs_interpretation_layer_present('ca74ea70-44c8-45ae-80bd-017be67dd89c').
narrative_ontology:cs_reading_relation('ca74ea70-44c8-45ae-80bd-017be67dd89c', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca74ea70-44c8-45ae-80bd-017be67dd89c', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('ca74ea70-44c8-45ae-80bd-017be67dd89c', foundational, public_health_overrides_private_ip_rights).
narrative_ontology:cs_axiom_status(public_health_overrides_private_ip_rights, holdable).
narrative_ontology:cs_axiom_grounding('ca74ea70-44c8-45ae-80bd-017be67dd89c', public_health_overrides_private_ip_rights, deontological).
narrative_ontology:cs_axiom('ca74ea70-44c8-45ae-80bd-017be67dd89c', foundational, flexibilities_are_inherent_to_trips_text).
narrative_ontology:cs_axiom_status(flexibilities_are_inherent_to_trips_text, holdable).
narrative_ontology:cs_axiom_grounding('ca74ea70-44c8-45ae-80bd-017be67dd89c', flexibilities_are_inherent_to_trips_text, conventional).
narrative_ontology:cs_reference_frame('ca74ea70-44c8-45ae-80bd-017be67dd89c', doha_declaration_consensus).
narrative_ontology:cs_drift_state('ca74ea70-44c8-45ae-80bd-017be67dd89c', contemporary_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ca74ea70-44c8-45ae-80bd-017be67dd89c', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_drug_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to produce and export generic versions of patented medicines under compulsory licenses or parallel import schemes, increasing their market access and profitability in developing countries.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_drug_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Utilize TRIPS flexibilities to issue compulsory licenses and facilitate parallel imports, aiming to reduce drug prices and improve public health access for their populations. They actively interpret and apply these flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from increased access to affordable essential medicines, which can be life-saving. Their ability to access these drugs is directly tied to the application of TRIPS flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_developing_countries, beneficiary,
    powerless, immediate, trapped, national).

% Face erosion of their market exclusivity and pricing power in developing countries due to compulsory licensing and parallel imports. They actively lobby against broad interpretations of TRIPS flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    institutional, generational, constrained, global).

% Interprets the TRIPS Agreement in specific trade disputes. While this reading emphasizes public health, the DSB's ultimate interpretation can shift the balance between patent protection and public health flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_body, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global intellectual property regime to allow for public health exceptions, balancing patent holder rights with the imperative of access to essential medicines, particularly in developing countries.
% TRANSFER_FUNCTION: Transfers negotiating leverage and market access from pharmaceutical patent holders to generic manufacturers and health ministries in developing countries, enabling the production and import of affordable medicines.
% ABSENT_VOICES: Advocacy groups for neglected diseases and marginalized populations, who would argue for even broader and more automatic application of public health flexibilities, are often underrepresented in formal WTO dispute mechanisms.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the global IP regime would revert to a strong exclusivity stance, leading to higher drug prices, reduced access to medicines in developing countries, and a significant public health crisis, forcing a reorganization of national health policies and international aid.
% FOUNDING_PROBLEM: The original TRIPS Agreement, without explicit public health flexibilities, threatened to make essential medicines unaffordable in developing countries, leading to a global health crisis.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (e.g., WHO, Doctors Without Borders) and numerous developing country governments consistently attest that the problem of access to affordable medicines remains live, citing ongoing pandemics and high drug costs. This corroboration comes from outside the direct beneficiaries of the pharmaceutical industry.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).
:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.3) as this reading aims to reduce the extractive power of patents for public health goods. Suppression is moderate (0.4) because while flexibilities exist, their implementation often faces legal and political pressure from patent holders, requiring active enforcement by national governments. Theater ratio is low (0.1) as the public health flexibilities are genuinely utilized and not merely performative. Resistance is high (0.7) from pharmaceutical patent holders who actively challenge the application of these flexibilities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health advocates and developing countries, this reading is a vital 'rope' for global health equity. From the perspective of pharmaceutical patent holders, it is an erosion of their legitimate intellectual property rights, potentially a 'snare' for innovation incentives. The engine's classification will reflect the structural position of each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic drug manufacturers, health ministries in developing countries, and patients in those countries are beneficiaries (low d) as this reading expands their access and leverage. Pharmaceutical patent holders are victims (high d) as their market exclusivity is challenged. The WTO Dispute Settlement Body acts as an observer, adjudicating disputes that can shift the balance of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively counters potential mandatrophy of the public health objective within the TRIPS framework. By emphasizing flexibilities, it ensures the original problem of access to medicines remains addressed, preventing the IP regime from becoming a pure extraction mechanism for patent holders at the expense of public health.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_public_health_emergency,
    'What constitutes a ''national emergency'' or ''other circumstances of extreme urgency'' justifying compulsory licensing under TRIPS Article 31?',
    'Further clarification through WTO jurisprudence or a new international agreement explicitly defining the scope of such emergencies beyond pandemics.',
    'A broader definition would strengthen this reading, increasing the extractiveness from patent holders and reducing suppression for generic manufacturers. A narrower definition would weaken it, shifting towards the strong exclusivity reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_public_health_emergency, conceptual, 'Ambiguity in defining conditions for TRIPS flexibilities.').

omega_variable(
    impact_on_innovation_incentives,
    'Does the broad application of TRIPS public health flexibilities significantly diminish incentives for pharmaceutical innovation, particularly for diseases prevalent in developing countries?',
    'Longitudinal empirical studies tracking R&D investment and new drug approvals specifically for diseases affected by compulsory licensing, compared to those not.',
    'Strong evidence of diminished innovation would lend credence to the ''strong exclusivity'' reading''s concerns, potentially leading to policy adjustments that reduce the scope of flexibilities. Weak or no evidence would further entrench this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_innovation_incentives, empirical, 'Empirical impact of flexibilities on pharmaceutical R&D.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''public health flexibility'' reading, or is it a strategic interpretation by developing countries to gain leverage in trade negotiations?',
    'Analysis of national legislation and policy implementation in developing countries over time: consistent, broad application for public health needs would support the former; inconsistent or opportunistic application would support the latter.',
    'If primarily strategic, the ''public health flexibility'' framing might be seen as a ''theater'' component, increasing the effective extractiveness from patent holders without a genuine public health coordination function. If genuine, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing genuine public health intent from strategic trade leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(trip_tr_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(trip_tr_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(trip_tr_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(trip_tr_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(trip_tr_t25, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(trip_be_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(trip_be_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(trip_be_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(trip_be_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement(trip_be_t25, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 25, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(trip_su_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(trip_su_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(trip_su_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(trip_su_t20, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(trip_su_t25, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 25, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement interpretive kernel, focusing on public health flexibilities. It is linked to the 'strong exclusivity' reading and the 'dispute settlement interpretive authority' reading, which represent alternative interpretations and enforcement mechanisms of the same core text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
