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
 *   TRIPS Agreement, which interprets the text as embedding broad provisions
 *   for compulsory licensing and parallel imports to prioritize public health
 *   access over strict intellectual property rights. This reading gained
 *   significant traction after the Doha Declaration on TRIPS and Public
 *   Health (2001), which clarified and affirmed these flexibilities. The
 *   metrics reflect a relatively low extractiveness and suppression compared
 *   to a 'strong exclusivity' reading, as this interpretation aims to reduce
 *   the extractive power of patents in public health contexts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.35).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.45).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Agreement: Public Health Flexibility Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '9e7cee35-b891-49c5-8495-e6d09d4a6ccc').
narrative_ontology:cs_kernel_codification('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', fixed_text).
narrative_ontology:cs_authority_grounding('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', lineage).
narrative_ontology:cs_interpretation_layer_present('9e7cee35-b891-49c5-8495-e6d09d4a6ccc').
narrative_ontology:cs_reading_relation('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', foundational, public_health_overrides_private_ip).
narrative_ontology:cs_axiom_status(public_health_overrides_private_ip, holdable).
narrative_ontology:cs_axiom_grounding('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', public_health_overrides_private_ip, deontological).
narrative_ontology:cs_axiom('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', foundational, flexibilities_are_inherent_to_trips).
narrative_ontology:cs_axiom_status(flexibilities_are_inherent_to_trips, holdable).
narrative_ontology:cs_axiom_grounding('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', flexibilities_are_inherent_to_trips, conventional).
narrative_ontology:cs_reference_frame('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', doha_declaration_consensus).
narrative_ontology:cs_drift_state('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', contemporary_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9e7cee35-b891-49c5-8495-e6d09d4a6ccc', '').
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

% Benefit from the ability to produce and export generic versions of patented medicines under compulsory licenses or parallel import schemes, increasing their market share and revenue, particularly in developing countries. This reading expands their negotiating leverage.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_drug_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Utilize TRIPS flexibilities to issue compulsory licenses and facilitate parallel imports, aiming to reduce the cost of essential medicines and improve public health outcomes. They actively interpret and apply these provisions to serve their national health agendas.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries, agenda_setter,
    institutional, generational, constrained, national).

% Gain access to more affordable, life-saving medicines that would otherwise be prohibitively expensive due to patent protection. Their well-being is directly tied to the robust application of these flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patients_developing_countries, beneficiary,
    powerless, immediate, trapped, local).

% Face erosion of their market exclusivity and pricing power in developing countries due to compulsory licensing and parallel imports. They argue these flexibilities undermine incentives for innovation and seek to limit their application.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    institutional, generational, constrained, global).

% Serves as the ultimate arbiter of TRIPS disputes. Its interpretations can either reinforce or constrain the public health flexibilities, influencing the balance between patent protection and access to medicines. This reading seeks to influence its jurisprudence.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_dispute_settlement_body, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global intellectual property regime to allow for public health exceptions, balancing the rights of patent holders with the imperative of access to essential medicines, particularly in public health emergencies.
% TRANSFER_FUNCTION: Transfers negotiating leverage and market access from pharmaceutical patent holders to generic manufacturers and health ministries, enabling the production and distribution of more affordable medicines.
% ABSENT_VOICES: Advocacy groups for neglected diseases and marginalized populations, who would argue for even broader and more automatic application of flexibilities, are often underrepresented in formal WTO negotiations, though their influence is felt through health ministries.
% DISAPPEARANCE_RATIONALE: If this reading of TRIPS vanished, developing countries would lose a critical legal tool for accessing affordable medicines, leading to higher drug prices, reduced access, and significant public health crises. The global pharmaceutical market would revert to a strong exclusivity model, fundamentally altering access dynamics.
% FOUNDING_PROBLEM: The original TRIPS Agreement, without explicit public health flexibilities, threatened to make essential medicines unaffordable in developing countries, leading to a global public health crisis.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (e.g., WHO, Doctors Without Borders) and numerous academic studies corroborate that the problem of access to affordable medicines remains live, especially for new pandemics and neglected diseases. This corroboration comes from outside the direct beneficiaries of the flexibilities.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is moderate because while it reduces the monopoly rents of patent holders, it doesn't eliminate them entirely, and some transaction costs remain. Suppression (0.45) is also moderate; while it actively counters the suppressive force of strong IP, it still requires active legal and political effort to implement, and patent holders continue to exert pressure. Theater ratio is low (0.1) as the flexibilities are genuinely used to achieve public health goals, not merely for show. The dip in extractiveness and suppression around 2001-2005 reflects the impact of the Doha Declaration, which strengthened this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health advocates and developing countries, this reading is a vital 'rope' for global health equity. From the perspective of pharmaceutical patent holders, it is an unwelcome 'snare' that undermines innovation incentives. The engine's classification will reflect the structural position of each seat, not a single, universal type.
 *
 * DIRECTIONALITY LOGIC:
 *   Generic drug manufacturers, health ministries in developing countries, and patients are beneficiaries, gaining access and leverage. Pharmaceutical patent holders are victims, facing reduced market exclusivity and pricing power. The WTO Dispute Settlement Body acts as an observer, whose interpretations are crucial to the persistence of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_enforcement_gap,
    'To what extent is the ''public health flexibility'' reading effectively implemented and enforced by developing countries, given political and economic pressures from patent-holding nations?',
    'Empirical studies tracking the actual issuance of compulsory licenses and utilization of parallel import provisions, alongside analysis of bilateral trade agreements that may limit these flexibilities.',
    'If implementation is weak despite the legal reading, the effective extractiveness of pharmaceutical patents remains higher than this reading suggests, pushing the constraint towards a ''tangled_rope'' or ''snare'' for developing countries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_enforcement_gap, empirical, 'Gap between legal interpretation and practical enforcement of TRIPS flexibilities.').

omega_variable(
    innovation_incentive_tradeoff,
    'Does the broad application of TRIPS public health flexibilities genuinely undermine pharmaceutical innovation, or is this a rhetorical claim by patent holders to maintain market power?',
    'Longitudinal economic studies correlating the use of flexibilities with R&D investment and new drug development, disaggregated by disease area and market type.',
    'If innovation is demonstrably harmed, the ''public health flexibility'' reading might be re-evaluated for its long-term sustainability. If not, the ''strong exclusivity'' reading''s core justification is weakened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_incentive_tradeoff, empirical, 'Impact of public health flexibilities on pharmaceutical innovation incentives.').

omega_variable(
    reading_coexistence_stability,
    'How stable is the coexistence of the ''public_health_flexibility_reading'' and the ''strong_exclusivity_reading'' within the TRIPS framework, or is one gradually foreclosing the other?',
    'Analysis of WTO dispute settlement rulings, national legislative changes, and the outcomes of subsequent international negotiations (e.g., pandemic treaties) over a multi-decade period.',
    'If one reading begins to foreclose the other, the entire TRIPS interpretive kernel shifts, leading to a reclassification of the dominant constraint type governing global IP and public health.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_stability, conceptual, 'Stability of coexisting interpretations of TRIPS flexibilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2001, 0.08).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2001, 0.3).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2010, 0.3).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, global_health_equity_framework).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_rd_incentive_structures).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement interpretive kernel. It is linked to the 'strong_exclusivity_reading' and 'dispute_settlement_interpretive_authority' as part of a constraint family, where different interpretations lead to structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
