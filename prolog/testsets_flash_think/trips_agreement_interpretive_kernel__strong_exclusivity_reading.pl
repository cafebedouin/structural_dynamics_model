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
 *   Agreement, which interprets the text as mandating high, uniform patent
 *   protections with narrow flexibilities. This reading prioritizes
 *   incentivizing pharmaceutical innovation through market exclusivity. It is
 *   one reading of the broader 'trips_agreement_interpretive_kernel',
 *   contested by the 'public_health_flexibility_reading'. The metrics reflect
 *   the outcomes of this strong exclusivity: high drug prices, limited
 *   generic access, and active enforcement through trade mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.9).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Agreement: Strong Exclusivity Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '76f9ab8f-9d12-4082-84cd-9141b9027feb').
narrative_ontology:cs_kernel_codification('76f9ab8f-9d12-4082-84cd-9141b9027feb', fixed_text).
narrative_ontology:cs_authority_grounding('76f9ab8f-9d12-4082-84cd-9141b9027feb', lineage).
narrative_ontology:cs_interpretation_layer_present('76f9ab8f-9d12-4082-84cd-9141b9027feb').
narrative_ontology:cs_reading_relation('76f9ab8f-9d12-4082-84cd-9141b9027feb', trips_agreement_interpretive_kernel__public_health_flexibility_reading, forecloses).
narrative_ontology:cs_axiom('76f9ab8f-9d12-4082-84cd-9141b9027feb', foundational, patent_exclusivity_drives_innovation).
narrative_ontology:cs_axiom_status(patent_exclusivity_drives_innovation, holdable).
narrative_ontology:cs_axiom_grounding('76f9ab8f-9d12-4082-84cd-9141b9027feb', patent_exclusivity_drives_innovation, empirically_contingent).
narrative_ontology:cs_axiom('76f9ab8f-9d12-4082-84cd-9141b9027feb', secondary, uniform_ip_standards_promote_global_trade).
narrative_ontology:cs_axiom_status(uniform_ip_standards_promote_global_trade, holdable).
narrative_ontology:cs_axiom_grounding('76f9ab8f-9d12-4082-84cd-9141b9027feb', uniform_ip_standards_promote_global_trade, conventional).
narrative_ontology:cs_reference_frame('76f9ab8f-9d12-4082-84cd-9141b9027feb', unfettered_ip_protection_as_norm).
narrative_ontology:cs_drift_state('76f9ab8f-9d12-4082-84cd-9141b9027feb', contemporary_public_health_crises, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('76f9ab8f-9d12-4082-84cd-9141b9027feb', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major pharmaceutical companies holding patents, actively lobbying for strong IP enforcement and interpreting TRIPS to maximize market exclusivity and profits. They benefit directly from high drug prices and limited generic competition, justifying this as necessary for R&D investment.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Governments of developing and least-developed countries, responsible for public health but facing high costs for patented medicines and limited ability to produce or import generics due to TRIPS obligations. Their exit options are constrained by trade agreements and potential sanctions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    organized, generational, constrained, national).

% Individuals in developing countries who rely on affordable medicines but face prohibitive costs for patented drugs, leading to lack of access, preventable suffering, and death. They have virtually no exit options from the global pharmaceutical market structure.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries, payer,
    powerless, immediate, trapped, local).

% Bodies within the WTO responsible for adjudicating disputes between member states regarding TRIPS compliance. Their interpretations are binding and can lead to trade sanctions, reinforcing the strong exclusivity reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, biographical, analytical, global).

% Companies capable of producing affordable generic versions of patented drugs, but legally barred from doing so in many markets due to TRIPS-mandated patent protections. They are excluded from the market segments where patents are enforced.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, excluded,
    organized, biographical, constrained, global).

% Non-governmental organizations and international bodies advocating for equitable access to medicines, challenging the strong exclusivity interpretation and pushing for broader use of TRIPS flexibilities. They analyze the constraint's impact and lobby for policy change.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To incentivize pharmaceutical innovation and R&D by guaranteeing market exclusivity for patent holders, thereby ensuring the development of new medicines and technologies.
% TRANSFER_FUNCTION: Moves substantial economic rents from national health systems and patients (especially in developing countries) to pharmaceutical patent holders, as the price for access to patented medicines.
% ABSENT_VOICES: Public health advocates, generic drug manufacturers, and patient groups often feel their voices are marginalized in TRIPS interpretation, particularly when the strong exclusivity reading is prioritized. They would argue for broader public health flexibilities.
% DISAPPEARANCE_RATIONALE: If strong patent exclusivity and its enforcement vanished overnight, generic competition would immediately drive down drug prices globally, R&D models for new medicines would shift dramatically, and global pharmaceutical markets would reorganize around different incentive structures.
% FOUNDING_PROBLEM: A perceived lack of sufficient intellectual property protection globally to incentivize R&D for new medicines and technologies, leading to underinvestment in pharmaceutical innovation.
% FOUNDING_PROBLEM_CORROBORATION: The pharmaceutical industry and developed nations attest that the problem of incentivizing innovation is still live. Developing nations, public health organizations, and independent economists attest that the founding problem is substantially addressed, and the current regime over-incentivizes, leading to rent collection rather than optimal innovation.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) due to the significant economic rents captured by patent holders through high drug prices. Suppression is very high (0.90) because the legal framework and enforcement mechanisms (e.g., WTO dispute settlement, trade sanctions) severely restrict alternatives like generic production or parallel imports. Theater ratio is moderate (0.40): while some innovation is genuinely incentivized, a substantial portion of the 'innovation' narrative serves to justify rent-seeking and market control. Accessibility collapse is high (0.88) as generic alternatives are largely unavailable. Resistance is also high (0.75) from developing countries and public health advocates. The claimed type is 'tangled_rope' because it presents a coordination function (innovation) but operates with clear asymmetric extraction and active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pharmaceutical patent holders, this reading of TRIPS is a necessary 'rope' for coordinating global innovation and ensuring returns on R&D. From the perspective of low-income states and patients, it operates as a 'snare' or highly extractive 'tangled_rope', prioritizing corporate profits over public health access. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders are the primary beneficiaries and agenda-setters, directly collecting economic rents. Low-income states and patients are the primary victims/payers, bearing the costs of high drug prices and limited access. WTO dispute panels act as agenda-setters by enforcing this interpretation. Generic drug manufacturers are excluded from markets, and public health advocates observe and resist the constraint's effects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_threshold,
    'Is the current level of TRIPS-mandated patent protection actually optimal for incentivizing pharmaceutical innovation, or does it exceed the necessary threshold, leading to rent-seeking without proportional R&D benefit?',
    'Independent economic analysis comparing R&D investment and innovation output under varying IP regimes, and analysis of ''evergreening'' practices vs. genuine new drug development.',
    'If the current protection exceeds the optimal threshold, the constraint''s effective extractiveness is higher than justified by its coordination function, supporting a reclassification towards Snare. If it is optimal, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_threshold, empirical, 'Whether high patent protection genuinely drives innovation or primarily enables rent extraction.').

omega_variable(
    public_health_vs_ip_balance,
    'How should the balance between intellectual property rights and public health needs be normatively struck within international trade law?',
    'International consensus-building through WHO and WTO negotiations, potentially leading to amendments or authoritative interpretations of TRIPS, or the development of new international legal instruments.',
    'A shift towards prioritizing public health would fundamentally alter the beneficiary/victim structure and reduce extractiveness, potentially reclassifying the constraint towards a Rope or even a Scaffold (if transitional).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_health_vs_ip_balance, preference, 'The normative weighting of IP rights against public health access.').

omega_variable(
    interpretive_ambiguity_trips,
    'Is the TRIPS Agreement text inherently ambiguous regarding public health flexibilities, allowing for multiple valid readings, or does one reading clearly dominate based on legal principles?',
    'Detailed legal-textual analysis, review of negotiating history, and analysis of subsequent state practice and WTO dispute panel rulings.',
    'If the text is genuinely ambiguous, the contest between readings is a fundamental conceptual omega. If one reading is legally dominant, the alternative reading is structurally weaker, affecting its persistence and the legitimacy of its claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_ambiguity_trips, conceptual, 'The degree of inherent ambiguity in the TRIPS Agreement text regarding IP flexibilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(trip_tr_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(trip_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(trip_be_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(trip_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(trip_su_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2015, 0.9).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.9).
narrative_ontology:measurement(trip_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, global_health_equity).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_supply_chains).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TRIPS Agreement interpretive kernel, focusing on strong patent exclusivity. It is linked to the 'public_health_flexibility_reading' which offers an alternative interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
