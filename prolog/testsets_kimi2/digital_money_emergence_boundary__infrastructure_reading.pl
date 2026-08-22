% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence: Infrastructure Reading
 *   domain: monetary_economics/financial_history
 *
 * SUMMARY:
 *   This constraint is the infrastructure reading of the contested
 *   digital_money_emergence_boundary kernel. It asserts that digital money
 *   emerged when banks could move value electronically via infrastructure
 *   milestones (1967 ATMs, 1972 ACH, 1977 SWIFT), even though consumers could
 *   not yet directly hold or transact with digital instruments. The reading
 *   functions as both a coordination device for monetary historiography and
 *   an asymmetric extraction mechanism that locates definitional power and
 *   historical legitimacy with the operators of interbank rails. It is one of
 *   three structurally distinct readings; the other two locate emergence at
 *   theoretical conceptualization or at consumer direct holdings.
 *
 * KEY AGENTS:
 *   - Interbank infrastructure operators (SWIFT, ACH): Primary beneficiary and historical narrator â they control the rails and collect definitional centrality.
 *   - Commercial banks: Secondary beneficiary â coordinated by the infrastructure and aligned with its historiography.
 *   - Consumers: Primary target â structurally excluded from the origin story of digital money and rendered invisible as monetary agents.
 *   - Central banks: Agenda setter â maintains the aggregate classifications and official histories that enforce the boundary.
 *   - Consumer digital money advocates: Excluded voice â would locate emergence at retail access but are marginalized by the infrastructure-first framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.48).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.45).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence: Infrastructure Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'b8b9d740-eee4-41b1-b20e-45393d462180').
narrative_ontology:cs_kernel_codification('b8b9d740-eee4-41b1-b20e-45393d462180', fixed_text).
narrative_ontology:cs_authority_grounding('b8b9d740-eee4-41b1-b20e-45393d462180', lineage).
narrative_ontology:cs_interpretation_layer_present('b8b9d740-eee4-41b1-b20e-45393d462180').
narrative_ontology:cs_reading_relation('b8b9d740-eee4-41b1-b20e-45393d462180', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8b9d740-eee4-41b1-b20e-45393d462180', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('b8b9d740-eee4-41b1-b20e-45393d462180', foundational, monetary_digitization_requires_transfer_infrastructure).
narrative_ontology:cs_axiom_status(monetary_digitization_requires_transfer_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('b8b9d740-eee4-41b1-b20e-45393d462180', monetary_digitization_requires_transfer_infrastructure, conventional).
narrative_ontology:cs_axiom('b8b9d740-eee4-41b1-b20e-45393d462180', secondary, retail_access_is_secondary_to_wholesale_rails).
narrative_ontology:cs_axiom_status(retail_access_is_secondary_to_wholesale_rails, holdable).
narrative_ontology:cs_axiom_grounding('b8b9d740-eee4-41b1-b20e-45393d462180', retail_access_is_secondary_to_wholesale_rails, empirically_contingent).
narrative_ontology:cs_reference_frame('b8b9d740-eee4-41b1-b20e-45393d462180', interbank_infrastructure_primacy).
narrative_ontology:cs_drift_state('b8b9d740-eee4-41b1-b20e-45393d462180', post_consumer_fintech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b8b9d740-eee4-41b1-b20e-45393d462180', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, interbank_infrastructure_operators).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the core electronic transfer networks (SWIFT, ACH) that the reading positions as the definitive origin of digital money. They collect historical legitimacy, policy centrality, and definitional authority from being named as the locus where digital money emerged.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, interbank_infrastructure_operators, beneficiary,
    institutional, generational, constrained, global).

% Rely on electronic clearing and settlement infrastructure to coordinate payments. They benefit from a shared historiography that normalizes their dependence on these rails as the natural and primary form of digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary,
    powerful, biographical, constrained, national).

% Are excluded from the definition of digital money in this reading. Their financial agency and direct digital holdings are treated as chronologically secondary and theoretically derivative of interbank infrastructure, rendering their role in monetary history structurally invisible.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumers, payer,
    powerless, biographical, trapped, national).

% Administer monetary aggregate classifications and official historiography that locate digital money's origin at the interbank infrastructure level. Their frameworks determine regulatory treatment, curriculum design, and standard economic narratives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, constrained, national).

% Would argue that digital money requires consumer-facing instruments and direct holdings. They are marginalized by the infrastructure reading, which predates and supersedes their preferred boundary in policy and academic discourse.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumer_digital_money_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, interbank_infrastructure_operators).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared historical and technical boundary for when digital money began, enabling coherent monetary aggregate classification and interbank settlement historiography across central banks and financial institutions.
% TRANSFER_FUNCTION: Moves definitional authority and historical centrality from consumer-facing or theoretical-conceptual understandings of money to the operators of electronic interbank transfer infrastructure and the commercial banks that depend on them.
% ABSENT_VOICES: Consumer digital money advocates, retail payment innovators, and heterodox monetary theorists who would locate digital money's emergence at the point of consumer access or theoretical formalization are absent from the standard policy and infrastructure-centric narrative.
% DISAPPEARANCE_RATIONALE: If the infrastructure reading vanished overnight, monetary historiography would reorganize around alternative boundaries such as consumer holdings or theoretical conceptualization, central bank aggregate classifications would shift, and the historical legitimacy currently accruing to interbank infrastructure operators would dissipate.
% FOUNDING_PROBLEM: How to classify and regulate electronic claims that did not fit physical cash or paper check categories, and how to coordinate interbank settlement in a newly computerized financial system.
% FOUNDING_PROBLEM_CORROBORATION: Academic monetary historians at independent universities attest that the classification problem has evolved beyond the infrastructure-only framing; consumer advocacy and fintech research communities attest the infrastructure reading is incomplete. Infrastructure providers themselves attest the problem remains live in terms of ongoing security needs.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the reading extracts definitional authority and historical legitimacy for infrastructure operators by embedding their rails as the necessary and sufficient condition for digital money's existence. Suppression (0.45) reflects the active marginalization of consumer-holdings and conceptualization narratives in standard monetary history and policy curricula. Theater ratio (0.28) captures the growing performative maintenance of the boundary as fintech and consumer digital wallets have made the retail alternative undeniable. Accessibility collapse (0.55) indicates that once the infrastructure reading is accepted, alternatives appear chronologically secondary rather than structurally parallel. Resistance (0.40) comes from heterodox monetary historians, consumer advocates, and retail fintech innovators who challenge the wholesale-first boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure operator's seat, the reading is an objective historical description of material capabilities. From the consumer's seat, it is an exclusionary narrative that naturalizes infrastructure control by predating and superseding their financial agency. The engine measures this divergence from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Interbank infrastructure operators and commercial banks occupy the beneficiary end: the reading subsidizes their historical role and operational centrality (low d). Consumers occupy the target end: they bear the cost of a historiography that excludes their agency (high d, amplified by trapped exit). Central banks sit near symmetric because they gain administrative clarity without personally capturing the extraction. The engine will compute divergent per-seat types: beneficiaries likely see a rope or scaffold, while consumers see a snare or tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve a genuine classification and coordination problem in the 1970s computerized banking transition. That founding problem is now contested: the technical problem of interbank messaging is solved, but the classification problem has evolved with crypto and CBDCs. The reading persists because it still performs a coordination function for central bank aggregates while simultaneously extracting definitional authority for infrastructure operators. It has not yet atrophied into a piton because the coordination function remains partially live, though the rising theater ratio indicates performative maintenance is growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the infrastructure reading foreclose the consumer holdings reading, or can they coexist as stage-dependent descriptions of digital money''s emergence?',
    'Analysis of whether monetary historiography frameworks treat these as competing singular origins or as complementary developmental stages.',
    'If foreclosed, the infrastructure reading functions as a stronger suppression mechanism against consumer-centric monetary boundaries; if coexistent, the extraction is lower and the type may shift toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural relationship between infrastructure and consumer readings of the digital money kernel.').

omega_variable(
    infrastructure_naturalness,
    'Is the infrastructure reading a technical description of an emergent material reality, or a constructed boundary that benefits identifiable infrastructure operators?',
    'Historical analysis of alternative classification proposals and the institutional incentives of SWIFT and ACH operators in promoting the infrastructure-origin narrative.',
    'If constructed, the constraint is a false summit candidate or tangled rope; if genuinely emergent, it may approach a natural-law description of monetary evolution with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_naturalness, empirical, 'Whether the boundary is constructed or naturally emergent.').

omega_variable(
    narrative_suppression_mechanism,
    'Is the dominance of the infrastructure reading maintained by institutional gatekeeping (academic curricula, central bank publications) or by epistemic lock-in (the reading seems obviously correct once the infrastructure exists)?',
    'Examination of citation networks, central bank working paper framing, and curriculum design in monetary economics.',
    'Institutional gatekeeping would indicate higher suppression and active enforcement; epistemic lock-in would indicate higher accessibility_collapse with lower active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_suppression_mechanism, empirical, 'Whether suppression is institutional or cognitive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(digi_tr_t10, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(digi_tr_t20, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(digi_tr_t30, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(digi_tr_t40, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(digi_tr_t50, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(digi_tr_t55, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 55, 0.28).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(digi_be_t10, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(digi_be_t20, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(digi_be_t30, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(digi_be_t40, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(digi_be_t50, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(digi_be_t55, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 55, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(digi_su_t10, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(digi_su_t20, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(digi_su_t30, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(digi_su_t40, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(digi_su_t50, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 50, 0.43).
narrative_ontology:measurement(digi_su_t55, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 55, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_emergence_boundary kernel, which decomposes into three structurally distinct claims about when digital money emerged: conceptualization (theoretical thinkability), infrastructure (interbank electronic transfer), and consumer holdings (direct retail access). Each reading has a different epsilon, beneficiary structure, and historical referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
