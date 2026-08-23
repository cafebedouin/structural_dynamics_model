% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Separation of Powers — Flexible Framework with Intelligible Principle Delegation
 *   domain: constitutional_law/administrative_law/political_theory
 *
 * SUMMARY:
 *   The functionalist reading of separation of powers treats the
 *   constitutional structure as a flexible framework that permits overlapping
 *   authority and intelligible principle delegation. This reading legitimates
 *   the modern administrative state: Congress delegates legislative
 *   detail-filling to agencies, the President supervises through appointment
 *   and removal, and courts review with deference. The constraint coordinates
 *   governance across branches, solving the legislative capacity problem.
 *   Extraction is low (ε=0.28) because the arrangement's primary function is
 *   coordination—enabling expert regulation—though democratic accountability
 *   attenuation creates some extractive pressure on citizens and states. The
 *   claimed type is 'rope' (pure coordination), but the engine will compute
 *   per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.28).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.32).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Separation of Powers — Flexible Framework with Intelligible Principle Delegation").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/administrative_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '60c81124-03d3-40ba-bf18-911cf05a8377').
narrative_ontology:cs_kernel_codification('60c81124-03d3-40ba-bf18-911cf05a8377', fixed_text).
narrative_ontology:cs_authority_grounding('60c81124-03d3-40ba-bf18-911cf05a8377', lineage).
narrative_ontology:cs_interpretation_layer_present('60c81124-03d3-40ba-bf18-911cf05a8377').
narrative_ontology:cs_reading_relation('60c81124-03d3-40ba-bf18-911cf05a8377', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('60c81124-03d3-40ba-bf18-911cf05a8377', separation_of_powers_text__unitary_executive_reading, forecloses).
narrative_ontology:cs_axiom('60c81124-03d3-40ba-bf18-911cf05a8377', foundational, intelligible_principle_delegation_permissible).
narrative_ontology:cs_axiom_status(intelligible_principle_delegation_permissible, holdable).
narrative_ontology:cs_axiom_grounding('60c81124-03d3-40ba-bf18-911cf05a8377', intelligible_principle_delegation_permissible, conventional).
narrative_ontology:cs_axiom('60c81124-03d3-40ba-bf18-911cf05a8377', foundational, independent_agencies_constitutional).
narrative_ontology:cs_axiom_status(independent_agencies_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('60c81124-03d3-40ba-bf18-911cf05a8377', independent_agencies_constitutional, conventional).
narrative_ontology:cs_axiom('60c81124-03d3-40ba-bf18-911cf05a8377', secondary, judicial_deference_as_coordination).
narrative_ontology:cs_axiom_status(judicial_deference_as_coordination, holdable).
narrative_ontology:cs_axiom_grounding('60c81124-03d3-40ba-bf18-911cf05a8377', judicial_deference_as_coordination, instrumental).
narrative_ontology:cs_reference_frame('60c81124-03d3-40ba-bf18-911cf05a8377', founding_flexible_framework).
narrative_ontology:cs_drift_state('60c81124-03d3-40ba-bf18-911cf05a8377', contemporary_administrative_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60c81124-03d3-40ba-bf18-911cf05a8377', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulated_entities).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, citizens_voters).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_entities).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, intelligible_principle_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, independent_agency_legitimacy).
narrative_ontology:constraint_vindicates(separation_of_powers_text__functionalist_reading, administrative_state_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Delegates legislative detail-filling to agencies via statutes with intelligible principles; retains oversight through hearings, appropriations, and statutory amendment; cannot practically reclaim all delegated authority due to complexity of modern governance.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, agenda_setter,
    powerful, biographical, constrained, national).

% Appoints agency heads, directs policy through executive orders and OIRA review, controls removal of principal officers; shares effective legislative power through agency rulemaking but depends on Senate confirmation and statutory frameworks.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, agenda_setter,
    powerful, biographical, constrained, national).

% Receive delegated rulemaking and adjudication authority; operate under presidential supervision and judicial deference doctrines (Chevron, Skidmore, Auer); their legitimacy and operational capacity depend on the functionalist reading's validation of delegation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, constrained, national).

% Review agency actions under deference frameworks; articulate and police the intelligible principle boundary; their doctrinal choices (Chevron, major questions doctrine) shape the constraint's operational extraction.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, courts, agenda_setter).

% Subject to agency rules and enforcement; benefit from expert, technically informed regulation and stable expectations; bear compliance costs and face asymmetric litigation risk against agency interpretations.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_entities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, regulated_entities, beneficiary).

% Experience regulation's benefits (safety, environment, finance) and costs (compliance burden, democratic attenuation); accountability runs through diffuse electoral channels (President, Congress) rather than direct agency control.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, citizens_voters, payer,
    moderate, biographical, constrained, national).

% Displaced by federal regulatory preemption in many domains; their structural role in the compound republic is attenuated by the administrative state's reach; litigate federalism boundaries but lack institutional veto over delegation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, states, excluded,
    powerful, generational, constrained, national).

% Argue that Article I forbids delegation of legislative power; their position is academically influential and judicially represented (e.g., Gorsuch, Thomas) but has not prevailed in operational doctrine since the 1930s.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_scholars, excluded,
    organized, generational, analytical, national).

% Argue that Article II vests all executive power in the President, rendering independent agencies unconstitutional; gained judicial traction (Seila Law, Collins v. Yellen) but functionalist deference framework remains dominant.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, unitary_executive_proponents, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables expert governance across separated branches by permitting Congress to delegate rulemaking authority with intelligible principles, subject to presidential oversight and judicial deference, solving the collective-action problem of legislative capacity in a complex society.
% TRANSFER_FUNCTION: Moves legislative detail-filling authority from Congress to agencies, with presidential appointment/removal and judicial review as checks; the intelligible principle standard calibrates the transfer, while deference doctrines (Chevron, Auer) determine how much interpretive authority agencies retain.
% ABSENT_VOICES: Formalist originalists who argue delegation violates Article I's vesting clause; unitary executive theorists who argue independent agencies violate Article II; both are present in academia and on the bench but structurally excluded from the operational doctrine that governs the administrative state.
% DISAPPEARANCE_RATIONALE: If the intelligible principle delegation framework vanished overnight, Congress would need to legislate all regulatory detail directly, agencies would lose rulemaking authority, the regulatory state would shrink dramatically, and governance of complex technical domains would shift to courts or private ordering.
% FOUNDING_PROBLEM: How to govern a complex modern society with separated powers when Congress lacks the time, expertise, and capacity to legislate all necessary regulatory detail.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists (e.g., Moe, Huber, Shipan) and administrative law scholars outside the beneficiary set (e.g., Vermeule, Watts) attest that legislative detail-filling by agencies is a practical necessity for modern governance; the legislative capacity gap is documented in empirical studies of congressional output versus regulatory scope.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.28) because the functionalist reading sees delegation as coordination, not extraction—the intelligible principle and deference doctrines are coordination mechanisms. Suppression is moderate (0.32): formalist and unitary executive alternatives persist in discourse and jurisprudence but are suppressed in operational doctrine. Theater ratio is low (0.22): the coordination function (expert governance) is genuine, though major questions doctrine introduces some performative constraint. Accessibility collapse is moderate (0.45): alternative readings remain intelligible and advocated. Resistance is significant (0.58): formalist and unitary executive challenges continue in courts and scholarship.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seats (Congress, President), the constraint is enabling coordination. From the payer seats (citizens, regulated entities), it mixes benefit and cost. From excluded seats (states, formalist/unitary theorists), it is a structural displacement. The engine computes these divergences; the functionalist reading itself claims the coordination story.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress and President are agenda_setters with constrained exit (they depend on delegation for governance capacity); agencies are beneficiaries (receive authority); regulated_entities are dual payer/beneficiary (compliance costs vs. expert regulation); citizens_voters are payers (democratic attenuation); states are excluded (federalism displacement); formalist_scholars and unitary_executive_proponents are excluded analytical voices. The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative capacity gap) remains live—modern governance complexity has increased, not decreased. The arrangement has not outlived its function; it has expanded to meet growing coordination demands. Mandatrophy is not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the functionalist reading a distinct constraint from the formalist and unitary executive readings, or a contingent interpretation of the same constitutional text?',
    'The ε-invariance principle: if the three readings produce structurally different beneficiary/victim sets, extractiveness profiles, and enforcement requirements, they are distinct constraints. This story authors the functionalist reading as a separate constraint with its own ε.',
    'If the readings are distinct constraints, each gets its own classification; if they are one constraint with measurement variance, the framework''s ε-invariance is violated. The decomposition into three stories is the framework''s resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are distinct constraints or one constraint measured differently.').

omega_variable(
    delegation_boundary_ambiguity,
    'Where does the intelligible principle end and legislative power begin? The boundary is contested (major questions doctrine, non-delegation revival).',
    'Supreme Court doctrine evolution: if major questions doctrine expands, the functionalist reading''s coordination domain shrinks, increasing ε for agencies and regulated entities.',
    'A narrowing delegation boundary raises extractiveness (more legislative power retained by Congress, less agency discretion) and shifts the constraint toward tangled_rope or snare from the agency seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_boundary_ambiguity, empirical, 'The structural location of the delegation boundary and its effect on ε.').

omega_variable(
    deference_doctrine_extraction,
    'Do Chevron, Auer, and Skidmore deference constitute coordination (expertise allocation) or extraction (judicial abdication concentrating power in agencies)?',
    'Empirical study of agency win rates with vs. without deference; comparative analysis of regulatory outcomes under different deference regimes.',
    'If deference is extraction, the functionalist reading''s ε is understated; the constraint reclassifies toward tangled_rope. If coordination, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_doctrine_extraction, conceptual, 'Whether judicial deference doctrines are coordination mechanisms or extraction amplifiers.').

omega_variable(
    democratic_accountability_gap,
    'Does the functionalist framework''s attenuation of democratic accountability (citizens → Congress/President → agencies) constitute structural extraction from citizens_voters?',
    'Political science measurement of policy responsiveness: compare regulatory outputs to voter preferences under delegation vs. direct legislation.',
    'If accountability gap is extractive, citizens_voters move from payer toward victim with higher effective extraction; the constraint may show tangled_rope characteristics from that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_accountability_gap, preference, 'Whether democratic attenuation in the administrative state is extractive or a necessary coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_tr_t1789, separation_of_powers_text__functionalist_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_tr_t1887, separation_of_powers_text__functionalist_reading, theater_ratio, 1887, 0.1).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_tr_t1935, separation_of_powers_text__functionalist_reading, theater_ratio, 1935, 0.15).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_tr_t1946, separation_of_powers_text__functionalist_reading, theater_ratio, 1946, 0.18).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_tr_t1984, separation_of_powers_text__functionalist_reading, theater_ratio, 1984, 0.2).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_tr_t2000, separation_of_powers_text__functionalist_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_tr_t2024, separation_of_powers_text__functionalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_be_t1789, separation_of_powers_text__functionalist_reading, base_extractiveness, 1789, 0.05).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_be_t1887, separation_of_powers_text__functionalist_reading, base_extractiveness, 1887, 0.12).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_be_t1935, separation_of_powers_text__functionalist_reading, base_extractiveness, 1935, 0.22).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_be_t1946, separation_of_powers_text__functionalist_reading, base_extractiveness, 1946, 0.25).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_be_t1984, separation_of_powers_text__functionalist_reading, base_extractiveness, 1984, 0.28).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_be_t2000, separation_of_powers_text__functionalist_reading, base_extractiveness, 2000, 0.27).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_be_t2024, separation_of_powers_text__functionalist_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_su_t1789, separation_of_powers_text__functionalist_reading, suppression_requirement, 1789, 0.05).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_su_t1887, separation_of_powers_text__functionalist_reading, suppression_requirement, 1887, 0.15).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_su_t1935, separation_of_powers_text__functionalist_reading, suppression_requirement, 1935, 0.25).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_su_t1946, separation_of_powers_text__functionalist_reading, suppression_requirement, 1946, 0.28).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_su_t1984, separation_of_powers_text__functionalist_reading, suppression_requirement, 1984, 0.3).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_su_t2000, separation_of_powers_text__functionalist_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(separation_of_powers_text__functionalist_reading_su_t2024, separation_of_powers_text__functionalist_reading, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__functionalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, major_questions_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one member of the separation_of_powers_text constraint family. The formalist_reading (strict boundaries, non-delegation) and unitary_executive_reading (presidential control, no independent agencies) are sibling constraints with different ε, beneficiary/victim structures, and claimed types. All three share the kernel_id separation_of_powers_text. The functionalist reading has lower ε (coordination via deference) and legitimates agencies; the formalist reading has higher ε from the agency seat (suppression of delegation); the unitary executive reading has higher ε from the agency seat (suppression of independence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, powerful, 0.35).
constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, organized, 0.45).
constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
