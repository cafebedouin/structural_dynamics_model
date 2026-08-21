% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitution: Parliamentary Constraint Reading
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint represents a 'parliamentary constraint' reading of the
 *   French Fifth Republic Constitution, where the President, despite direct
 *   election, is significantly constrained by the need for legislative
 *   authorization for policy implementation. This reading emphasizes the role
 *   of the National Assembly and the Prime Minister in shaping and approving
 *   government policy, placing the President in a more coordinated, rather
 *   than dominant, executive role. This is one reading of the
 *   'fifth_republic_constitution' kernel, distinct from
 *   'hyper_presidential_reading' and 'cohabitation_equilibrium_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.25).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.3).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitution: Parliamentary Constraint Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, 'af03f4d7-24fc-48e4-a63d-09c8280ef668').
narrative_ontology:cs_kernel_codification('af03f4d7-24fc-48e4-a63d-09c8280ef668', fixed_text).
narrative_ontology:cs_authority_grounding('af03f4d7-24fc-48e4-a63d-09c8280ef668', lineage).
narrative_ontology:cs_interpretation_layer_present('af03f4d7-24fc-48e4-a63d-09c8280ef668').
narrative_ontology:cs_reading_relation('af03f4d7-24fc-48e4-a63d-09c8280ef668', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('af03f4d7-24fc-48e4-a63d-09c8280ef668', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('af03f4d7-24fc-48e4-a63d-09c8280ef668', foundational, parliamentary_sovereignty_in_policy_implementation).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_in_policy_implementation, holdable).
narrative_ontology:cs_axiom_grounding('af03f4d7-24fc-48e4-a63d-09c8280ef668', parliamentary_sovereignty_in_policy_implementation, conventional).
narrative_ontology:cs_axiom('af03f4d7-24fc-48e4-a63d-09c8280ef668', foundational, executive_accountability_to_assembly).
narrative_ontology:cs_axiom_status(executive_accountability_to_assembly, holdable).
narrative_ontology:cs_axiom_grounding('af03f4d7-24fc-48e4-a63d-09c8280ef668', executive_accountability_to_assembly, deontological).
narrative_ontology:cs_reference_frame('af03f4d7-24fc-48e4-a63d-09c8280ef668', post_fourth_republic_parliamentary_revival).
narrative_ontology:cs_drift_state('af03f4d7-24fc-48e4-a63d-09c8280ef668', contemporary_presidential_activism_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('af03f4d7-24fc-48e4-a63d-09c8280ef668', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, french_citizens).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president_of_france).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The President, while directly elected, must secure legislative authorization for major policy initiatives. This reading places the President in a position of needing to coordinate with the National Assembly, especially when the Assembly is controlled by an opposing party. Their power is constrained by the need for parliamentary confidence and legislative approval.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president_of_france, payer,
    powerful, biographical, constrained, national).

% The majority in the National Assembly holds significant power to shape policy and can withhold confidence from the government, effectively constraining presidential action. This reading emphasizes their role in ensuring executive accountability and policy alignment with the popular mandate expressed through parliamentary elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    institutional, biographical, mobile, national).

% Appointed by the President but accountable to the National Assembly, the Prime Minister leads the government and is responsible for policy implementation. This reading highlights the Prime Minister's role as a bridge between the executive and legislative branches, navigating the need for both presidential and parliamentary support.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, agenda_setter,
    institutional, immediate, constrained, national).

% Benefit from a system where executive power is checked by legislative oversight, ensuring policies reflect a broader democratic consensus. This reading emphasizes the protection against unchecked presidential authority and the promotion of deliberative governance.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, french_citizens, beneficiary,
    organized, generational, mobile, national).

% Acts as the guardian of the Constitution, reviewing legislation and executive acts for their conformity with constitutional principles. This reading sees the Council as an impartial arbiter ensuring the balance of powers and the respect for parliamentary prerogatives.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that executive policy implementation is aligned with legislative will, preventing unilateral presidential action and fostering a more deliberative policy-making process through parliamentary authorization.
% TRANSFER_FUNCTION: Transfers policy-making authority from the President to the legislative majority for implementation, requiring the President to expend political capital to secure parliamentary support.
% ABSENT_VOICES: A 'strong presidentialist' faction, advocating for a more direct and unconstrained presidential mandate, would argue that this reading unduly weakens the executive's ability to act decisively in the national interest.
% DISAPPEARANCE_RATIONALE: If the requirement for legislative authorization vanished, the President would gain unchecked power, fundamentally altering the balance of the Fifth Republic. Policy would become more presidential-centric, potentially leading to less democratic accountability and greater political instability.
% FOUNDING_PROBLEM: The instability of the Fourth Republic, characterized by weak executive power and frequent changes in government, necessitated a stronger executive while still ensuring democratic accountability.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political scientists, outside of the immediate political actors, corroborate that the tension between executive strength and parliamentary accountability remains a live and central challenge in French politics, constantly negotiated through constitutional interpretation and political practice.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the constraint primarily functions to distribute power and ensure accountability, rather than to extract rents. Suppression is also low (0.30) as the system relies on political negotiation and constitutional checks, not overt coercion, to enforce the parliamentary role. Theater ratio is low (0.10) as the legislative authorization process is a genuine and active part of governance, not merely performative. The metrics reflect a functional, democratically oriented constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President, this constraint might feel like an impediment to effective governance, forcing compromises and slowing policy. From the perspective of the legislative majority and citizens, it is a vital safeguard of democratic principles. The engine's classification will reflect the structural reality of distributed power, not the subjective experience of any single actor.
 *
 * DIRECTIONALITY LOGIC:
 *   The President of France is the primary 'payer' (victim) in this reading, as their executive power is curtailed by the need for legislative approval. The legislative majority and French citizens are the 'beneficiaries', gaining from increased democratic accountability and policy alignment. The Prime Minister acts as an 'agenda_setter' within this constrained executive framework. This structural asymmetry drives the classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    presidential_vs_parliamentary_primacy,
    'Is the Fifth Republic Constitution fundamentally designed for presidential primacy or parliamentary constraint?',
    'Analysis of constitutional amendments, judicial interpretations by the Constitutional Council, and historical patterns of executive-legislative relations, particularly during periods of non-cohabitation.',
    'If presidential primacy is found to be the dominant structural feature, this reading''s low extractiveness and high democratic constraint would be re-evaluated, potentially shifting its classification towards a more extractive type for the legislature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_vs_parliamentary_primacy, conceptual, 'Ambiguity in the core design principle of the Fifth Republic''s executive-legislative balance.').

omega_variable(
    cohabitation_impact_on_constraint,
    'How does the occurrence of ''cohabitation'' (President and Prime Minister from opposing parties) alter the effective extractiveness and suppression of this constraint?',
    'Comparative analysis of policy outcomes and power dynamics during periods of cohabitation versus periods of aligned executive and legislative majorities.',
    'If cohabitation significantly increases the President''s effective extraction (by forcing concessions) or the legislative majority''s suppression (by blocking initiatives), the base metrics of this reading would need adjustment to reflect context-dependent variability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_impact_on_constraint, empirical, 'The effect of political alignment on the operational dynamics of the constitutional constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(fift_tr_t50, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 30, 0.23).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'fifth_republic_constitution' kernel, each representing a different interpretation of executive-legislative power dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
