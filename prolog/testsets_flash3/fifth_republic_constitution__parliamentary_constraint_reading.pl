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
 *   This constraint story instantiates the 'parliamentary constraint' reading
 *   of the French Fifth Republic Constitution. In this reading, the
 *   President, while powerful, is fundamentally a coordinated executive whose
 *   policy implementation requires legislative authorization. This places the
 *   President in a 'victim' role relative to the constraint, as their power
 *   is checked, and the legislative majority acts as a beneficiary. The
 *   constraint ensures democratic accountability by requiring executive
 *   action to align with parliamentary will.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.25).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.4).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitution: Parliamentary Constraint Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, 'a63e1022-f153-4038-9f89-4cc33d888914').
narrative_ontology:cs_kernel_codification('a63e1022-f153-4038-9f89-4cc33d888914', fixed_text).
narrative_ontology:cs_authority_grounding('a63e1022-f153-4038-9f89-4cc33d888914', lineage).
narrative_ontology:cs_interpretation_layer_present('a63e1022-f153-4038-9f89-4cc33d888914').
narrative_ontology:cs_reading_relation('a63e1022-f153-4038-9f89-4cc33d888914', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('a63e1022-f153-4038-9f89-4cc33d888914', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('a63e1022-f153-4038-9f89-4cc33d888914', foundational, legislative_supremacy_in_policy_making).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_policy_making, holdable).
narrative_ontology:cs_axiom_grounding('a63e1022-f153-4038-9f89-4cc33d888914', legislative_supremacy_in_policy_making, conventional).
narrative_ontology:cs_axiom('a63e1022-f153-4038-9f89-4cc33d888914', foundational, executive_accountability_to_parliament).
narrative_ontology:cs_axiom_status(executive_accountability_to_parliament, holdable).
narrative_ontology:cs_axiom_grounding('a63e1022-f153-4038-9f89-4cc33d888914', executive_accountability_to_parliament, deontological).
narrative_ontology:cs_reference_frame('a63e1022-f153-4038-9f89-4cc33d888914', parliamentary_republicanism).
narrative_ontology:cs_drift_state('a63e1022-f153-4038-9f89-4cc33d888914', contemporary_political_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a63e1022-f153-4038-9f89-4cc33d888914', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, citizens).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The President, as head of the executive, must seek legislative authorization for major policy initiatives and can be constrained by a hostile parliamentary majority. This reading places the President in a position of needing to coordinate with the Assembly, limiting unilateral action.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, payer,
    institutional, biographical, constrained, national).

% The parliamentary majority holds the power to approve or reject legislation, to censure the government, and to withhold confidence, thereby directly constraining the President's ability to implement policy. This seat benefits from the constraint by exercising its constitutional authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, agenda_setter,
    institutional, biographical, mobile, national).

% Citizens benefit from a system where executive power is checked by legislative oversight, ensuring democratic accountability and preventing unchecked presidential authority. Their influence is primarily through elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, citizens, beneficiary,
    organized, generational, constrained, national).

% The Constitutional Court adjudicates disputes between the executive and legislative branches, interpreting the Constitution's provisions regarding their respective powers. Its rulings can reinforce or weaken the parliamentary constraint on the President.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that major policy implementation requires consensus between the executive and legislative branches, preventing unilateral executive action and fostering democratic legitimacy through parliamentary approval.
% TRANSFER_FUNCTION: Transfers policy-making authority from the President to the legislative majority when the President lacks a supportive majority, requiring the President to negotiate or defer to parliamentary will.
% ABSENT_VOICES: A 'hyper-presidential' faction would argue for stronger presidential prerogative and less legislative interference, but their interpretation is not dominant in this reading's framework.
% DISAPPEARANCE_RATIONALE: If the parliamentary constraint vanished, the President would gain unchecked power, leading to a significant shift in the balance of power, potentially undermining democratic accountability and legislative authority. Policy implementation would become largely unilateral.
% FOUNDING_PROBLEM: The instability and perceived weakness of the Fourth Republic's parliamentary system, which led to frequent government collapses and ineffective governance.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political historians attest to the historical context of the Fifth Republic's founding, aiming to balance executive stability with democratic accountability. The ongoing debates about presidential vs. parliamentary power confirm the problem's continued relevance.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.25) because the constraint primarily functions to distribute power and ensure accountability, rather than to extract rents. Suppression (0.4) reflects the active enforcement by the legislative branch to maintain its prerogatives against potential executive overreach. Theater ratio is low (0.1) as the parliamentary checks are generally real and functional, not merely performative. The metrics reflect a robust, albeit sometimes contested, system of checks and balances.
 *
 * PERSPECTIVAL GAP:
 *   From the President's perspective, this constraint can feel like an impediment to effective governance, especially when facing a hostile Assembly. From the legislative majority's perspective, it is a necessary safeguard of democratic principles. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The President is structurally a 'payer' or 'target' of this constraint, as it limits their unilateral power (d near 1.0). The legislative majority is a 'beneficiary' and 'agenda_setter', gaining authority and influence through the constraint (d near 0.0). Citizens are also beneficiaries, as the constraint promotes democratic accountability. The Constitutional Court acts as an 'observer', interpreting the rules without directly benefiting or paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    presidential_power_ambiguity,
    'To what extent does the President''s informal political capital and direct popular mandate allow them to bypass or significantly influence legislative authorization, despite formal constitutional constraints?',
    'Empirical analysis of presidential success rates in passing legislation without a clear parliamentary majority, and the impact of presidential popularity on legislative deference.',
    'If informal power consistently overrides formal constraints, the effective extractiveness on the President is lower, and the constraint might reclassify towards a Tangled Rope or even Snare from the legislative seat, as the coordination story becomes cover for presidential dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_power_ambiguity, empirical, 'Ambiguity regarding the true extent of presidential power beyond formal constitutional limits.').

omega_variable(
    cohabitation_impact_on_constraint,
    'How does the phenomenon of ''cohabitation'' (President and Prime Minister from opposing parties) alter the effective operation and classification of this parliamentary constraint?',
    'Comparative analysis of legislative-executive relations during periods of cohabitation versus periods of unified government, focusing on policy outcomes and power dynamics.',
    'Cohabitation might temporarily shift the constraint''s classification towards a more balanced Rope or even a Scaffold, as the President becomes a more direct ''victim'' of the constraint, forced into genuine power-sharing. Outside cohabitation, the constraint might appear less impactful on a President with a supportive majority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_impact_on_constraint, empirical, 'The effect of cohabitation on the parliamentary constraint''s operational dynamics.').

omega_variable(
    reading_framing_legitimacy,
    'Is this ''parliamentary constraint'' reading a legitimate interpretation of the Fifth Republic Constitution, or is it a normative preference for a particular balance of power?',
    'Analysis of constitutional jurisprudence, historical legislative practice, and the intent of the framers, weighed against contemporary political science interpretations.',
    'If primarily a normative preference, the constraint''s ''naturalness'' (emerges_naturally) is lower, and its persistence depends more on active political will than on inherent constitutional structure, potentially shifting its classification towards a more constructed type like Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Whether the parliamentary constraint is an inherent constitutional feature or a preferred political outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(fift_be_t50, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(fift_be_t60, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 60, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(fift_su_t50, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(fift_su_t60, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 60, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Fifth Republic Constitution kernel. This 'parliamentary constraint' reading emphasizes legislative checks on executive power, contrasting with the 'hyper-presidential' reading (more executive autonomy) and the 'cohabitation equilibrium' reading (negotiated power-sharing during divided government).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
