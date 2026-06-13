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
 *   This constraint story describes the 'parliamentary constraint' reading of
 *   the French Fifth Republic Constitution, where the President's executive
 *   power is significantly conditioned by the need for legislative
 *   authorization and the confidence of the National Assembly. This reading
 *   emphasizes the democratic checks on presidential authority, positioning
 *   the President as a coordinated executive rather than a dominant,
 *   unconstrained figure. The constraint ensures that policy implementation
 *   reflects the will of the legislative majority, making the President a
 *   'payer' in this system and the legislative majority a 'beneficiary'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.25).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.35).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.35).
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
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '761d562d-60e2-49bf-a8b1-997c8ffcd6ee').
narrative_ontology:cs_kernel_codification('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', fixed_text).
narrative_ontology:cs_authority_grounding('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', lineage).
narrative_ontology:cs_interpretation_layer_present('761d562d-60e2-49bf-a8b1-997c8ffcd6ee').
narrative_ontology:cs_reading_relation('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', foundational, executive_accountability_to_assembly).
narrative_ontology:cs_axiom_status(executive_accountability_to_assembly, holdable).
narrative_ontology:cs_axiom_grounding('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', executive_accountability_to_assembly, deontological).
narrative_ontology:cs_axiom('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', foundational, policy_legitimacy_from_legislative_consent).
narrative_ontology:cs_axiom_status(policy_legitimacy_from_legislative_consent, holdable).
narrative_ontology:cs_axiom_grounding('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', policy_legitimacy_from_legislative_consent, conventional).
narrative_ontology:cs_reference_frame('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', parliamentary_republic_with_strong_executive).
narrative_ontology:cs_drift_state('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('761d562d-60e2-49bf-a8b1-997c8ffcd6ee', '').
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

% The President, under this reading, is constrained by the need for legislative authorization to implement policy. Their agenda is subject to the confidence of the National Assembly, making them a target of the constraint when the Assembly withholds support or blocks legislation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, payer,
    institutional, biographical, constrained, national).

% Appointed by the President but accountable to the National Assembly, the Prime Minister leads the government and is responsible for policy implementation. Their authority is derived from the legislative majority, making them a key actor in ensuring presidential compliance with parliamentary will.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).

% The legislative body that holds the power to pass laws, approve the government's program, and censure the Prime Minister. Under this reading, the Assembly's confidence is essential for the executive to govern, making it the primary beneficiary of the constraint.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly, agenda_setter,
    institutional, generational, mobile, national).

% The political coalition that controls the National Assembly. This group benefits directly from the constraint as it ensures their policy agenda can be implemented and the President must seek their approval, reinforcing democratic accountability.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    organized, biographical, mobile, national).

% The general populace, who benefit from a system where executive power is checked by legislative oversight, ensuring policies reflect the will of their elected representatives and preventing unchecked presidential authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, citizens, beneficiary,
    organized, generational, mobile, national).

% The body responsible for ensuring the constitutionality of laws and government actions. It acts as an impartial arbiter, interpreting the Fifth Republic Constitution and upholding the balance of power, including the parliamentary constraints on the executive.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that executive policy implementation is aligned with the will of the legislative majority, preventing unilateral presidential action and fostering democratic accountability through parliamentary oversight.
% TRANSFER_FUNCTION: Transfers policy-making authority and legitimacy from the President to the legislative majority, requiring presidential initiatives to gain parliamentary consent. It also transfers accountability for policy outcomes to the government led by the Prime Minister, who is responsible to the Assembly.
% ABSENT_VOICES: A 'hyper-presidentialist' faction would argue that the President, as directly elected by the nation, should have a stronger mandate to implement policy without significant legislative hurdles. They are present in political discourse but are structurally excluded from this reading's operational logic.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the President would gain unchecked power to implement policy without legislative approval, fundamentally altering the balance of power in the Fifth Republic. The Prime Minister's role would diminish, and the National Assembly's legislative and oversight functions would be severely curtailed, leading to a more authoritarian system.
% FOUNDING_PROBLEM: The instability and perceived ineffectiveness of the Fourth Republic's parliamentary system, which led to frequent changes in government and a weak executive.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists widely corroborate the founding problem, citing the chronic governmental instability of the Fourth Republic. While the Fifth Republic aimed for a stronger executive, this reading emphasizes the continued importance of parliamentary checks to prevent a return to pre-war authoritarianism, a concern attested by constitutional scholars and opposition parties.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).

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
 *   The extractiveness (0.25) is low because the constraint primarily functions to coordinate executive action with legislative will, rather than to extract rents. Suppression (0.35) is moderate, reflecting the active enforcement mechanisms (e.g., votes of no confidence, legislative blocking) required to maintain this balance of power. The theater ratio (0.1) is low, indicating that the parliamentary checks are genuinely functional and not merely performative. The metrics reflect a system designed for coordination and democratic accountability, where the President's power is genuinely curtailed by the legislature.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislative majority, this constraint is a robust Rope, ensuring their democratic mandate is respected. For the President, it operates as a mild Snare, limiting their ability to act unilaterally and forcing them to negotiate and compromise. The engine will compute these divergent classifications based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative majority and citizens are beneficiaries (d near 0.0) as they gain from executive accountability and policy alignment. The President is a target/payer (d near 1.0) as their executive power is constrained and they must expend political capital to secure legislative approval. The Prime Minister and National Assembly are agenda-setters, mediating this relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the constraint from being mislabeled as a 'hyper-presidential' Snare by emphasizing the active and effective role of the legislature. It highlights that the founding problem of governmental instability is addressed not by eliminating parliamentary power, but by structuring it to provide a check on the executive, thus maintaining its 'live' status. The constraint's persistence is justified by its ongoing function of balancing executive authority with democratic representation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_naturalness_vs_construction,
    'Is this parliamentary constraint reading an inherent structural feature of the Fifth Republic Constitution, or a constructed interpretation that benefits the legislative majority?',
    'Analysis of constitutional text, historical practice, and judicial precedent to determine if the parliamentary checks are explicitly mandated or have evolved through political interpretation.',
    'If inherent, the constraint is closer to a Mountain; if constructed, it is a Rope or Tangled Rope, with the legislative majority as a clear beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_naturalness_vs_construction, conceptual, 'Ambiguity between constitutional design and political interpretation.').

omega_variable(
    cohabitation_impact_on_constraint,
    'How does the phenomenon of ''cohabitation'' (President and Prime Minister from opposing parties) alter the effective strength and nature of this parliamentary constraint?',
    'Comparative analysis of presidential behavior and legislative outcomes during periods of cohabitation versus periods of unified government.',
    'If cohabitation significantly strengthens parliamentary control, it supports this reading''s emphasis on legislative power. If it leads to gridlock or presidential circumvention, it suggests the constraint is weaker than this reading implies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cohabitation_impact_on_constraint, empirical, 'Impact of cohabitation on parliamentary constraint.').

omega_variable(
    hyper_presidential_vs_parliamentary_framing,
    'Is the Fifth Republic Constitution fundamentally designed for a hyper-presidential system, with parliamentary constraints as secondary, or is the parliamentary constraint an equally foundational element?',
    'Detailed textual analysis of the Constitution''s drafting history, debates among its framers, and early judicial interpretations, alongside contemporary political science scholarship.',
    'If the hyper-presidential framing is found to be more foundational, this reading''s classification as a Rope would be challenged, potentially shifting towards a Snare from the perspective of the legislature. If the parliamentary constraint is equally foundational, this reading is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hyper_presidential_vs_parliamentary_framing, conceptual, 'Framing under-determination between hyper-presidential and parliamentary interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(fift_tr_t1970, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(fift_tr_t1982, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(fift_tr_t1994, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1994, 0.08).
narrative_ontology:measurement(fift_tr_t2006, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2006, 0.09).
narrative_ontology:measurement(fift_tr_t2018, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2018, 0.09).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.3).
narrative_ontology:measurement(fift_be_t1970, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(fift_be_t1982, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1982, 0.25).
narrative_ontology:measurement(fift_be_t1994, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement(fift_be_t2006, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2006, 0.23).
narrative_ontology:measurement(fift_be_t2018, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2018, 0.24).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.4).
narrative_ontology:measurement(fift_su_t1970, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(fift_su_t1982, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1982, 0.35).
narrative_ontology:measurement(fift_su_t1994, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1994, 0.32).
narrative_ontology:measurement(fift_su_t2006, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2006, 0.33).
narrative_ontology:measurement(fift_su_t2018, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2018, 0.34).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Fifth Republic Constitution kernel. This 'parliamentary constraint' reading emphasizes legislative checks on executive power, contrasting with the 'hyper-presidential' reading (minimal legislative constraint) and the 'cohabitation equilibrium' reading (negotiated power sharing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
