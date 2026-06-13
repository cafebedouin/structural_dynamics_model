% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Principle (Reading of Separation of Powers Text)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint represents the 'unitary executive' reading of the U.S.
 *   Constitution's separation of powers, asserting that all executive power
 *   vests solely in the President, and therefore, independent agencies
 *   operating outside direct presidential control are unconstitutional. This
 *   reading has gained prominence since the 1980s, leading to increased
 *   efforts by presidents to assert control over such agencies. It is claimed
 *   as a 'tangled_rope' because it purports to coordinate executive function
 *   while demonstrably extracting power from other branches and agencies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.65).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.7).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Principle (Reading of Separation of Powers Text)").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'd04a1330-a139-4284-8322-adcc9c7a2a09').
narrative_ontology:cs_kernel_codification('d04a1330-a139-4284-8322-adcc9c7a2a09', fixed_text).
narrative_ontology:cs_authority_grounding('d04a1330-a139-4284-8322-adcc9c7a2a09', lineage).
narrative_ontology:cs_interpretation_layer_present('d04a1330-a139-4284-8322-adcc9c7a2a09').
narrative_ontology:cs_reading_relation('d04a1330-a139-4284-8322-adcc9c7a2a09', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d04a1330-a139-4284-8322-adcc9c7a2a09', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('d04a1330-a139-4284-8322-adcc9c7a2a09', foundational, all_executive_power_vests_in_president).
narrative_ontology:cs_axiom_status(all_executive_power_vests_in_president, holdable).
narrative_ontology:cs_axiom_grounding('d04a1330-a139-4284-8322-adcc9c7a2a09', all_executive_power_vests_in_president, deontological).
narrative_ontology:cs_axiom('d04a1330-a139-4284-8322-adcc9c7a2a09', secondary, presidential_removal_power_is_absolute).
narrative_ontology:cs_axiom_status(presidential_removal_power_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d04a1330-a139-4284-8322-adcc9c7a2a09', presidential_removal_power_is_absolute, conventional).
narrative_ontology:cs_reference_frame('d04a1330-a139-4284-8322-adcc9c7a2a09', energetic_executive_accountability).
narrative_ontology:cs_drift_state('d04a1330-a139-4284-8322-adcc9c7a2a09', contemporary_administrative_state, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d04a1330-a139-4284-8322-adcc9c7a2a09', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, the_president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch_officials).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congressional_oversight).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the President holds absolute authority over the executive branch, including the power to remove any executive official at will. This maximizes presidential control and policy implementation speed.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, the_president, agenda_setter,
    institutional, immediate, constrained, national).

% Benefit from clear lines of authority and reduced bureaucratic friction, as independent agencies' resistance to presidential directives is minimized. Their careers are tied to presidential favor.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch_officials, beneficiary,
    powerful, biographical, constrained, national).

% Agencies like the FTC, NLRB, and Federal Reserve are designed to operate with a degree of independence from direct presidential control. This reading subjects them to presidential removal power, undermining their statutory independence and mission. Their institutional identity is tied to their independence.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    organized, generational, identity_locked, national).

% Congress's ability to create and empower independent agencies as a check on executive power is diminished. This reading reduces the legislative branch's influence over administrative policy and enforcement.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congressional_oversight, payer,
    institutional, generational, constrained, national).

% The judiciary is tasked with interpreting the Constitution and adjudicating disputes over the separation of powers. This reading presents a specific interpretive challenge, potentially leading to judicial review of presidential actions and agency structures.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to ensure a unified and efficient executive branch, where all administrative actions are directly accountable to the President, thereby streamlining policy implementation and reducing inter-agency conflict.
% TRANSFER_FUNCTION: Transfers authority and control over independent agencies from Congress and the agencies themselves to the President, centralizing executive power.
% ABSENT_VOICES: Advocates for administrative expertise, non-partisanship, and long-term policy stability would object, arguing that independent agencies serve vital functions that require insulation from political pressures. They are often marginalized in debates dominated by executive power claims.
% DISAPPEARANCE_RATIONALE: If the unitary executive principle as read here vanished, the President's power would be significantly curtailed, independent agencies would regain their statutory insulation, and Congress's role in administrative governance would be reasserted. The balance of power within the federal government would fundamentally shift.
% FOUNDING_PROBLEM: The framers sought to create an energetic executive capable of acting decisively, while also preventing tyranny through a system of checks and balances.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars from various perspectives corroborate the framers' intent to balance executive energy with checks. The specific interpretation of 'unitary' remains contested, but the underlying problem of executive power and accountability is universally acknowledged as live.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial as it centralizes significant power in the presidency, diminishing the checks and balances intended by Congress in creating independent agencies. Suppression (0.70) is high because it requires active legal and political enforcement to challenge and dismantle existing structures of agency independence. Theater ratio (0.20) is low, as the arguments for unitary executive power are genuinely advanced and acted upon, not merely performative. The rising extractiveness and suppression over time reflect the increasing assertiveness of this reading in legal and political discourse.
 *
 * PERSPECTIVAL GAP:
 *   The President and executive branch officials experience this as a legitimate and necessary coordination mechanism for effective governance. Independent agencies and congressional oversight bodies experience it as an extractive and suppressive force undermining their constitutional roles and statutory mandates. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and executive branch officials are clear beneficiaries (d=0.0-0.2) as they gain power and control. Independent agencies and congressional oversight are targets (d=0.8-1.0) as their authority is diminished and their structures challenged. The federal judiciary acts as an analytical observer, adjudicating the claims without directly benefiting or being targeted by the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a power grab as pure coordination. While the unitary executive reading claims to solve a coordination problem (executive efficiency), its high extractiveness and suppression, coupled with identifiable victims, reveal its 'tangled_rope' nature. It's not a 'snare' because a genuine, albeit contested, coordination argument exists, and it's not a 'rope' due to the clear asymmetric extraction and active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unitary_executive_constitutional_basis,
    'Is the unitary executive principle a direct textual mandate of the Constitution, or an interpretive theory derived from broader structural inferences?',
    'Further historical and textual analysis of the founding era, and evolving judicial precedent. A definitive textual basis would strengthen its claim to naturalness; a purely inferential basis would highlight its constructed nature.',
    'If a direct textual mandate, the constraint''s extractiveness might be re-evaluated as an inherent cost of constitutional design. If an interpretive theory, its constructed nature and potential for extraction become more salient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unitary_executive_constitutional_basis, conceptual, 'Ambiguity regarding the constitutional grounding of the unitary executive principle.').

omega_variable(
    presidential_removal_power_scope,
    'Does the President''s removal power extend to all ''principal officers'' or only to those performing purely executive functions, excluding quasi-legislative or quasi-judicial roles within independent agencies?',
    'Supreme Court rulings on specific removal cases (e.g., Humphrey''s Executor, Morrison v. Olson, Seila Law LLC v. CFPB) and subsequent legislative responses.',
    'A broad removal power would increase the constraint''s effective suppression on independent agencies; a narrow power would reduce it, allowing agencies more insulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_removal_power_scope, empirical, 'Scope of presidential removal power over independent agency officials.').

omega_variable(
    reading_vs_kernel_distinction,
    'Is this constraint a genuine reading of the ''separation_of_powers_text'' kernel, or does it fundamentally alter the kernel''s meaning to serve a specific political agenda?',
    'Comparative analysis with other constitutional readings (formalist, functionalist) and their historical evolution. If this reading consistently requires reinterpreting core constitutional principles in a way that other readings do not, it suggests a greater departure from the kernel.',
    'If it''s a fundamental alteration, the constraint''s legitimacy as a ''reading'' is undermined, potentially reclassifying it as a ''snare'' that uses constitutional language as cover for power consolidation. If a valid reading, its ''tangled_rope'' classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_kernel_distinction, conceptual, 'Whether the unitary executive reading is a faithful interpretation or a re-framing of the separation of powers kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1980, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(sepa_tr_t2020, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1980, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(sepa_be_t2020, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1980, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(sepa_su_t2020, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, congressional_delegation_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'separation_of_powers_text' kernel. It emphasizes presidential control over the executive branch, contrasting with formalist (strict boundaries) and functionalist (flexible boundaries) readings. Each reading generates a distinct constraint with different beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
