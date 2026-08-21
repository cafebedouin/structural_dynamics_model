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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitution: Parliamentary Constraint Reading
 *   domain: Constitutional Law / Political Systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'parliamentary_constraint_reading'
 *   of the Fifth Republic Constitution, emphasizing the President's role as a
 *   coordinated executive requiring legislative authorization for policy
 *   implementation. This reading posits a strong democratic constraint on
 *   presidential power, with the legislative majority as the primary
 *   beneficiary of this arrangement. The metrics reflect this ideal, showing
 *   low extractiveness and suppression, consistent with a 'rope'
 *   classification, even as other readings might describe a more powerful,
 *   less constrained executive.
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
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitution: Parliamentary Constraint Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "Constitutional Law / Political Systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '530c694c-2950-4beb-bda1-fe953dd67a9b').
narrative_ontology:cs_kernel_codification('530c694c-2950-4beb-bda1-fe953dd67a9b', fixed_text).
narrative_ontology:cs_authority_grounding('530c694c-2950-4beb-bda1-fe953dd67a9b', lineage).
narrative_ontology:cs_interpretation_layer_present('530c694c-2950-4beb-bda1-fe953dd67a9b').
narrative_ontology:cs_reading_relation('530c694c-2950-4beb-bda1-fe953dd67a9b', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('530c694c-2950-4beb-bda1-fe953dd67a9b', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('530c694c-2950-4beb-bda1-fe953dd67a9b', foundational, parliamentary_supremacy_in_policy_implementation).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_in_policy_implementation, holdable).
narrative_ontology:cs_axiom_grounding('530c694c-2950-4beb-bda1-fe953dd67a9b', parliamentary_supremacy_in_policy_implementation, conventional).
narrative_ontology:cs_axiom('530c694c-2950-4beb-bda1-fe953dd67a9b', foundational, executive_accountability_to_national_assembly).
narrative_ontology:cs_axiom_status(executive_accountability_to_national_assembly, holdable).
narrative_ontology:cs_axiom_grounding('530c694c-2950-4beb-bda1-fe953dd67a9b', executive_accountability_to_national_assembly, deontological).
narrative_ontology:cs_reference_frame('530c694c-2950-4beb-bda1-fe953dd67a9b', parliamentary_republicanism).
narrative_ontology:cs_drift_state('530c694c-2950-4beb-bda1-fe953dd67a9b', contemporary_presidential_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('530c694c-2950-4beb-bda1-fe953dd67a9b', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, citizens).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, legislative_minority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The head of state, whose policy initiatives and implementation require legislative authorization. Bears the constraint of needing parliamentary confidence and legislative approval, limiting unilateral action.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, payer,
    institutional, biographical, constrained, national).

% Leads the government and is accountable to the National Assembly. Acts as the primary interface between the President and the legislative majority, navigating the need for parliamentary support.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, agenda_setter,
    institutional, biographical, constrained, national).

% The dominant coalition in the National Assembly. Wields the power to grant or withhold confidence from the government and to approve or block legislation, ensuring executive policy aligns with its mandate.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary).

% Opposes the government and legislative majority. While constrained in direct policy influence, it plays a critical role in debate, scrutiny, and holding the executive accountable, albeit without the power to block legislation unilaterally.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_minority, payer,
    organized, biographical, constrained, national).

% Benefit from democratic accountability, as executive power is channeled through and constrained by their elected representatives. Their influence is diffuse and exercised primarily through elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, citizens, beneficiary,
    powerless, generational, constrained, national).

% Adjudicates the constitutionality of laws and executive actions, acting as a check on both legislative and executive power. Provides an independent, analytical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that executive policy implementation aligns with the will of the legislative majority, thereby maintaining democratic accountability and preventing presidential overreach.
% TRANSFER_FUNCTION: Transfers ultimate policy-making authority and accountability from the President to the Prime Minister and the legislative majority, requiring the executive to secure parliamentary support for its agenda.
% ABSENT_VOICES: A strong, unified executive that believes in a direct presidential mandate, minimally constrained by the legislature, would object. This perspective is often associated with the 'hyper_presidential_reading' of the constitution.
% DISAPPEARANCE_RATIONALE: If the requirement for legislative authorization vanished, the President could act unilaterally, fundamentally altering the balance of power, democratic accountability, and the nature of the Fifth Republic's political system.
% FOUNDING_PROBLEM: To establish a stable and effective executive after periods of governmental instability, while simultaneously ensuring democratic accountability and preventing the executive from becoming unchecked.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and historical analyses from independent academic institutions and non-partisan observers consistently corroborate the ongoing tension and the founding problem's continued relevance, often contrasting it with the executive's own narrative.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The low extractiveness (0.25) and suppression (0.30) reflect this reading's emphasis on the constraint as a legitimate democratic mechanism, where the executive's power is genuinely channeled and limited by the legislature, rather than being extracted from. The low theater ratio (0.10) indicates that the legislative authorization process is seen as a functional, not merely performative, aspect of governance. Resistance is moderate (0.30) as the executive naturally seeks to maximize its room for maneuver, but the system is designed to manage this tension.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President (the 'payer' seat), this constraint might be experienced as an impediment to effective governance or a dilution of their direct mandate. However, from the perspective of the legislative majority and citizens (the 'beneficiary' seats), it is a vital mechanism for democratic control and accountability. The engine's per-seat classification would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   In this reading, the President is structurally positioned as a 'payer' or 'victim' because their actions are constrained by the need for legislative approval. The legislative majority and citizens are 'beneficiaries' as they gain from the democratic accountability and policy alignment this constraint enforces. The Prime Minister, while leading the government, acts as an 'agenda_setter' within the bounds set by the legislative majority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_practice_fidelity,
    'To what extent does the actual practice of governance in the Fifth Republic consistently reflect this ''parliamentary constraint'' reading, versus other readings that emphasize presidential power?',
    'Empirical analysis of legislative success rates for presidential initiatives, frequency of government defeats, and the use of executive decrees (ordonnances) over time, compared across different presidential terms and periods of cohabitation.',
    'If practice consistently deviates towards greater presidential autonomy, this reading''s ''rope'' classification might be re-evaluated towards a ''tangled_rope'' or even ''snare'' from the perspective of legislative power, indicating a gap between the claimed and actual function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_practice_fidelity, empirical, 'Assessing the fidelity of actual political practice to the parliamentary constraint ideal.').

omega_variable(
    cohabitation_impact_on_constraint_strength,
    'How do periods of ''cohabitation'' (when the President and parliamentary majority are from opposing political parties) alter the effective strength and nature of this parliamentary constraint?',
    'Comparative case studies of policy outcomes and power dynamics during cohabitation versus periods of unified government, focusing on the relative influence of the President and Prime Minister.',
    'If cohabitation significantly strengthens the parliamentary constraint, it suggests the constraint''s effectiveness is highly contingent on political alignment, potentially pushing the ''rope'' classification towards a ''scaffold'' (temporary strengthening) or highlighting a ''tangled_rope'' dynamic where power shifts more explicitly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_impact_on_constraint_strength, empirical, 'Impact of divided government on the parliamentary constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.08).
narrative_ontology:measurement(fift_tr_t1970, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1970, 0.09).
narrative_ontology:measurement(fift_tr_t1985, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(fift_tr_t2010, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(fift_tr_t2023, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.2).
narrative_ontology:measurement(fift_be_t1970, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(fift_be_t1985, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1985, 0.24).
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(fift_be_t2010, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(fift_be_t2023, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2023, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(fift_su_t1970, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(fift_su_t1985, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(fift_su_t2010, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2010, 0.31).
narrative_ontology:measurement(fift_su_t2023, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Fifth Republic Constitution kernel. Each reading emphasizes different aspects of executive-legislative power dynamics, leading to different structural classifications and metric profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
