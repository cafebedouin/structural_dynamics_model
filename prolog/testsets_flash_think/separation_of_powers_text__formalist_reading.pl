% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist Reading of Separation of Powers: Non-Delegation Doctrine
 *   domain: constitutional_law/administrative_law/political_theory
 *
 * SUMMARY:
 *   This constraint represents the formalist reading of the U.S.
 *   Constitution's separation of powers, asserting strict, impermeable
 *   boundaries between the legislative, executive, and judicial branches. It
 *   specifically claims that Congress cannot delegate its legislative
 *   authority to administrative agencies. While proponents claim this as a
 *   fundamental, unchangeable constitutional truth (hence 'mountain' claim),
 *   its active enforcement and the identifiable victims (administrative
 *   agencies, executive branch) suggest a highly extractive and suppressive
 *   operation. The metrics reflect the actual impact of this interpretation,
 *   which is distinct from its proponents' claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.85).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.9).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, mountain).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist Reading of Separation of Powers: Non-Delegation Doctrine").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/administrative_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).
domain_priors:emerges_naturally(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, 'af7daa52-4257-4a2c-b4d2-ac32d4abd4a7').
narrative_ontology:cs_kernel_codification('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', fixed_text).
narrative_ontology:cs_authority_grounding('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', lineage).
narrative_ontology:cs_interpretation_layer_present('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7').
narrative_ontology:cs_reading_relation('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', foundational, strict_separation_of_powers).
narrative_ontology:cs_axiom_status(strict_separation_of_powers, holdable).
narrative_ontology:cs_axiom_grounding('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', strict_separation_of_powers, deontological).
narrative_ontology:cs_axiom('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', foundational, non_delegation_principle).
narrative_ontology:cs_axiom_status(non_delegation_principle, holdable).
narrative_ontology:cs_axiom_grounding('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', non_delegation_principle, conventional).
narrative_ontology:cs_reference_frame('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', founding_era_strict_separation).
narrative_ontology:cs_drift_state('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', rise_of_administrative_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('af7daa52-4257-4a2c-b4d2-ac32d4abd4a7', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_judges).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, industries_opposed_to_regulation).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, executive_branch).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, public_benefiting_from_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Constitution as establishing strict, impermeable boundaries between branches, prohibiting Congress from delegating legislative authority to administrative agencies. They actively seek to enforce this interpretation through judicial review.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_judges, agenda_setter,
    institutional, civilizational, analytical, national).

% Are the primary targets of this constraint, losing their ability to issue rules with the force of law. Their regulatory capacity is drastically reduced, impacting their ability to implement policy and respond to complex societal problems.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    institutional, biographical, trapped, national).

% Theoretically reclaims legislative power that was previously delegated. While gaining authority, it also faces an increased workload and the challenge of legislating on highly technical matters without agency expertise.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, legislative_branch, beneficiary,
    institutional, generational, mobile, national).

% Loses a key mechanism for implementing its policy agenda, as agencies under its control are stripped of legislative authority. This limits the President's ability to govern effectively in a modern, complex state.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Benefit significantly from the reduction in regulatory burden and the weakening of administrative agencies. They actively support and lobby for this formalist interpretation to limit government oversight.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, industries_opposed_to_regulation, beneficiary,
    organized, biographical, arbitrage, national).

% Loses protections and benefits provided by agency regulations (e.g., environmental, consumer, worker safety). The costs are diffuse but significant, as legislative gridlock often prevents Congress from filling the regulatory void.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, public_benefiting_from_regulation, payer,
    organized, generational, constrained, national).

% Advocate for a more flexible interpretation of separation of powers, arguing that delegation is necessary for effective modern governance. Their views are actively suppressed by the formalist reading's enforcement.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_scholars_and_judges, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate governmental power by strictly separating legislative, executive, and judicial functions to prevent the concentration of power and ensure accountability to the people through their elected representatives.
% TRANSFER_FUNCTION: Transfers legislative authority from administrative agencies back to Congress; transfers regulatory burden from industries to the public (via reduced protections and slower legislative action).
% ABSENT_VOICES: Functionalist scholars and judges, administrative law experts, and the public who benefit from agency expertise and regulation. They would argue for the necessity of delegation in a complex modern state and the practical impossibility of Congress legislating every detail.
% DISAPPEARANCE_RATIONALE: If this strict formalist reading vanished overnight, Congress would continue to delegate legislative authority to agencies, agencies would regain full legislative and rulemaking capacity, and the balance of power would shift back to a more flexible, administrative state model, as has largely been the practice for decades.
% FOUNDING_PROBLEM: Preventing the concentration of power in any single branch of government and ensuring legislative accountability to the people, as envisioned by the framers of the Constitution.
% FOUNDING_PROBLEM_CORROBORATION: Formalist legal scholars, some political theorists, and conservative advocacy groups attest that the problem of concentrated power and unaccountable bureaucracy is still live. Functionalist scholars and administrative law experts would contest this, arguing the problem has evolved and requires different solutions.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(separation_of_powers_text__formalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(separation_of_powers_text__formalist_reading),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading severely curtails the legislative and rulemaking power of administrative agencies, effectively extracting their capacity to govern. Suppression is also high (0.90) as it requires active judicial enforcement to prevent delegation and invalidate agency actions, suppressing alternative models of governance. Theater ratio is low (0.10) because formalist proponents genuinely believe in the structural truth of this interpretation and are not merely performing; their actions are consistent with their stated beliefs. Accessibility collapse is high (0.90) as it aims to eliminate the alternative of flexible delegation. Resistance is high (0.75) from agencies, functionalist legal scholars, and those who benefit from agency regulation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of formalist judges and their allies, this constraint restores constitutional order and prevents tyranny, operating as a fundamental 'mountain' of governance. From the perspective of administrative agencies, the executive, and the public, it operates as a 'snare' that dismantles effective governance and extracts regulatory capacity, leading to a less responsive and protected society. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Formalist judges, the legislative branch (which theoretically reclaims power), and industries seeking less regulation are the primary beneficiaries. Administrative agencies, the executive branch (which relies on agencies for policy implementation), and the public who benefit from agency-led regulation are the primary victims. The constraint subsidizes the former by restricting the latter.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_interpretation,
    'Is the strict separation of powers, as interpreted by formalists, a genuine natural law of governance embedded in the Constitution, or a constructed legal interpretation that benefits identifiable actors?',
    'Analysis of historical constitutional practice, comparative constitutional law, and the evolving needs of modern governance. If consistent practice and necessity point to flexibility, it''s a construction.',
    'If a construction, the ''mountain'' claim is a false summit, and the constraint''s true classification would be more extractive (e.g., Snare or Tangled Rope), reflecting its active enforcement and identifiable victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_interpretation, conceptual, 'Ambiguity between claimed natural law and constructed interpretation.').

omega_variable(
    delegation_necessity_empirical,
    'Is legislative delegation to administrative agencies truly necessary for effective governance in a complex modern state, or can Congress effectively legislate on all technical matters?',
    'Empirical studies of legislative capacity, policy outcomes in jurisdictions with strict non-delegation, and expert testimony on the technical complexity of modern regulation.',
    'If delegation is empirically necessary, the formalist reading imposes an unworkable burden on Congress, leading to regulatory gaps and societal harm, further supporting a high extractiveness and suppression profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_necessity_empirical, empirical, 'Empirical necessity of legislative delegation.').

omega_variable(
    original_intent_ambiguity,
    'What did the framers of the Constitution *actually* intend regarding the precise boundaries of legislative delegation, given the vastly different governmental context of the 18th century?',
    'Further historical and textual analysis of founding-era documents, debates, and early governmental practice, acknowledging the limitations of applying 18th-century concepts to modern administrative structures.',
    'If original intent is found to be ambiguous or to permit some forms of delegation, it weakens the foundational premise of the formalist reading, potentially shifting its classification towards a more contested or constructed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, conceptual, 'Ambiguity of framers'' original intent on delegation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 1935, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1935, separation_of_powers_text__formalist_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(sepa_tr_t1960, separation_of_powers_text__formalist_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(sepa_tr_t1985, separation_of_powers_text__formalist_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(sepa_tr_t2000, separation_of_powers_text__formalist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__formalist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(sepa_tr_t2025, separation_of_powers_text__formalist_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1935, separation_of_powers_text__formalist_reading, base_extractiveness, 1935, 0.4).
narrative_ontology:measurement(sepa_be_t1960, separation_of_powers_text__formalist_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(sepa_be_t1985, separation_of_powers_text__formalist_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(sepa_be_t2000, separation_of_powers_text__formalist_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__formalist_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(sepa_be_t2025, separation_of_powers_text__formalist_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1935, separation_of_powers_text__formalist_reading, suppression_requirement, 1935, 0.5).
narrative_ontology:measurement(sepa_su_t1960, separation_of_powers_text__formalist_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(sepa_su_t1985, separation_of_powers_text__formalist_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(sepa_su_t2000, separation_of_powers_text__formalist_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__formalist_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(sepa_su_t2025, separation_of_powers_text__formalist_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, administrative_state_regulatory_capacity).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, executive_branch_policy_implementation).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is the formalist reading of the 'separation_of_powers_text' kernel, which also includes functionalist and unitary executive readings. Each reading instantiates a distinct constraint with different structural properties and impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
