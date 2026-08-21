% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__colorblind_reading, []).

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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection: Color-Blind Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint instantiates the 'color-blind' reading of the Equal
 *   Protection Clause, asserting that the Constitution categorically forbids
 *   state use of racial classifications regardless of purpose. This
 *   interpretation, primarily advanced by a majority on the Supreme Court,
 *   mandates formal equality and prohibits race-conscious policies, even
 *   those designed to remedy past discrimination or achieve diversity. It is
 *   one reading of the broader 'equal_protection_kernel' and stands in direct
 *   opposition to 'remedial_reading' and 'antisubordination_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.8).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.9).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, snare).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection: Color-Blind Reading").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '9c1c2bcd-5e27-446c-9b08-522bfb67f2d7').
narrative_ontology:cs_kernel_codification('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', fixed_text).
narrative_ontology:cs_authority_grounding('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', lineage).
narrative_ontology:cs_interpretation_layer_present('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7').
narrative_ontology:cs_reading_relation('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_reading_relation('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_axiom('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', foundational, racial_neutrality_is_equality).
narrative_ontology:cs_axiom_status(racial_neutrality_is_equality, holdable).
narrative_ontology:cs_axiom_grounding('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', racial_neutrality_is_equality, deontological).
narrative_ontology:cs_axiom('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', foundational, individual_rights_over_group_rights).
narrative_ontology:cs_axiom_status(individual_rights_over_group_rights, holdable).
narrative_ontology:cs_axiom_grounding('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', individual_rights_over_group_rights, deontological).
narrative_ontology:cs_reference_frame('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', post_civil_rights_formal_equality).
narrative_ontology:cs_drift_state('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9c1c2bcd-5e27-446c-9b08-522bfb67f2d7', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, dominant_social_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, universities_seeking_diversity).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, civil_rights_advocates).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, individual_meritocracy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Equal Protection Clause to forbid all state-sponsored racial classifications, regardless of purpose. Actively enforces this reading by striking down race-conscious policies, thereby shaping legal and social outcomes across the nation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, national).

% Bear the costs of this interpretation as it removes pathways for addressing systemic inequalities and historical discrimination. Their ability to achieve substantive equality is constrained by the formal color-blindness mandate, making exit from the affected social structures difficult.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_groups, payer,
    powerless, generational, identity_locked, national).

% Are compelled to abandon race-conscious admissions and hiring policies, making it harder to achieve educational and institutional diversity goals. They face legal challenges if they attempt to implement policies that indirectly consider race.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, universities_seeking_diversity, payer,
    organized, biographical, constrained, national).

% Benefit from the removal of race-conscious policies, which can reduce competition for educational and professional opportunities. This interpretation reinforces existing social hierarchies by treating formal equality as sufficient.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, dominant_social_groups, beneficiary,
    powerful, generational, mobile, national).

% Bear the costs of litigating against the color-blind interpretation and advocating for alternative approaches to equality. Their efforts are often met with judicial resistance, limiting their ability to effect change through legal channels.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocates, payer,
    organized, generational, constrained, national).

% Propose alternative interpretations of the Equal Protection Clause that focus on dismantling systemic subordination. Their arguments are often marginalized or explicitly rejected by the courts enforcing the color-blind reading, effectively excluding their perspective from legal doctrine.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, antisubordination_scholars, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, universal standard for state action regarding race, aiming to ensure all individuals are treated identically under the law without explicit racial distinctions.
% TRANSFER_FUNCTION: Transfers opportunities and resources from historically excluded groups (by denying remedial pathways) to dominant social groups (by reinforcing existing competitive advantages), under the guise of formal equality.
% ABSENT_VOICES: Scholars and advocates of antisubordination and critical race theory are largely absent from the judicial discourse that entrenches this reading. They would argue that true equality requires addressing historical and systemic disadvantages, not merely ignoring race.
% DISAPPEARANCE_RATIONALE: If this color-blind interpretation vanished overnight, state and federal entities would likely re-evaluate and implement race-conscious policies to address disparities, leading to significant shifts in admissions, hiring, and resource allocation across various sectors. The legal landscape of civil rights would fundamentally reorganize.
% FOUNDING_PROBLEM: The Equal Protection Clause was established to prevent state-sponsored racial discrimination and ensure equal treatment under the law, particularly in the aftermath of slavery and the Civil War.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the color-blind reading argue the founding problem (racial discrimination) is still live and best addressed by strict neutrality. Critics, including civil rights organizations and legal historians, argue that the original intent was to achieve substantive equality, and the color-blind reading has co-opted the clause to perpetuate existing inequalities, thus shifting the problem rather than solving it. This contestation is evident in ongoing legal debates and scholarly critiques from outside the benefiting parties.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the transfer of opportunities away from historically excluded groups by dismantling race-conscious programs. Suppression (0.9) is severe because this reading actively forbids and strikes down alternative approaches to equality, leaving little room for maneuver. The theater ratio (0.1) is low because the enforcement of this interpretation is direct and functional, not performative; courts genuinely act to eliminate race-conscious policies. Accessibility collapse is high (0.85) as legal avenues for race-conscious remedies are largely foreclosed. Resistance (0.7) is substantial, evidenced by ongoing litigation and advocacy from civil rights groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Supreme Court majority and dominant social groups, this constraint is a just application of constitutional principles, ensuring fairness and individual meritocracy. From the perspective of historically excluded groups and civil rights advocates, it is a snare that perpetuates systemic inequality by ignoring historical context and denying necessary remedies.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court majority acts as the agenda-setter, enforcing this interpretation. Dominant social groups are beneficiaries, as the removal of race-conscious policies often benefits them in competitive environments. Historically excluded groups, universities seeking diversity, and civil rights advocates are victims/payers, bearing the costs of lost opportunities and legal battles. Antisubordination scholars are excluded, as their alternative framings are not adopted by the enforcing authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'snare' prevents mislabeling the color-blind reading as a 'rope' or 'mountain' of natural law. While framed as a neutral application of law, its high extractiveness from identifiable victims and active suppression of alternatives reveal its coercive and asymmetric nature, rather than a benign coordination function or an unchangeable truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_substantive_equality,
    'Does the color-blind application of the Equal Protection Clause achieve substantive equality, or does it merely enforce formal equality while perpetuating historical disparities?',
    'Longitudinal empirical studies tracking socioeconomic outcomes, educational attainment, and representation for historically excluded groups under color-blind regimes versus race-conscious policies.',
    'If substantive disparities persist or worsen, it would undermine the claim that color-blindness achieves true equality, strengthening arguments for alternative interpretations. If disparities genuinely diminish, it would support the color-blind reading''s efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality, empirical, 'Ambiguity regarding the actual impact of color-blindness on societal equality.').

omega_variable(
    judicial_intent_vs_effect,
    'Is the judicial intent behind the color-blind reading genuinely to eliminate all racial discrimination, or does its effect primarily serve to protect existing power structures and privileges?',
    'Historical analysis of judicial opinions, dissenting arguments, and external critiques, focusing on the consistency between stated intent and observed outcomes over time. This is a conceptual omega, as ''intent'' is often debated.',
    'If the effect consistently diverges from stated neutral intent to the detriment of historically excluded groups, it would expose the reading as a mechanism of extraction rather than a neutral application of law, reinforcing its ''snare'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_intent_vs_effect, conceptual, 'The gap between the stated purpose of color-blindness and its actual societal effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1978, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1988, equal_protection_kernel__colorblind_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement(equa_tr_t1998, equal_protection_kernel__colorblind_reading, theater_ratio, 1998, 0.11).
narrative_ontology:measurement(equa_tr_t2008, equal_protection_kernel__colorblind_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_kernel__colorblind_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(equa_tr_t2028, equal_protection_kernel__colorblind_reading, theater_ratio, 2028, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.65).
narrative_ontology:measurement(equa_be_t1988, equal_protection_kernel__colorblind_reading, base_extractiveness, 1988, 0.7).
narrative_ontology:measurement(equa_be_t1998, equal_protection_kernel__colorblind_reading, base_extractiveness, 1998, 0.75).
narrative_ontology:measurement(equa_be_t2008, equal_protection_kernel__colorblind_reading, base_extractiveness, 2008, 0.78).
narrative_ontology:measurement(equa_be_t2018, equal_protection_kernel__colorblind_reading, base_extractiveness, 2018, 0.79).
narrative_ontology:measurement(equa_be_t2028, equal_protection_kernel__colorblind_reading, base_extractiveness, 2028, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.7).
narrative_ontology:measurement(equa_su_t1988, equal_protection_kernel__colorblind_reading, suppression_requirement, 1988, 0.78).
narrative_ontology:measurement(equa_su_t1998, equal_protection_kernel__colorblind_reading, suppression_requirement, 1998, 0.83).
narrative_ontology:measurement(equa_su_t2008, equal_protection_kernel__colorblind_reading, suppression_requirement, 2008, 0.86).
narrative_ontology:measurement(equa_su_t2018, equal_protection_kernel__colorblind_reading, suppression_requirement, 2018, 0.88).
narrative_ontology:measurement(equa_su_t2028, equal_protection_kernel__colorblind_reading, suppression_requirement, 2028, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equal_protection_kernel', alongside 'remedial_reading' and 'antisubordination_reading'. Each reading represents a distinct structural claim about the Equal Protection Clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
