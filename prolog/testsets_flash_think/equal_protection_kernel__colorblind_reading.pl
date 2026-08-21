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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Clause: Color-Blind Reading
 *   domain: constitutional_law/civil_rights/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'color-blind' reading of the Equal
 *   Protection Clause of the 14th Amendment, which holds that the
 *   Constitution categorically forbids state use of racial classifications
 *   regardless of purpose. This interpretation gained significant judicial
 *   traction, particularly from the late 20th century, leading to the
 *   invalidation of many race-conscious policies, including affirmative
 *   action in university admissions. It is a reading that coordinates a
 *   specific vision of formal equality while extracting from historically
 *   excluded groups by denying remedial pathways.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.78).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.85).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Clause: Color-Blind Reading").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/civil_rights/education_policy").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '95be8ac4-4366-4977-843a-dba6fa269968').
narrative_ontology:cs_kernel_codification('95be8ac4-4366-4977-843a-dba6fa269968', fixed_text).
narrative_ontology:cs_authority_grounding('95be8ac4-4366-4977-843a-dba6fa269968', lineage).
narrative_ontology:cs_interpretation_layer_present('95be8ac4-4366-4977-843a-dba6fa269968').
narrative_ontology:cs_reading_relation('95be8ac4-4366-4977-843a-dba6fa269968', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('95be8ac4-4366-4977-843a-dba6fa269968', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('95be8ac4-4366-4977-843a-dba6fa269968', foundational, state_must_be_race_neutral).
narrative_ontology:cs_axiom_status(state_must_be_race_neutral, holdable).
narrative_ontology:cs_axiom_grounding('95be8ac4-4366-4977-843a-dba6fa269968', state_must_be_race_neutral, deontological).
narrative_ontology:cs_axiom('95be8ac4-4366-4977-843a-dba6fa269968', foundational, formal_equality_is_sufficient_for_justice).
narrative_ontology:cs_axiom_status(formal_equality_is_sufficient_for_justice, holdable).
narrative_ontology:cs_axiom_grounding('95be8ac4-4366-4977-843a-dba6fa269968', formal_equality_is_sufficient_for_justice, deontological).
narrative_ontology:cs_reference_frame('95be8ac4-4366-4977-843a-dba6fa269968', post_civil_war_amendment_era).
narrative_ontology:cs_drift_state('95be8ac4-4366-4977-843a-dba6fa269968', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('95be8ac4-4366-4977-843a-dba6fa269968', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, dominant_social_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, those_opposed_to_affirmative_action).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, diversity_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, state_universities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Equal Protection Clause, enforcing the color-blind interpretation through judicial review and precedent. Its rulings shape state and federal policy, effectively forbidding race-conscious measures.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Bear the costs of the color-blind reading by losing access to remedial pathways designed to address past and ongoing discrimination in areas like education and employment. Their ability to achieve substantive equality is hampered by formal equality.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_groups, payer,
    powerless, generational, constrained, national).

% Benefit from the color-blind reading by ensuring that state action cannot use racial classifications, which often translates to maintaining existing social and economic hierarchies and preventing 'reverse discrimination' claims.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, dominant_social_groups, beneficiary,
    powerful, generational, mobile, national).

% Actively campaign for race-conscious policies to achieve diversity and address historical injustice. They bear the costs of legal challenges and legislative defeats, as their preferred policy tools are deemed unconstitutional by this reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, diversity_advocates, payer,
    organized, generational, constrained, national).

% Must comply with judicial mandates forbidding race-conscious admissions policies. They lose a tool they believe is necessary to achieve educational diversity and often face challenges in maintaining diverse student bodies without it.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, state_universities, payer,
    institutional, biographical, constrained, local).

% Actively support the color-blind reading, viewing it as essential for meritocracy and individual rights. They benefit from the legal prohibition of policies they perceive as discriminatory or unfair.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, those_opposed_to_affirmative_action, beneficiary,
    organized, biographical, mobile, national).

% While active in the broader civil rights contest, their arguments for race-conscious remedies are structurally excluded from the color-blind reading's framework, which deems such approaches unconstitutional per se.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_organizations, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, formally equal standard for state action, preventing any governmental use of racial classifications and ensuring all individuals are treated identically under the law.
% TRANSFER_FUNCTION: Transfers the burden of achieving racial equity from state-led, race-conscious interventions to individual and private efforts, while transferring legal certainty (for some) and perceived fairness (for others) to dominant social groups.
% ABSENT_VOICES: Advocates for substantive equality and anti-subordination, who argue that formal color-blindness perpetuates historical disadvantage and that the state has an obligation to address the effects of past discrimination. Their perspectives are legally foreclosed by this reading.
% DISAPPEARANCE_RATIONALE: If the color-blind reading of the Equal Protection Clause vanished overnight, states would likely reintroduce race-conscious policies in education, employment, and contracting. This would lead to significant shifts in institutional demographics, resource allocation, and legal challenges, fundamentally reorganizing civil rights law and policy.
% FOUNDING_PROBLEM: The Equal Protection Clause was established to prevent state-sponsored racial discrimination, particularly against formerly enslaved people, and to ensure equal protection under the law for all citizens.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and civil rights scholars outside the immediate beneficiaries attest to the original intent to dismantle Jim Crow and establish basic civil rights. However, whether a strictly color-blind interpretation is the most effective or faithful means to achieve this founding problem's goals is heavily disputed by other legal schools and social scientists.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because this reading, by forbidding race-conscious remedies, effectively extracts opportunities and resources from historically excluded groups, who are then left to overcome systemic disadvantages without state intervention. Suppression is very high (0.85) because the Supreme Court actively enforces this interpretation, legally suppressing any alternative race-conscious policies. Theater ratio is low (0.10) as the enforcement is direct and functional, not performative. Accessibility collapse is high (0.90) because the legal framework makes race-conscious alternatives virtually impossible for state actors. Resistance is high (0.70) due to ongoing legal and social challenges from civil rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dominant social groups, this reading ensures fairness and meritocracy by treating all individuals equally. From the perspective of historically excluded groups, it perpetuates existing inequalities by ignoring historical context and systemic barriers, effectively extracting opportunities under the guise of equality. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court, as the agenda-setter, benefits from establishing a clear, judicially enforceable standard. Dominant social groups and those opposed to affirmative action are beneficiaries, as the reading aligns with their interests in formal equality and preventing 'reverse discrimination.' Historically excluded groups, diversity advocates, and state universities are payers, bearing the costs of lost remedial tools and the inability to address systemic inequalities. Civil rights organizations are excluded from the framework's internal logic, as their preferred solutions are deemed unconstitutional.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_substantive_equality,
    'Does a categorical color-blind approach to the Equal Protection Clause genuinely achieve substantive equality, or does it merely entrench existing inequalities by ignoring historical context?',
    'Longitudinal empirical studies comparing social and economic outcomes in jurisdictions with and without race-conscious policies, controlling for other variables. Analysis of whether formal equality alone closes racial gaps.',
    'If formal color-blindness is shown to perpetuate inequality, the extractiveness of this constraint would be re-evaluated as higher, and its coordination function as more illusory, potentially shifting its classification towards a Snare. If it demonstrably leads to substantive equality, its extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_substantive_equality, empirical, 'The efficacy of color-blindness in achieving actual equality.').

omega_variable(
    judicial_construction_vs_original_intent,
    'Is the color-blind reading a faithful interpretation of the original intent of the 14th Amendment, or a judicial construction that evolved to serve contemporary political and social interests?',
    'Further historical and legal scholarship on the legislative history and early interpretations of the 14th Amendment, particularly regarding its application to race-conscious measures beyond the immediate post-Civil War context.',
    'If found to be a later construction, it would strengthen the argument that the constraint''s ''naturalness'' is a cover story, increasing its perceived extractiveness and potentially shifting its classification from Tangled Rope to Snare, especially if beneficiaries are clearly identifiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_construction_vs_original_intent, conceptual, 'The historical and interpretive legitimacy of the color-blind reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__colorblind_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_kernel__colorblind_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__colorblind_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__colorblind_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__colorblind_reading, base_extractiveness, 1954, 0.4).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.6).
narrative_ontology:measurement(equa_be_t1995, equal_protection_kernel__colorblind_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.72).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__colorblind_reading, base_extractiveness, 2016, 0.75).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__colorblind_reading, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.7).
narrative_ontology:measurement(equa_su_t1995, equal_protection_kernel__colorblind_reading, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.82).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__colorblind_reading, suppression_requirement, 2016, 0.84).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Its categorical color-blindness directly forecloses the premises of the remedial and anti-subordination readings, which permit or require race-conscious state action under certain conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
