% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: 14th Amendment Equal Protection (Formal Equality Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'formal equality' reading of the
 *   Fourteenth Amendment's Equal Protection Clause, which prohibits explicit
 *   state racial or status classification unless justified by a compelling
 *   government interest. While it coordinates state action towards
 *   non-discrimination, it also extracts the ability of state actors to
 *   implement race-conscious remedies aimed at addressing systemic
 *   inequalities. The claimed type is Tangled Rope because it serves a
 *   genuine coordination function (preventing overt discrimination) but also
 *   involves asymmetric extraction from those seeking to use state power for
 *   affirmative action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.45).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.6).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "14th Amendment Equal Protection (Formal Equality Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '90768455-06a1-4d48-bdcc-118ba8da4d01').
narrative_ontology:cs_kernel_codification('90768455-06a1-4d48-bdcc-118ba8da4d01', fixed_text).
narrative_ontology:cs_authority_grounding('90768455-06a1-4d48-bdcc-118ba8da4d01', lineage).
narrative_ontology:cs_interpretation_layer_present('90768455-06a1-4d48-bdcc-118ba8da4d01').
narrative_ontology:cs_reading_relation('90768455-06a1-4d48-bdcc-118ba8da4d01', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('90768455-06a1-4d48-bdcc-118ba8da4d01', foundational, colorblind_constitution).
narrative_ontology:cs_axiom_status(colorblind_constitution, holdable).
narrative_ontology:cs_axiom_grounding('90768455-06a1-4d48-bdcc-118ba8da4d01', colorblind_constitution, deontological).
narrative_ontology:cs_axiom('90768455-06a1-4d48-bdcc-118ba8da4d01', foundational, state_neutrality_principle).
narrative_ontology:cs_axiom_status(state_neutrality_principle, holdable).
narrative_ontology:cs_axiom_grounding('90768455-06a1-4d48-bdcc-118ba8da4d01', state_neutrality_principle, conventional).
narrative_ontology:cs_reference_frame('90768455-06a1-4d48-bdcc-118ba8da4d01', post_civil_rights_era_colorblindness).
narrative_ontology:cs_drift_state('90768455-06a1-4d48-bdcc-118ba8da4d01', contemporary_diversity_equity_inclusion_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('90768455-06a1-4d48-bdcc-118ba8da4d01', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, citizens_seeking_non_discrimination).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_avoiding_explicit_classification).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_actors_implementing_race_conscious_remedies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, groups_seeking_affirmative_action).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, state_legislatures).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitution_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, individual_rights_over_group_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Equal Protection Clause, interpreting its meaning and enforcing its application through judicial review. Its rulings define the scope of permissible state action regarding classification.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Must craft laws that do not explicitly classify citizens by race or other status without meeting strict scrutiny. They bear the cost of legal challenges when attempting to implement race-conscious remedies.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_legislatures, payer,
    institutional, generational, constrained, national).

% Benefit from the prohibition of explicit state-sponsored discrimination, ensuring they are treated equally under the law regardless of race or other protected characteristics.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, citizens_seeking_non_discrimination, beneficiary,
    moderate, biographical, mobile, national).

% Advocate for policies designed to address historical and systemic inequality. They bear the cost of having race-conscious remedies constrained or struck down by this reading of Equal Protection.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, groups_seeking_affirmative_action, payer,
    organized, generational, constrained, national).

% Propose an alternative reading of Equal Protection that would require active state intervention to dismantle racial and status hierarchies. Their perspective is largely outside the framework of formal equality, which views such interventions as potentially discriminatory themselves.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_caste_advocates, excluded,
    organized, generational, identity_locked, national).

% Analyze and critique the Supreme Court's interpretation of Equal Protection, debating its historical grounding, practical effects, and philosophical coherence. They do not directly benefit or pay but shape the intellectual landscape.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to ensure that government does not explicitly classify individuals based on race or other protected characteristics without a compelling justification, thereby promoting a principle of formal equality under the law.
% TRANSFER_FUNCTION: Transfers the burden of justifying any explicit classification to the state, and implicitly transfers the ability to implement certain race-conscious remedies away from state actors and groups advocating for them.
% ABSENT_VOICES: Advocates for an anti-caste reading of Equal Protection are structurally excluded from this framework, as their proposals for active state intervention to dismantle hierarchy are often viewed as violating the formal equality principle itself.
% DISAPPEARANCE_RATIONALE: If the Equal Protection Clause, as interpreted through formal equality, vanished, states would be free to enact explicit racial and status classifications without judicial oversight, likely leading to a rapid re-establishment of discriminatory laws and social stratification.
% FOUNDING_PROBLEM: The 14th Amendment was ratified to ensure legal equality for newly freed slaves, preventing states from enacting discriminatory 'Black Codes' and guaranteeing equal protection of the laws.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Reconstruction era, legislative debates surrounding the 14th Amendment, and ongoing civil rights litigation against various forms of discrimination corroborate the enduring relevance of the founding problem, though its manifestations have evolved.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the cost borne by those whose race-conscious policies are constrained or invalidated. Suppression (0.60) is significant, as the legal framework actively suppresses both explicit discrimination and certain forms of affirmative action. The theater ratio (0.10) is low, indicating that the constraint is genuinely enforced and not merely performative. The increasing extractiveness and suppression over the interval reflect a hardening of the formal equality interpretation in judicial precedent, making it more difficult to implement race-conscious policies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of citizens seeking non-discrimination, this constraint is a protective Rope, ensuring fair treatment. However, from the perspective of groups seeking affirmative action, it operates as a Snare, preventing necessary corrective measures. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court, as the agenda setter, benefits from maintaining its interpretive authority. Citizens seeking non-discrimination are beneficiaries, as the constraint protects them from explicit state-sponsored bias. State legislatures and groups seeking affirmative action are payers/victims, as their efforts to address inequality through race-conscious means are often curtailed by this reading. Anti-caste advocates are excluded, as their framework for Equal Protection is fundamentally at odds with the formal equality approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as the ''formal_equality_reading'' of the 14th Amendment''s Equal Protection Clause?',
    'Analysis of judicial opinions, legislative history, and legal scholarship to confirm the consistent application of a ''colorblind'' principle and the rejection of explicit classifications.',
    'If misidentified, the classification of this constraint would shift to reflect the true dominant reading, potentially altering its extractiveness and beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific interpretive framework being analyzed.').

omega_variable(
    structural_delta_impact_anti_caste_reading,
    'How would the ''anti_caste_reading'' structurally alter the victim set and extractiveness of Equal Protection?',
    'Hypothetical application of the anti-caste framework to current legal challenges, identifying which state actions would become permissible and which groups would shift from victim to beneficiary.',
    'Under an anti-caste reading, ''state_actors_implementing_race_conscious_remedies'' would likely shift from victims to beneficiaries, and the overall extractiveness of the constraint for those seeking to address systemic inequality would decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_impact_anti_caste_reading, conceptual, 'Examines the counterfactual impact of an alternative kernel reading.').

omega_variable(
    disagreement_location_formal_vs_substantive,
    'Is the core disagreement between the formal equality and anti-caste readings primarily about the means (explicit classification) or the ends (dismantling hierarchy)?',
    'Detailed analysis of legal arguments from both sides to pinpoint whether the dispute centers on the permissibility of race-conscious measures or the ultimate goal of racial justice.',
    'If the disagreement is primarily about means, a compromise on implementation might be possible. If it''s about fundamental ends, the readings are more likely to remain in direct conflict, with one foreclosing the other''s policy goals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_formal_vs_substantive, conceptual, 'Locates the fundamental point of contention between sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(four_tr_t1987, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1987, 0.1).
narrative_ontology:measurement(four_tr_t1996, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1996, 0.1).
narrative_ontology:measurement(four_tr_t2005, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(four_tr_t2014, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(four_tr_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(four_be_t1987, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1987, 0.38).
narrative_ontology:measurement(four_be_t1996, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1996, 0.41).
narrative_ontology:measurement(four_be_t2005, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2005, 0.43).
narrative_ontology:measurement(four_be_t2014, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement(four_be_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(four_su_t1987, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1987, 0.53).
narrative_ontology:measurement(four_su_t1996, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1996, 0.56).
narrative_ontology:measurement(four_su_t2005, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(four_su_t2014, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2014, 0.59).
narrative_ontology:measurement(four_su_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2023, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two sibling readings of the 14th Amendment's Equal Protection Clause. This 'formal equality' reading focuses on prohibiting explicit state classifications, while the 'anti-caste' reading focuses on dismantling systemic hierarchy. Both are live interpretations within constitutional law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
