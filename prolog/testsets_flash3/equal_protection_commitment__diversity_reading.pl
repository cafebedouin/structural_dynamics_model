% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection: Diversity as Compelling State Interest (Diversity Reading)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'diversity reading' of the Equal
 *   Protection Clause, which permits the consideration of race as one factor
 *   among many in university admissions to achieve educational diversity. It
 *   is a procedural constraint, granting discretion rather than mandating
 *   outcomes. The metrics reflect a low-to-moderate extractiveness, as the
 *   primary 'cost' is the complexity and opacity of holistic review for
 *   applicants, rather than direct financial extraction. Suppression is low,
 *   as alternatives (e.g., race-neutral diversity strategies) are not
 *   entirely foreclosed, but the legal framework constrains the options. This
 *   reading is one of several competing interpretations of the Equal
 *   Protection Clause, each with distinct structural implications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.15).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection: Diversity as Compelling State Interest (Diversity Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'a526976f-9ad8-436d-862a-24e20255602b').
narrative_ontology:cs_kernel_codification('a526976f-9ad8-436d-862a-24e20255602b', fixed_text).
narrative_ontology:cs_authority_grounding('a526976f-9ad8-436d-862a-24e20255602b', lineage).
narrative_ontology:cs_interpretation_layer_present('a526976f-9ad8-436d-862a-24e20255602b').
narrative_ontology:cs_reading_relation('a526976f-9ad8-436d-862a-24e20255602b', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('a526976f-9ad8-436d-862a-24e20255602b', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('a526976f-9ad8-436d-862a-24e20255602b', foundational, diversity_as_compelling_state_interest).
narrative_ontology:cs_axiom_status(diversity_as_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('a526976f-9ad8-436d-862a-24e20255602b', diversity_as_compelling_state_interest, deontological).
narrative_ontology:cs_axiom('a526976f-9ad8-436d-862a-24e20255602b', secondary, holistic_review_as_permissible_means).
narrative_ontology:cs_axiom_status(holistic_review_as_permissible_means, holdable).
narrative_ontology:cs_axiom_grounding('a526976f-9ad8-436d-862a-24e20255602b', holistic_review_as_permissible_means, conventional).
narrative_ontology:cs_reference_frame('a526976f-9ad8-436d-862a-24e20255602b', bakke_grutter_precedent).
narrative_ontology:cs_drift_state('a526976f-9ad8-436d-862a-24e20255602b', contemporary_sfafa_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a526976f-9ad8-436d-862a-24e20255602b', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, diverse_student_body_advocates).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, all_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer admissions policies, seeking to achieve educational diversity by considering race as one factor among many in a holistic review process. They benefit from the discretion to shape their student bodies according to their educational mission.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities, agenda_setter,
    institutional, generational, constrained, national).

% Subject to a holistic review process where their racial identity may be considered. While no individual is explicitly excluded based on race, the process can obscure the specific weight given to various factors, leading to perceived unfairness or lack of transparency for some.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, all_applicants, payer,
    powerless, immediate, constrained, national).

% Benefit from policies that allow for the consideration of race to achieve educational diversity, believing it enriches the learning environment and prepares students for a diverse society. They actively defend this reading of equal protection.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, diverse_student_body_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue that any consideration of race in state action, even for benign purposes, violates the principle of equal protection. They are excluded from the direct implementation of diversity policies but actively challenge them through legal and political means.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% Believe that race-conscious measures are necessary to dismantle systemic subordination and address historical injustices, going beyond mere diversity. They find the diversity reading insufficient but may strategically support it as a partial step.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, remedial_justice_advocates, excluded,
    organized, generational, constrained, national).

% The ultimate arbiter of equal protection jurisprudence, whose rulings define the scope and limits of race-conscious policies. Its composition and interpretive methodologies significantly influence the stability and evolution of this constraint.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the admissions practices of universities with the constitutional mandate of equal protection, allowing for the pursuit of educational diversity while navigating prohibitions against racial discrimination.
% TRANSFER_FUNCTION: Transfers discretion to universities to consider race as one factor in admissions, from a strict colorblind interpretation that would forbid such consideration. It also transfers the burden of a complex, holistic review process to all applicants.
% ABSENT_VOICES: Advocates for a strictly colorblind interpretation of the Constitution, and those who believe more robust race-conscious remedies are needed to address systemic inequality, are structurally excluded from the direct implementation of this reading's policies, though they exert influence through litigation and public discourse.
% DISAPPEARANCE_RATIONALE: If this reading of equal protection vanished, universities would lose the legal basis for considering race in admissions, forcing a complete overhaul of their diversity strategies. This would likely lead to less diverse student bodies and significant legal challenges from advocates on both sides of the issue.
% FOUNDING_PROBLEM: The problem of reconciling the constitutional guarantee of equal protection with the desire to achieve educational diversity and address the lingering effects of historical racial discrimination in higher education.
% FOUNDING_PROBLEM_CORROBORATION: Universities and educational policy experts attest to the ongoing challenge of achieving diversity without explicit race-conscious measures. Legal scholars and civil rights organizations corroborate the persistent tension between formal equality and substantive equity, supporting the view that the problem remains live, albeit with evolving interpretations.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low-moderate (0.28) because the constraint primarily affects the process of admissions, not a direct financial transfer. The 'cost' is borne by all applicants through a less transparent, more complex review process, and by universities through increased administrative burden and legal risk. Suppression (0.15) is also low, as the constraint does not actively prevent alternative approaches to diversity, but rather defines the permissible boundaries for race-conscious ones. Theater ratio (0.1) is low, indicating that the stated purpose of achieving diversity is largely genuine, though the legal justifications may sometimes involve rhetorical maneuvering.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of universities and diversity advocates, this constraint is a necessary and beneficial coordination mechanism for achieving educational goals within constitutional limits. From the perspective of some applicants or colorblind advocates, it may be seen as an unfair or unconstitutional imposition that complicates merit-based admissions. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are beneficiaries (d near 0.0) as they gain the discretion to pursue their educational missions through diverse student bodies. All applicants are payers (d near 1.0) as they bear the costs of a complex, potentially opaque admissions process. Advocates for diversity are beneficiaries, while advocates for colorblindness or more robust remedies are excluded, as their preferred interpretations are not directly implemented by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_compelling_interest_stability,
    'Is ''educational diversity'' a sufficiently stable and compelling state interest to justify race-conscious measures under strict scrutiny, or is its legal foundation eroding?',
    'Future Supreme Court rulings on affirmative action cases, or legislative action clarifying the scope of diversity as a state interest.',
    'If the compelling interest is deemed unstable or insufficient, this reading would be foreclosed, shifting the constraint towards a more colorblind interpretation or requiring new justifications for race-conscious policies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diversity_compelling_interest_stability, empirical, 'The legal stability of diversity as a compelling state interest.').

omega_variable(
    holistic_review_transparency,
    'To what extent does ''holistic review'' genuinely consider race as ''one factor among many'' versus operating as a de facto quota or set-aside?',
    'Detailed audits of admissions data, internal university communications, and testimony from admissions officers under oath.',
    'If holistic review is found to be a pretext for quotas, the constraint would shift towards a Snare, as the coordination story (holistic review) would be revealed as cover for extraction (from those excluded by de facto quotas).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holistic_review_transparency, empirical, 'Transparency and genuine application of holistic review in admissions.').

omega_variable(
    remedial_vs_diversity_framing,
    'Is the ''diversity reading'' a genuine, distinct interpretation, or is it a strategic framing to achieve remedial goals that would otherwise be unconstitutional?',
    'Analysis of legal arguments and policy outcomes: if the outcomes consistently align with remedial goals beyond what diversity alone would achieve, it suggests a strategic framing.',
    'If it''s primarily a strategic framing, the constraint''s true nature might be closer to the ''remedial reading'' but operating under a less robust legal justification, potentially increasing its fragility or perceived extractiveness for those who oppose remedial measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_diversity_framing, conceptual, 'Whether diversity is a genuine goal or a proxy for remedial justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__diversity_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__diversity_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__diversity_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__diversity_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__diversity_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__diversity_reading, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__diversity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__diversity_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__diversity_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__diversity_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
