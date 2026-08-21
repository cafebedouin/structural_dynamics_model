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
 *   human_readable: Equal Protection Clause: Colorblind Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'colorblind' reading of the Equal
 *   Protection Clause, which holds that the Constitution categorically
 *   forbids state use of racial classifications, regardless of purpose. This
 *   interpretation views any race-conscious policy as inherently
 *   discriminatory, leading to the invalidation of affirmative action
 *   programs. The constraint is framed as a Tangled Rope because it provides
 *   a coordination function (a clear, universal standard for state action)
 *   but also involves significant asymmetric extraction from historically
 *   excluded groups and requires active enforcement by the judiciary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.65).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.7).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Clause: Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d').
narrative_ontology:cs_kernel_codification('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', fixed_text).
narrative_ontology:cs_authority_grounding('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', lineage).
narrative_ontology:cs_interpretation_layer_present('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d').
narrative_ontology:cs_reading_relation('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', foundational, racial_classifications_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classifications_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', racial_classifications_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', foundational, formal_equality_is_sufficient_for_justice).
narrative_ontology:cs_axiom_status(formal_equality_is_sufficient_for_justice, holdable).
narrative_ontology:cs_axiom_grounding('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', formal_equality_is_sufficient_for_justice, deontological).
narrative_ontology:cs_reference_frame('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', post_civil_war_amendments_originalism).
narrative_ontology:cs_drift_state('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', contemporary_judicial_precedent, gap(stable, minor, true)).
narrative_ontology:cs_created_at('20a843b1-3ec1-4e77-b0f8-fb16ed9fa68d', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, majority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, institutions_seeking_formal_equality).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of constitutional meaning, which has increasingly adopted and enforced the colorblind reading, striking down race-conscious policies. Its decisions shape the legal landscape for all other actors.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from policies that forbid racial classifications, as it removes considerations that might favor minority applicants in competitive environments like university admissions. They are treated identically under formal equality principles.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, majority_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Bear the cost of the colorblind reading, as it eliminates remedial pathways designed to address systemic disadvantages and promote diversity. Their access to certain educational or professional opportunities is constrained by the inability to consider race as a factor.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_minority_applicants, payer,
    powerless, generational, identity_locked, national).

% Universities and other public institutions that seek to achieve diverse student bodies or workforces find their ability to implement race-conscious policies severely restricted or forbidden, forcing them to adopt race-neutral alternatives that are often less effective.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity, payer,
    organized, generational, constrained, national).

% Public institutions that align with the colorblind principle find their policies affirmed and protected, reinforcing their approach to admissions and hiring based solely on race-neutral criteria.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_seeking_formal_equality, beneficiary,
    organized, generational, mobile, national).

% Advocate for policies that consider race to remedy past discrimination and achieve substantive equality. Their arguments are largely foreclosed by the colorblind reading, pushing them to seek legislative or alternative legal strategies.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocates_remedial, excluded,
    organized, generational, constrained, national).

% Interpret and defend the colorblind reading as the most faithful interpretation of the Equal Protection Clause, emphasizing individual rights and formal equality over group-based remedies. They contribute to the intellectual grounding of the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, legal_scholars_colorblind, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally applicable standard for state action regarding race, aiming to prevent arbitrary or discriminatory classifications and ensure formal equality before the law.
% TRANSFER_FUNCTION: Transfers opportunities and advantages from historically excluded minority groups (who would benefit from race-conscious policies) to majority groups (who benefit from race-neutral policies), by mandating a 'colorblind' approach.
% ABSENT_VOICES: Advocates for substantive equality and anti-subordination readings of the Equal Protection Clause are structurally marginalized in the current legal discourse dominated by the colorblind interpretation. They would argue that ignoring race perpetuates existing inequalities.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished, state institutions would immediately re-evaluate and likely re-implement race-conscious policies to address diversity and historical disadvantage, leading to significant shifts in admissions, hiring, and resource allocation across the country.
% FOUNDING_PROBLEM: The Equal Protection Clause was established to ensure that states do not deny any person within their jurisdiction the equal protection of the laws, primarily in response to racial discrimination against formerly enslaved people.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and proponents of the colorblind reading argue that the founding problem is still live, requiring strict adherence to race-neutrality to prevent new forms of discrimination. Civil rights advocates and legal historians, from outside the benefiting parties, argue that the original intent was to dismantle racial hierarchy, and the colorblind reading has shifted the problem's definition to maintain existing power structures.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the colorblind reading, while appearing neutral, disproportionately impacts historically marginalized groups by removing tools to address systemic inequality. Suppression is also high, as the Supreme Court actively enforces this interpretation, striking down policies that deviate from it and limiting the options for institutions seeking diversity. The low theater ratio reflects that the enforcement is direct and impactful, not merely performative. The increasing extractiveness and suppression over the interval reflect the judiciary's hardening stance on colorblindness.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of majority applicants and institutions seeking formal equality, this constraint is a legitimate application of constitutional principles ensuring fairness. From the perspective of minority applicants and institutions seeking diversity, it is an extractive mechanism that perpetuates existing inequalities under the guise of neutrality. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court, as the agenda-setter, benefits from establishing a clear, judicially enforceable rule. Majority applicants and institutions prioritizing formal equality are beneficiaries, as the constraint aligns with their interests. Historically excluded minority applicants and institutions seeking diversity are victims, bearing the costs of lost opportunities and restricted policy options. Civil rights advocates for remedial approaches are excluded, as their arguments are largely foreclosed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formal_equality,
    'Is the Equal Protection Clause primarily concerned with formal equality (treating all individuals identically) or substantive equality (addressing historical and systemic disadvantages to achieve equal outcomes)?',
    'A shift in judicial philosophy or a constitutional amendment explicitly clarifying the clause''s primary aim. Empirical studies on the impact of colorblind policies on racial disparities could also inform the debate.',
    'If substantive equality is prioritized, the colorblind reading would be reclassified as a Snare, as its ''coordination'' function would be seen as cover for perpetuating inequality. If formal equality is definitively affirmed, its Tangled Rope classification would be reinforced, with its extraction seen as a necessary cost of universal application.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_formal_equality, conceptual, 'Ambiguity in the core purpose of the Equal Protection Clause.').

omega_variable(
    causal_link_to_disparity,
    'To what extent do current racial disparities in education and opportunity directly result from past state-sanctioned discrimination, and to what extent are they attributable to other factors?',
    'Comprehensive historical and sociological research, coupled with robust statistical analysis, to establish or refute direct causal links between historical discrimination and contemporary disparities.',
    'Strong evidence of direct causation would weaken the ''colorblind'' argument by highlighting the ongoing effects of past discrimination, potentially shifting the constraint towards a Snare if its ''neutrality'' is seen as actively harmful. Weak evidence would bolster the colorblind reading''s claim to neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_link_to_disparity, empirical, 'The empirical basis for claims of ongoing systemic discrimination vs. race-neutral outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(equa_be_t1995, equal_protection_kernel__colorblind_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.6).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__colorblind_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(equa_su_t1995, equal_protection_kernel__colorblind_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.65).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__colorblind_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, university_admissions_criteria).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Its strict colorblind interpretation directly influences and often forecloses the remedial and anti-subordination readings in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
