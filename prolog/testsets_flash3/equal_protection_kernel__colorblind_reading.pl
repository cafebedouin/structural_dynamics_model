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
 *   interpretation treats all individuals identically under the law,
 *   emphasizing formal equality. It is one of several competing readings of
 *   the Equal Protection Clause, and its increasing judicial enforcement has
 *   significant implications for affirmative action and diversity policies.
 *   The claimed type 'tangled_rope' reflects the dual function: it
 *   coordinates a uniform legal standard while extracting from historically
 *   excluded groups by denying remedial pathways.
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
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '0fde7765-a546-49bd-83a6-7e365cb525fb').
narrative_ontology:cs_kernel_codification('0fde7765-a546-49bd-83a6-7e365cb525fb', fixed_text).
narrative_ontology:cs_authority_grounding('0fde7765-a546-49bd-83a6-7e365cb525fb', lineage).
narrative_ontology:cs_interpretation_layer_present('0fde7765-a546-49bd-83a6-7e365cb525fb').
narrative_ontology:cs_reading_relation('0fde7765-a546-49bd-83a6-7e365cb525fb', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('0fde7765-a546-49bd-83a6-7e365cb525fb', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('0fde7765-a546-49bd-83a6-7e365cb525fb', foundational, racial_classifications_are_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classifications_are_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('0fde7765-a546-49bd-83a6-7e365cb525fb', racial_classifications_are_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('0fde7765-a546-49bd-83a6-7e365cb525fb', foundational, formal_equality_is_the_sole_constitutional_mandate).
narrative_ontology:cs_axiom_status(formal_equality_is_the_sole_constitutional_mandate, holdable).
narrative_ontology:cs_axiom_grounding('0fde7765-a546-49bd-83a6-7e365cb525fb', formal_equality_is_the_sole_constitutional_mandate, deontological).
narrative_ontology:cs_reference_frame('0fde7765-a546-49bd-83a6-7e365cb525fb', post_civil_war_formal_equality).
narrative_ontology:cs_drift_state('0fde7765-a546-49bd-83a6-7e365cb525fb', contemporary_judicial_enforcement, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0fde7765-a546-49bd-83a6-7e365cb525fb', '').
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

% Benefit from policies that forbid racial classifications, as it removes any consideration of race that might favor historically excluded groups in competitive processes like university admissions. They are treated identically under formal equality.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, majority_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Bear the cost of the colorblind reading, as it eliminates remedial pathways designed to address the lingering effects of past and present discrimination. Their access to educational and economic opportunities is constrained by a system that ignores historical context.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_minority_applicants, payer,
    powerless, generational, identity_locked, national).

% Are constrained in their ability to implement race-conscious policies to achieve educational diversity or remedy past discrimination. They face legal challenges and potential penalties if they deviate from the colorblind mandate, forcing them to adopt race-neutral alternatives that are often less effective.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity, payer,
    organized, generational, constrained, national).

% Benefit from the clarity and legal protection offered by the colorblind reading, as it aligns with their preference for race-neutral policies and avoids the complexities and potential legal risks of race-conscious programs. They can implement policies based purely on individual merit without considering race.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_seeking_formal_equality, beneficiary,
    organized, generational, mobile, national).

% Advocate for policies that address systemic racial inequality, often through race-conscious means. The colorblind reading directly opposes their goals, effectively excluding their preferred policy solutions from legal consideration and forcing them into a defensive posture.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, uniform standard for state action regarding race, ensuring that all individuals are treated identically under the law, thereby simplifying legal compliance and promoting a sense of formal fairness.
% TRANSFER_FUNCTION: Transfers opportunities and advantages from historically excluded minority groups to majority applicants by eliminating remedial pathways, while transferring legal certainty and simplified compliance to institutions that prefer race-neutral policies.
% ABSENT_VOICES: Advocates for substantive equality and anti-subordination would argue that the colorblind reading perpetuates existing inequalities by ignoring historical context and systemic barriers. Their arguments for race-conscious remedies are effectively silenced by the legal framework established by this reading.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished, institutions would likely reintroduce race-conscious policies to address diversity and historical disadvantage, leading to a significant shift in admissions, hiring, and contracting practices. The legal landscape would become more complex, and civil rights advocacy would gain new avenues for action.
% FOUNDING_PROBLEM: The Equal Protection Clause was established to ensure that states do not deny any person 'the equal protection of the laws,' primarily in response to post-Civil War racial discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the colorblind reading argue that the founding problem (racial discrimination) is best addressed by prohibiting all racial classifications. Opponents (civil rights advocates, some legal scholars) argue that the founding problem of systemic racial inequality persists and is exacerbated by a colorblind approach, citing ongoing disparities in education, wealth, and justice. Historical legal texts and contemporary social science data from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.65) is substantial because the colorblind reading, while appearing neutral, disproportionately impacts historically marginalized groups by dismantling mechanisms designed to counteract systemic disadvantages. Suppression (0.70) is high due to active judicial enforcement that invalidates race-conscious policies, effectively suppressing alternative approaches to equality. Theater ratio is low (0.10) because the constraint is genuinely enforced and has clear, direct effects, not primarily performative maintenance. The increasing extractiveness and suppression over time reflect the hardening of this judicial interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of majority applicants and institutions seeking formal equality, this reading is a 'rope' that ensures fair, equal treatment. From the perspective of minority applicants and diversity-seeking institutions, it operates as a 'snare' that perpetuates inequality by ignoring its historical roots. The engine's computation of 'tangled_rope' reflects this hybrid nature, coordinating a legal standard while extracting from specific groups.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court (agenda_setter) benefits from establishing a clear, judicially manageable standard. Majority applicants and institutions seeking formal equality are beneficiaries, as the reading aligns with their interests. Historically excluded minority applicants and institutions seeking diversity are victims, bearing the costs of lost remedial pathways and constrained policy options. Civil rights advocates are excluded, as their preferred solutions are legally foreclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading prevents mislabeling coordination as pure extraction by acknowledging its function in establishing a clear, uniform legal standard. However, it also prevents mislabeling extraction as pure coordination by highlighting the asymmetric costs borne by historically excluded groups and the active enforcement required to maintain this specific interpretation, despite ongoing resistance and calls for alternative approaches.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_vs_present_neutrality,
    'To what extent does a ''colorblind'' approach, by ignoring historical and systemic racial discrimination, perpetuate rather than remedy inequality?',
    'Longitudinal studies tracking socioeconomic outcomes for historically excluded groups under colorblind policies versus race-conscious policies, coupled with legal scholarship on the original intent and evolving purpose of the Equal Protection Clause.',
    'If ignoring history is shown to perpetuate inequality, the ''extractiveness'' and ''suppression'' metrics would be further validated, potentially shifting the classification towards a ''snare'' for victims. If a colorblind approach is demonstrably effective at achieving substantive equality, the metrics would decrease, supporting a ''rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_context_vs_present_neutrality, empirical, 'The impact of historical context on the ''neutrality'' of colorblind policies.').

omega_variable(
    judicial_activism_vs_interpretation,
    'Is the Supreme Court''s increasing enforcement of the colorblind reading an act of judicial interpretation consistent with constitutional text and precedent, or an act of judicial activism imposing a particular ideological view?',
    'Analysis of legal scholarship across ideological divides, historical review of judicial philosophy, and comparison with international constitutional jurisprudence on equality. This is a conceptual question about the nature of judicial power.',
    'If deemed activism, it would highlight the ''agenda_setter'' role''s power and potentially increase the ''suppression'' metric by revealing a non-textual basis for enforcement. If deemed consistent interpretation, it would reinforce the ''mountain'' aspect of the constraint''s perceived inevitability for some actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_interpretation, conceptual, 'The nature of judicial enforcement of the colorblind reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__colorblind_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__colorblind_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__colorblind_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.4).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__colorblind_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__colorblind_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__colorblind_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.45).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__colorblind_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__colorblind_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__colorblind_reading, suppression_requirement, 2010, 0.67).
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
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Its strict interpretation of colorblindness directly influences the viability and legal status of policies derived from the remedial and antisubordination readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
