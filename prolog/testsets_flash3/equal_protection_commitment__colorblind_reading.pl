% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Equal Protection: Colorblindness Principle
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'colorblind' reading of the Equal
 *   Protection Clause, which holds that the Constitution forbids any state
 *   use of racial classification, echoing Justice Harlan's dissent in Plessy
 *   v. Ferguson. This reading views any race-conscious policy, even those
 *   intended to remedy past discrimination or promote diversity, as
 *   inherently discriminatory and unconstitutional. The constraint is
 *   classified as a Tangled Rope because it provides a coordination function
 *   (a clear, universal standard for state action) but also involves
 *   asymmetric extraction (from minority groups and institutions seeking to
 *   address inequality) and requires active enforcement by the judiciary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.45).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.6).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection: Colorblindness Principle").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '272a47eb-3221-4427-b84b-59502160d448').
narrative_ontology:cs_kernel_codification('272a47eb-3221-4427-b84b-59502160d448', fixed_text).
narrative_ontology:cs_authority_grounding('272a47eb-3221-4427-b84b-59502160d448', lineage).
narrative_ontology:cs_interpretation_layer_present('272a47eb-3221-4427-b84b-59502160d448').
narrative_ontology:cs_reading_relation('272a47eb-3221-4427-b84b-59502160d448', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('272a47eb-3221-4427-b84b-59502160d448', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('272a47eb-3221-4427-b84b-59502160d448', foundational, racial_classification_is_inherently_discriminatory).
narrative_ontology:cs_axiom_status(racial_classification_is_inherently_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('272a47eb-3221-4427-b84b-59502160d448', racial_classification_is_inherently_discriminatory, deontological).
narrative_ontology:cs_axiom('272a47eb-3221-4427-b84b-59502160d448', foundational, state_must_be_colorblind).
narrative_ontology:cs_axiom_status(state_must_be_colorblind, holdable).
narrative_ontology:cs_axiom_grounding('272a47eb-3221-4427-b84b-59502160d448', state_must_be_colorblind, deontological).
narrative_ontology:cs_reference_frame('272a47eb-3221-4427-b84b-59502160d448', harlans_original_colorblind_dissent).
narrative_ontology:cs_drift_state('272a47eb-3221-4427-b84b-59502160d448', contemporary_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('272a47eb-3221-4427-b84b-59502160d448', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, majority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_advocates).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, racial_minority_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, institutions_implementing_affirmative_action).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, harlans_plessy_dissent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of constitutional interpretation, responsible for applying the Equal Protection Clause. Its rulings define the scope and limits of state action regarding race. Its decisions are binding on all other state actors.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Individuals, often white or Asian-American, who argue that race-conscious admissions or hiring policies disadvantage them. They benefit from rulings that prohibit such classifications, as it removes a factor that might reduce their chances of admission or employment.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, majority_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Groups that have historically faced discrimination and advocate for race-conscious policies to address systemic inequalities. They bear the cost of colorblind rulings, which limit tools for achieving diversity and inclusion, potentially perpetuating historical disadvantages.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, racial_minority_groups, payer,
    organized, generational, constrained, national).

% Universities, employers, and government agencies that previously used or wished to use race-conscious programs to achieve diversity or remedy past discrimination. They face legal challenges and are forced to dismantle or modify their programs, incurring compliance costs and potentially failing to meet diversity goals.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, institutions_implementing_affirmative_action, payer,
    institutional, biographical, constrained, national).

% Legal scholars, advocacy groups, and political actors who champion the colorblind interpretation of the Constitution. They benefit from the legal and political victories that reinforce their ideological position and shape public policy.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_advocates, beneficiary,
    organized, generational, mobile, national).

% Advocates for the remedial reading of equal protection, who believe the clause permits race-conscious measures to dismantle systemic subordination. Their arguments are often foreclosed or marginalized by the colorblind framework, limiting their ability to shape policy.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, remedial_reading_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal standard for state action regarding race, aiming to prevent arbitrary or discriminatory classifications and ensure equal treatment under the law.
% TRANSFER_FUNCTION: Transfers opportunities (e.g., university admissions, employment) from individuals who might benefit from race-conscious policies to those who would benefit from a strictly colorblind approach. It also transfers legal certainty to institutions by defining permissible boundaries for racial considerations.
% ABSENT_VOICES: Advocates for the remedial reading of equal protection are often excluded from the dominant discourse, as their premise that race-conscious measures are necessary to dismantle subordination is directly challenged by the colorblind framework. Their perspective is often framed as advocating for 'reverse discrimination'.
% DISAPPEARANCE_RATIONALE: If the colorblind interpretation vanished, state institutions would likely reintroduce or expand race-conscious programs, leading to a significant shift in admissions, hiring, and contracting practices. Legal challenges would pivot to different constitutional theories, and the political landscape around race and equality would fundamentally alter.
% FOUNDING_PROBLEM: The problem of racial discrimination and the need to ensure equal protection under the law, particularly in the aftermath of slavery and during the Jim Crow era.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and colorblind advocates attest that the problem of racial discrimination, albeit in new forms, remains live and requires a strict colorblind approach to prevent new forms of discrimination. Remedial and diversity advocates, while disagreeing on the solution, also attest to the ongoing problem of racial inequality, corroborating the 'live' status of the founding problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.45) because this reading imposes significant costs on racial minority groups and institutions that seek to address historical and ongoing inequalities through race-conscious means. Suppression (0.6) is also substantial, as this interpretation actively suppresses alternative approaches to racial justice and limits the policy tools available to state actors. The theater ratio is low (0.1) because the enforcement of this principle is direct and functional, not performative. The claimed type is Tangled Rope, reflecting the dual nature of providing a clear legal standard while simultaneously extracting from specific groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of majority applicants and colorblind advocates, this constraint is a just application of constitutional principle, ensuring fairness and preventing discrimination. From the perspective of racial minority groups and institutions, it is an extractive mechanism that perpetuates inequality by disallowing remedies for systemic issues. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court acts as the agenda-setter, defining and enforcing this interpretation. Majority applicants and colorblind advocates are beneficiaries, as their interests align with the prohibition of race-conscious policies. Racial minority groups and institutions implementing affirmative action are payers, bearing the costs of restricted policy options and legal challenges. Remedial reading advocates are excluded, as their foundational premises are often incompatible with this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing racial discrimination) is still live, but its application (strict colorblindness) is contested. The classification as Tangled Rope prevents mislabeling it as a pure Snare, acknowledging its coordination function in providing a clear legal standard, while still highlighting its extractive and suppressive aspects for certain groups. It also avoids mislabeling it as a pure Rope, which would ignore the significant costs borne by those it extracts from.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblindness_vs_equity,
    'Does a strictly colorblind approach to equal protection genuinely achieve equity, or does it perpetuate existing inequalities by ignoring historical context?',
    'Longitudinal empirical studies comparing social and economic outcomes in jurisdictions with strict colorblind policies versus those with race-conscious policies, controlling for other factors.',
    'If strict colorblindness is shown to perpetuate inequality, the extractiveness and suppression metrics would be re-evaluated upward, potentially shifting the classification towards a Snare. If it demonstrably achieves equity, the metrics might be re-evaluated downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblindness_vs_equity, empirical, 'The empirical effect of colorblind policies on societal equity.').

omega_variable(
    classification_as_harm,
    'Is any state use of racial classification inherently harmful, regardless of intent, or is harm contingent on discriminatory intent or outcome?',
    'Conceptual analysis and jurisprudential debate on the nature of harm in constitutional law, potentially informed by social psychology research on the impact of categorization.',
    'If classification itself is the harm, the current extractiveness and suppression are justified by the reading''s premise. If harm requires discriminatory intent or outcome, the metrics might be too high, as some race-conscious policies might not be inherently harmful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_as_harm, conceptual, 'The conceptual basis of harm in racial classification.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the ''colorblind'' principle a genuine natural law of justice, or a constructed legal interpretation that benefits identifiable agents?',
    'Philosophical inquiry into the foundations of justice and equality, alongside historical and sociological analysis of the development and enforcement of the colorblind doctrine.',
    'If it were a natural law, its extractiveness would be negligible, and it would be reclassified as a Mountain. If it is a constructed interpretation benefiting specific groups, its current classification as a Tangled Rope is appropriate, and its persistence depends on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the colorblind principle is a natural law or a constructed legal doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__colorblind_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__colorblind_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__colorblind_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__colorblind_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__colorblind_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(equa_tr_t50, equal_protection_commitment__colorblind_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__colorblind_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__colorblind_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__colorblind_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__colorblind_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__colorblind_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(equa_be_t50, equal_protection_commitment__colorblind_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__colorblind_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__colorblind_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__colorblind_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__colorblind_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__colorblind_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(equa_su_t50, equal_protection_commitment__colorblind_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equal_protection_commitment' kernel. This 'colorblind_reading' directly influences the viability and interpretation of the 'remedial_reading' and 'diversity_reading' by limiting the scope of permissible race-conscious state action.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
