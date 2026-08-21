% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__colorblind_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection Clause: Colorblind Reading
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'colorblind' reading of the Equal
 *   Protection Clause of the Fourteenth Amendment, which holds that all
 *   governmental racial classifications are presumptively unconstitutional,
 *   treating individuals as rights-bearers independent of group membership.
 *   This reading asserts that the Constitution mandates formal equality,
 *   where race should not be a factor in governmental decision-making. It is
 *   one interpretation of a contested kernel, with sibling readings (remedial
 *   and diversity) offering alternative approaches to achieving equality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.65).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.7).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause: Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'ae4bde8d-8df5-425c-aade-51065d0ba385').
narrative_ontology:cs_kernel_codification('ae4bde8d-8df5-425c-aade-51065d0ba385', fixed_text).
narrative_ontology:cs_authority_grounding('ae4bde8d-8df5-425c-aade-51065d0ba385', lineage).
narrative_ontology:cs_interpretation_layer_present('ae4bde8d-8df5-425c-aade-51065d0ba385').
narrative_ontology:cs_reading_relation('ae4bde8d-8df5-425c-aade-51065d0ba385', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('ae4bde8d-8df5-425c-aade-51065d0ba385', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('ae4bde8d-8df5-425c-aade-51065d0ba385', foundational, racial_classifications_presumptively_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classifications_presumptively_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('ae4bde8d-8df5-425c-aade-51065d0ba385', racial_classifications_presumptively_unconstitutional, deontological).
narrative_ontology:cs_axiom('ae4bde8d-8df5-425c-aade-51065d0ba385', foundational, individual_rights_not_group_rights).
narrative_ontology:cs_axiom_status(individual_rights_not_group_rights, holdable).
narrative_ontology:cs_axiom_grounding('ae4bde8d-8df5-425c-aade-51065d0ba385', individual_rights_not_group_rights, deontological).
narrative_ontology:cs_reference_frame('ae4bde8d-8df5-425c-aade-51065d0ba385', post_brown_formal_equality).
narrative_ontology:cs_drift_state('ae4bde8d-8df5-425c-aade-51065d0ba385', contemporary_judicial_precedent, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ae4bde8d-8df5-425c-aade-51065d0ba385', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_individuals).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, civil_rights_advocates_colorblind).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, state_governments).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, racial_minority_groups).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, individual_litigants_seeking_remedy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individual_litigants_challenging_policy).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, formal_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Equal Protection Clause, interpreting its meaning and enforcing the colorblind reading through judicial review. Its decisions shape policy nationwide and are binding on lower courts and governments.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who challenge race-conscious governmental policies (e.g., affirmative action) on colorblind grounds. They benefit when such policies are struck down, but bear the costs and risks of litigation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_litigants_challenging_policy, beneficiary,
    moderate, biographical, constrained, national).

% Individuals who seek race-conscious remedies for historical or systemic discrimination. They bear the cost of the colorblind reading when their claims for such remedies are denied or policies benefiting them are invalidated.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_litigants_seeking_remedy, payer,
    moderate, biographical, constrained, national).

% Governmental entities (state and local) that must comply with the Supreme Court's interpretation of equal protection. They bear the cost of having their race-conscious policies (e.g., in education or contracting) struck down, limiting their ability to address racial disparities.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Groups whose members have historically faced discrimination and who advocate for race-conscious policies to achieve substantive equality. They bear the cost of the colorblind reading when policies designed to benefit them are prohibited, potentially perpetuating existing disparities.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, racial_minority_groups, payer,
    organized, generational, constrained, national).

% Advocacy organizations and legal scholars who champion the colorblind interpretation of equal protection. They benefit from judicial decisions that align with this reading, seeing it as the true fulfillment of constitutional equality.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, civil_rights_advocates_colorblind, beneficiary,
    organized, generational, mobile, national).

% Advocacy organizations and legal scholars who argue for race-conscious policies to achieve remedial or diversity goals. They are structurally excluded from the dominant legal framework of the colorblind reading, as their core arguments are often rejected by the courts.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, civil_rights_advocates_remedial_diversity, excluded,
    organized, generational, constrained, national).

% The abstract concept of all individuals, who are theoretically protected from governmental racial discrimination under the colorblind reading, regardless of their own race. This group represents the universal ideal of formal equality.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, all_individuals, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(equal_protection_clause__colorblind_reading, all_individuals).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__colorblind_reading, civil_rights_advocates_colorblind).
narrative_ontology:fixing_cost_class(equal_protection_clause__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform legal standard that governmental racial classifications are presumptively unconstitutional, providing a clear rule for non-discrimination and formal equality across all individuals.
% TRANSFER_FUNCTION: Transfers the burden of justifying race-conscious policies from the state to individual litigants challenging such policies; it transfers the benefit of non-discrimination to all individuals, while simultaneously transferring the cost of foregone race-conscious remedies to racial minority groups and state governments.
% ABSENT_VOICES: Advocates for race-conscious remedies or diversity rationales are largely absent from the prevailing legal framework of the colorblind reading, as their arguments for substantive equality are often rejected by the courts. They would argue that formal colorblindness perpetuates systemic inequality.
% DISAPPEARANCE_RATIONALE: If the colorblind reading of equal protection vanished overnight, governmental bodies would be free to implement a wide range of race-conscious policies without strict judicial scrutiny. This would fundamentally alter civil rights law, education policy, and other areas across all levels of government, leading to a significant reorganization of legal and social structures.
% FOUNDING_PROBLEM: The Equal Protection Clause was established after the Civil War to prevent governmental discrimination and ensure equal protection of the laws for newly freed slaves, addressing the problem of state-sanctioned racial hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historical documents, and ongoing litigation attest to the enduring problem of racial discrimination and the need for equal protection. However, the *method* of achieving this (colorblindness vs. race-consciousness) is highly contested among legal experts, civil rights organizations, and political actors, with corroboration for the 'live' status coming from diverse sources, though the 'solution' is disputed.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.70 at end) is substantial because this reading actively prohibits race-conscious policies, extracting the ability of state governments and racial minority groups to pursue substantive equality through such means. `suppression` (0.75 at end) is high as it actively suppresses alternative legal interpretations and policy approaches, requiring continuous judicial enforcement. `theater_ratio` is low (0.15) as the judicial function is genuinely about legal interpretation and enforcement, not performance. `accessibility_collapse` is high (0.80) because this reading aims to legally collapse race-conscious alternatives. `resistance` is high (0.75) due to ongoing legal and political challenges from advocates of remedial and diversity-based approaches. The measurements show a general trend of increasing extractiveness and suppression as the colorblind reading has gained judicial dominance over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Supreme Court and colorblind advocates, this reading is a necessary enforcement of fundamental individual rights and formal equality. From the perspective of state governments and racial minority groups, it is an extractive constraint that limits their ability to address historical injustices and achieve substantive equality, effectively creating victims by prohibiting beneficial policies.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court, as the agenda-setter, benefits from its authority to define and enforce this reading. Individual litigants challenging race-conscious policies and civil rights advocates for colorblindness are direct beneficiaries of its enforcement. State governments and racial minority groups are primary payers/victims, as their policy options and desired outcomes are constrained or denied. Individual litigants seeking race-conscious remedies also bear costs when their claims are rejected. 'All individuals' is an abstract beneficiary, representing the ideal of formal equality this reading purports to uphold.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_vs_substantive_equality,
    'Is formal colorblindness, as enforced by this reading, sufficient to achieve the goals of equal protection, or does it impede the achievement of substantive equality by prohibiting necessary race-conscious remedies?',
    'Longitudinal empirical studies comparing social and economic outcomes in jurisdictions with strict colorblind enforcement versus those with more permissive race-conscious policies, alongside legal scholarship on the historical intent of the Equal Protection Clause.',
    'If formal colorblindness is found to impede substantive equality, the constraint''s effective extraction from racial minority groups would be higher, and its classification might shift towards a Snare from their perspective. If it is found sufficient, its coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_vs_substantive_equality, conceptual, 'Ambiguity regarding whether formal colorblindness achieves or hinders the broader goal of equality.').

omega_variable(
    judicial_role_in_social_change,
    'What is the appropriate role of the judiciary in addressing racial inequality: strictly enforcing formal legal principles (colorblindness) or actively shaping social outcomes to achieve substantive equality?',
    'This is a fundamental question of political philosophy and constitutional theory, resolvable through ongoing public debate, shifts in judicial philosophy, and legislative action that clarifies the scope of judicial power.',
    'A shift towards a more activist judicial role would likely lead to a re-evaluation of race-conscious policies, potentially reducing the extractive impact of the colorblind reading. A reinforcement of judicial restraint would solidify its current impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_role_in_social_change, preference, 'Ambiguity regarding the normative role of the judiciary in addressing racial inequality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_clause__colorblind_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_clause__colorblind_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__colorblind_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_clause__colorblind_reading, theater_ratio, 2016, 0.16).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__colorblind_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_clause__colorblind_reading, base_extractiveness, 1954, 0.55).
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.6).
narrative_ontology:measurement(equa_be_t1995, equal_protection_clause__colorblind_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__colorblind_reading, base_extractiveness, 2003, 0.62).
narrative_ontology:measurement(equa_be_t2016, equal_protection_clause__colorblind_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__colorblind_reading, base_extractiveness, 2023, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_clause__colorblind_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(equa_su_t1995, equal_protection_clause__colorblind_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__colorblind_reading, suppression_requirement, 2003, 0.68).
narrative_ontology:measurement(equa_su_t2016, equal_protection_clause__colorblind_reading, suppression_requirement, 2016, 0.72).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__colorblind_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Equal Protection Clause kernel. Its interpretation directly influences the viability and legal status of the remedial and diversity readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
