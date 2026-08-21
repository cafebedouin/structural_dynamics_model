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
 *   human_readable: Equal Protection: Color-Blind Reading
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'color-blind' reading of the Equal
 *   Protection Clause, which holds that the Constitution forbids any state
 *   use of racial classification, regardless of intent. Originating in
 *   Justice Harlan's dissent in Plessy v. Ferguson, this reading gained
 *   prominence in the late 20th and early 21st centuries, leading to rulings
 *   that restrict affirmative action and other race-conscious policies. It is
 *   one reading of the 'equal_protection_commitment' kernel, distinct from
 *   'remedial_reading' and 'diversity_reading'. The core structural delta is
 *   that race-conscious state programs are seen as the harm, and implementing
 *   institutions become perpetrators, leading to moderate-high extraction
 *   from racial minority groups and institutions seeking diversity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.45).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.7).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection: Color-Blind Reading").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '01fb05bc-f5c4-4645-a7b3-d3ff06e665c6').
narrative_ontology:cs_kernel_codification('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', fixed_text).
narrative_ontology:cs_authority_grounding('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', lineage).
narrative_ontology:cs_interpretation_layer_present('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6').
narrative_ontology:cs_reading_relation('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', foundational, racial_classifications_inherently_suspect).
narrative_ontology:cs_axiom_status(racial_classifications_inherently_suspect, holdable).
narrative_ontology:cs_axiom_grounding('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', racial_classifications_inherently_suspect, deontological).
narrative_ontology:cs_axiom('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', foundational, constitution_is_colorblind).
narrative_ontology:cs_axiom_status(constitution_is_colorblind, holdable).
narrative_ontology:cs_axiom_grounding('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', constitution_is_colorblind, deontological).
narrative_ontology:cs_reference_frame('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', harlan_plessy_dissent_framework).
narrative_ontology:cs_drift_state('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', contemporary_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('01fb05bc-f5c4-4645-a7b3-d3ff06e665c6', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, majority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_advocates).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, racial_minority_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, institutions_seeking_diversity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of constitutional meaning, interpreting and enforcing the Equal Protection Clause. Its rulings define the scope of permissible state action regarding race.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Individuals, often white or Asian-American, who argue that race-conscious admissions or hiring policies discriminate against them. They benefit from a color-blind interpretation that removes racial preferences.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, majority_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Legal organizations and political groups that champion a strict color-blind interpretation of the Constitution, viewing any racial classification as inherently discriminatory. They benefit from rulings that align with this view.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_advocates, beneficiary,
    organized, generational, mobile, national).

% Groups that have historically faced discrimination and argue that color-blind policies ignore systemic inequalities, hindering efforts to achieve substantive equality. They bear the cost of policies that prevent race-conscious remedies.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, racial_minority_groups, payer,
    powerless, generational, constrained, national).

% Universities, employers, and government agencies that seek to implement race-conscious programs to achieve diversity or address historical disadvantage. They face legal challenges and restrictions under a color-blind interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, institutions_seeking_diversity, payer,
    institutional, biographical, constrained, national).

% Advocates for race-conscious measures to dismantle systemic subordination and achieve substantive equality. Their arguments for affirmative action are largely foreclosed by the color-blind reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, remedial_justice_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally applicable standard for state action regarding race, aiming to prevent arbitrary or discriminatory classifications and ensure equal treatment under the law.
% TRANSFER_FUNCTION: Transfers the burden of achieving racial equity from state institutions (which are forbidden from using race-conscious means) to individuals or non-racialized mechanisms. It also transfers opportunities from racial minority groups to majority applicants in competitive contexts.
% ABSENT_VOICES: Advocates for a remedial or diversity-focused reading of equal protection are present in public discourse but are structurally excluded from the 'color-blind' interpretation's internal logic, which views their proposed solutions as part of the problem. Their arguments for race-conscious remedies are deemed unconstitutional.
% DISAPPEARANCE_RATIONALE: If the color-blind interpretation vanished, state institutions would likely re-evaluate and potentially re-implement race-conscious policies, leading to significant shifts in admissions, hiring, and resource allocation. The legal landscape around civil rights would fundamentally change.
% FOUNDING_PROBLEM: The original problem was the state's use of racial classifications to enforce segregation and discrimination, leading to the 'separate but equal' doctrine and systemic racial subordination.
% FOUNDING_PROBLEM_CORROBORATION: The problem of state-sanctioned racial discrimination is widely acknowledged as having been live at the founding of the Equal Protection Clause. However, its current status as 'live' (in terms of requiring a color-blind solution) is contested by those who argue that systemic inequalities persist and require race-conscious remedies, as evidenced by ongoing academic debate, legal challenges, and public policy discussions from diverse legal scholars and civil rights organizations.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate-high because this reading, while framed as preventing discrimination, effectively extracts opportunities from racial minority groups by prohibiting remedies for systemic inequality. Suppression (0.70) is high due to the active judicial enforcement required to strike down race-conscious policies, which are often supported by significant portions of the population and institutions. The theater ratio (0.10) is low as the enforcement is direct and functional, not performative. The claimed type is 'tangled_rope' because it coordinates a universal standard of non-discrimination while simultaneously extracting from those who seek to use race-conscious measures to achieve equity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of color-blind advocates, this constraint is a 'rope' that ensures fairness and prevents discrimination. From the perspective of racial minority groups and institutions seeking diversity, it operates as a 'snare' or 'tangled_rope' that perpetuates existing inequalities by disallowing necessary remedies. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court acts as the agenda-setter, defining and enforcing the constraint. Majority applicants and color-blind advocates are beneficiaries, as their interests align with the prohibition of racial classifications. Racial minority groups and institutions seeking diversity are payers, bearing the costs of this interpretation through restricted access to opportunities or limitations on their policy tools. Remedial justice advocates are excluded, as their perspective is fundamentally at odds with the color-blind premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (preventing racial discrimination) is still considered live. However, the interpretation of what constitutes 'discrimination' and what remedies are permissible is highly contested. The 'color-blind' reading prevents mislabeling its coordination function (a universal standard of non-discrimination) as pure extraction, but the high extractiveness and suppression metrics indicate that this coordination comes at a significant cost to specific groups, suggesting it operates more as a tangled rope than a pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_vs_substantive_equality,
    'Does a color-blind approach to equal protection achieve substantive equality, or does it perpetuate existing inequalities by ignoring historical context?',
    'Longitudinal studies tracking socioeconomic outcomes for racial minority groups under color-blind policies versus policies allowing race-conscious remedies. Legal and philosophical analysis of ''equality of opportunity'' versus ''equality of outcome''.',
    'If a color-blind approach is shown to perpetuate inequality, the extractiveness of this constraint would be re-evaluated as higher, and its classification might shift closer to a Snare for affected groups. If it demonstrably leads to substantive equality, its extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_vs_substantive_equality, empirical, 'Ambiguity regarding the ultimate impact of color-blind policies on racial equity.').

omega_variable(
    judicial_role_in_racial_policy,
    'Is the judiciary the appropriate institution to define and enforce a strict color-blind standard, or should legislative and executive branches have more latitude in crafting race-conscious policies?',
    'Comparative analysis of judicial versus legislative approaches to racial equity in other democracies. Constitutional scholarship on judicial restraint versus activism in civil rights.',
    'If the judiciary is deemed to overstep, the ''suppression'' metric might be re-evaluated as more illegitimate, and the ''agenda_setter'' role of the Supreme Court might be seen as more extractive of democratic processes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_role_in_racial_policy, conceptual, 'Ambiguity regarding the proper institutional locus of authority for racial policy.').

omega_variable(
    kernel_reading_divergence,
    'How do the ''colorblind_reading'', ''remedial_reading'', and ''diversity_reading'' of the Equal Protection Clause diverge in their core premises and practical effects?',
    'Detailed legal and philosophical analysis of each reading''s foundational axioms, historical development, and impact on case law and social policy. This is a conceptual omega that clarifies the structural differences between the sibling constraints.',
    'Clarifying the divergence helps to precisely define the boundaries and unique extractive/coordinative properties of each reading, preventing conflation and enabling accurate per-reading classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Documenting the structural differences between the three readings of the Equal Protection Clause kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1896, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1896, equal_protection_commitment__colorblind_reading, theater_ratio, 1896, 0.05).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__colorblind_reading, theater_ratio, 1954, 0.08).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_commitment__colorblind_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1896, equal_protection_commitment__colorblind_reading, base_extractiveness, 1896, 0.1).
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__colorblind_reading, base_extractiveness, 1954, 0.2).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.4).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__colorblind_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1896, equal_protection_commitment__colorblind_reading, suppression_requirement, 1896, 0.1).
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__colorblind_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.65).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__colorblind_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
