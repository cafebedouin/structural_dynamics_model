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
 *   domain: Constitutional Law / Political Philosophy / Education Policy
 *
 * SUMMARY:
 *   This constraint represents the 'colorblind' reading of the Equal
 *   Protection Clause of the Fourteenth Amendment, which holds that all
 *   governmental racial classifications are presumptively unconstitutional,
 *   treating individuals as rights-bearers independent of group membership.
 *   It asserts that the Constitution mandates formal equality, where race
 *   should not be a factor in law or policy. This reading is one of several
 *   competing interpretations of the Equal Protection Clause.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.15).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.75).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Clause: Colorblind Reading").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "Constitutional Law / Political Philosophy / Education Policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, '22cd1143-df3b-4341-846b-fa5fc8a50af7').
narrative_ontology:cs_kernel_codification('22cd1143-df3b-4341-846b-fa5fc8a50af7', fixed_text).
narrative_ontology:cs_authority_grounding('22cd1143-df3b-4341-846b-fa5fc8a50af7', lineage).
narrative_ontology:cs_interpretation_layer_present('22cd1143-df3b-4341-846b-fa5fc8a50af7').
narrative_ontology:cs_reading_relation('22cd1143-df3b-4341-846b-fa5fc8a50af7', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('22cd1143-df3b-4341-846b-fa5fc8a50af7', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('22cd1143-df3b-4341-846b-fa5fc8a50af7', foundational, individual_rights_supremacy).
narrative_ontology:cs_axiom_status(individual_rights_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('22cd1143-df3b-4341-846b-fa5fc8a50af7', individual_rights_supremacy, deontological).
narrative_ontology:cs_axiom('22cd1143-df3b-4341-846b-fa5fc8a50af7', foundational, racial_classifications_inherently_suspect).
narrative_ontology:cs_axiom_status(racial_classifications_inherently_suspect, holdable).
narrative_ontology:cs_axiom_grounding('22cd1143-df3b-4341-846b-fa5fc8a50af7', racial_classifications_inherently_suspect, conventional).
narrative_ontology:cs_reference_frame('22cd1143-df3b-4341-846b-fa5fc8a50af7', post_brown_formal_equality).
narrative_ontology:cs_drift_state('22cd1143-df3b-4341-846b-fa5fc8a50af7', contemporary_judicial_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('22cd1143-df3b-4341-846b-fa5fc8a50af7', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, colorblind_advocates).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, race_conscious_policy_advocates).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, institutions_using_race_conscious_policies).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, formal_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Equal Protection Clause, responsible for interpreting and enforcing the colorblind reading through its rulings, striking down policies deemed to violate it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Legal and political groups who champion the colorblind interpretation, seeing it as the true meaning of equality and benefiting from its legal victories that align with their ideological goals.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, colorblind_advocates, beneficiary,
    powerful, generational, mobile, national).

% Groups and individuals who advocate for policies that consider race to achieve diversity or remedy historical discrimination. They bear the cost of legal challenges and policy invalidations under the colorblind reading.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, race_conscious_policy_advocates, payer,
    organized, biographical, constrained, national).

% Universities, government agencies, and other entities that implement race-conscious programs (e.g., affirmative action). They face legal challenges and are forced to dismantle or modify their policies, incurring compliance costs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, institutions_using_race_conscious_policies, payer,
    institutional, biographical, constrained, national).

% The abstract concept of individuals whose rights are protected by the colorblind reading, regardless of their racial identity. This is the ideal beneficiary, though not an active agent.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individuals_as_rights_bearers, observer,
    powerless, generational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(equal_protection_clause__colorblind_reading, individuals_as_rights_bearers).

% Academics who analyze the Equal Protection Clause and its various interpretations, contributing to the intellectual debate but not directly enforcing or being subject to the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear, uniform legal standard for governmental action regarding race, ensuring that individuals are treated equally under the law without regard to group membership.
% TRANSFER_FUNCTION: Transfers the legal burden of justification to any governmental policy that uses racial classifications, effectively transferring the benefit of non-discrimination to all individuals and the cost of policy invalidation to those advocating for race-conscious measures.
% ABSENT_VOICES: Those who argue that formal colorblindness is insufficient to address systemic racial inequality, or that race-conscious policies are essential for achieving substantive equality and diversity, are often marginalized or legally silenced by this interpretation.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished, the legal landscape regarding race and government action would be fundamentally altered. Courts would lack a clear standard, potentially leading to a proliferation of race-conscious policies or a return to explicit racial classifications, necessitating a complete re-evaluation of anti-discrimination law.
% FOUNDING_PROBLEM: To prevent governmental discrimination based on race and ensure equal treatment under the law, particularly in the aftermath of slavery and during the Civil Rights era.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the colorblind reading argue the problem of racial discrimination remains live, requiring strict adherence to individual non-discrimination. Critics (e.g., advocates for remedial or diversity readings) contend the original problem has evolved into systemic inequality, which colorblindness fails to address, and that the constraint now serves to block necessary remedies. This contestation is evident in ongoing legal and political debates, and in dissenting judicial opinions.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).
:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because, from this reading's perspective, it primarily enforces a neutral rule of non-discrimination, not extracting rents. Suppression is high (0.75) because it actively forbids and strikes down race-conscious policies, effectively suppressing alternatives to colorblindness. Theater ratio is low (0.1) as the judicial enforcement is generally direct and functional, not performative. Accessibility collapse is high (0.85) as it aims to eliminate race-conscious alternatives. Resistance is moderate (0.6) due to ongoing legal and political challenges from other readings. The slight fluctuations in extractiveness and suppression reflect periods of more or less aggressive judicial enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of colorblind advocates, this constraint is a pure 'rope' that coordinates legal principles for universal benefit. From the perspective of race-conscious policy advocates and institutions, it operates as a 'snare' or 'tangled rope,' extracting the ability to address systemic inequalities and imposing costs through policy invalidation. The Supreme Court, as agenda-setter, views its role as upholding the constitutional text, while critics see it as making policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Colorblind advocates are beneficiaries (d near 0.0) as the constraint aligns with their ideological and legal goals. Race-conscious policy advocates and institutions are targets (d near 1.0) as they bear the direct costs of policy invalidation and legal challenges. Individuals as rights-bearers are an abstract beneficiary, representing the ideal outcome of the reading. Legal scholars are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate is to prevent racial discrimination. Its status is 'contested' because while the original problem of overt discrimination is largely addressed, critics argue new forms of systemic inequality require different approaches. The colorblind reading, by strictly adhering to its original mandate, is seen by some as having outlived its functional utility for contemporary problems, potentially becoming a 'piton' that maintains a formal ideal while failing to address substantive issues, though its active enforcement prevents full atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblindness_vs_substantive_equality,
    'Is formal colorblindness sufficient to achieve substantive racial equality, or does it perpetuate existing disparities by ignoring historical context?',
    'Longitudinal empirical studies tracking racial disparities in outcomes (e.g., education, wealth, health) under strict colorblind regimes versus regimes permitting race-conscious policies.',
    'If colorblindness is shown to exacerbate disparities, its claim to universal benefit would be undermined, potentially reclassifying it as more extractive for certain groups. If it demonstrably reduces disparities, its ''rope'' claim would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblindness_vs_substantive_equality, empirical, 'Whether formal colorblindness achieves its stated goal of equality in practice.').

omega_variable(
    judicial_role_interpretation,
    'Should the Supreme Court''s role be limited to a strict, textualist interpretation of ''equal protection'' (colorblindness), or should it interpret the clause dynamically to address evolving societal conditions and systemic inequalities?',
    'This is a conceptual and preference-based question, resolvable only through shifts in legal philosophy within the judiciary or through constitutional amendment, rather than empirical data.',
    'A shift towards a dynamic interpretation would weaken the colorblind reading''s authority, potentially leading to its reclassification as a ''piton'' or ''snare'' from the perspective of its original proponents, while strengthening alternative readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_role_interpretation, conceptual, 'The fundamental disagreement over the proper role of judicial interpretation in constitutional law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__colorblind_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_clause__colorblind_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_clause__colorblind_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(equa_tr_t2020, equal_protection_clause__colorblind_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_clause__colorblind_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.12).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__colorblind_reading, base_extractiveness, 1990, 0.14).
narrative_ontology:measurement(equa_be_t2000, equal_protection_clause__colorblind_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(equa_be_t2010, equal_protection_clause__colorblind_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(equa_be_t2020, equal_protection_clause__colorblind_reading, base_extractiveness, 2020, 0.17).
narrative_ontology:measurement(equa_be_t2024, equal_protection_clause__colorblind_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__colorblind_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(equa_su_t2000, equal_protection_clause__colorblind_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(equa_su_t2010, equal_protection_clause__colorblind_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(equa_su_t2020, equal_protection_clause__colorblind_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(equa_su_t2024, equal_protection_clause__colorblind_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, voting_rights_act_interpretation).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, racial_gerrymandering_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel, each with different structural properties and classifications. See also: equal_protection_clause__remedial_reading and equal_protection_clause__diversity_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
