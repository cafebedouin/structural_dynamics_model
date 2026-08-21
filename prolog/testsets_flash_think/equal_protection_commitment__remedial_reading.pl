% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection: Remedial Reading (Anti-Subordination)
 *   domain: constitutional_law/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which interprets it as forbidding the perpetuation of a racial
 *   caste system and permitting (or requiring) race-conscious measures to
 *   dismantle systemic subordination. It acknowledges that formal equality is
 *   insufficient to overcome historical and ongoing discrimination,
 *   necessitating active state intervention. The constraint operates as a
 *   Tangled Rope, coordinating efforts to achieve substantive equality while
 *   extracting from historically privileged groups who lose preferential
 *   access.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.55).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.7).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection: Remedial Reading (Anti-Subordination)").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'f97a6083-0be5-4788-a05f-49721d66d7d0').
narrative_ontology:cs_kernel_codification('f97a6083-0be5-4788-a05f-49721d66d7d0', fixed_text).
narrative_ontology:cs_authority_grounding('f97a6083-0be5-4788-a05f-49721d66d7d0', lineage).
narrative_ontology:cs_interpretation_layer_present('f97a6083-0be5-4788-a05f-49721d66d7d0').
narrative_ontology:cs_reading_relation('f97a6083-0be5-4788-a05f-49721d66d7d0', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('f97a6083-0be5-4788-a05f-49721d66d7d0', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('f97a6083-0be5-4788-a05f-49721d66d7d0', foundational, anti_subordination_principle).
narrative_ontology:cs_axiom_status(anti_subordination_principle, holdable).
narrative_ontology:cs_axiom_grounding('f97a6083-0be5-4788-a05f-49721d66d7d0', anti_subordination_principle, deontological).
narrative_ontology:cs_axiom('f97a6083-0be5-4788-a05f-49721d66d7d0', foundational, race_conscious_remedies_permissible).
narrative_ontology:cs_axiom_status(race_conscious_remedies_permissible, holdable).
narrative_ontology:cs_axiom_grounding('f97a6083-0be5-4788-a05f-49721d66d7d0', race_conscious_remedies_permissible, deontological).
narrative_ontology:cs_reference_frame('f97a6083-0be5-4788-a05f-49721d66d7d0', substantive_equality_framework).
narrative_ontology:cs_drift_state('f97a6083-0be5-4788-a05f-49721d66d7d0', contemporary_judicial_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f97a6083-0be5-4788-a05f-49721d66d7d0', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_preferential_access).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, equal_citizenship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from race-conscious measures designed to overcome past and present discrimination, gaining access to opportunities previously denied due to systemic barriers. Their ability to exit systemic disadvantage is directly tied to the effectiveness of these remedies.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    powerless, generational, constrained, national).

% Implement and defend race-conscious policies to achieve substantive equality and dismantle racial hierarchies. They face legal challenges and political resistance, constraining their ability to act but also solidifying their role as enforcers of this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the costs of remedial measures, experiencing denial of preferential access or advantages they previously held due to historical systems of privilege. They often perceive these measures as 'reverse discrimination' and seek legal avenues to challenge them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_preferential_access, payer,
    powerful, biographical, constrained, national).

% Advocate for a strictly colorblind interpretation of equal protection, arguing that any state use of racial classification is unconstitutional. Their position is fundamentally at odds with the remedial reading's premise and is therefore excluded from its internal logic.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_advocates, excluded,
    organized, generational, analytical, national).

% Support race-conscious measures but primarily on the grounds of achieving educational or institutional diversity as a compelling state interest. While their policy outcomes might overlap, their underlying rationale differs from the anti-subordination principle of this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, diversity_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicate legal challenges to race-conscious measures, interpreting the scope and limits of the Equal Protection Clause. Their decisions shape the practical application and legitimacy of this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action and public policy to actively dismantle systemic racial subordination and achieve substantive equality for historically marginalized groups, rather than merely formal equality.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and institutional access from historically privileged groups (who benefited from prior systems of racial hierarchy) to historically subordinated groups, as a remedy for past and present discrimination.
% ABSENT_VOICES: Advocates for a strictly colorblind interpretation of the Constitution are structurally excluded from the framework of this reading, as their core premise (no race-conscious measures) directly contradicts the remedial approach. Similarly, diversity advocates, while sometimes supporting similar policies, are excluded from this reading's anti-subordination rationale.
% DISAPPEARANCE_RATIONALE: If this reading of Equal Protection vanished, state actors would lose a key legal and moral justification for proactive anti-subordination measures. This would likely lead to a re-entrenchment of systemic inequalities, a shift towards purely colorblind or diversity-only approaches, and a significant reorganization of civil rights advocacy and policy.
% FOUNDING_PROBLEM: The historical perpetuation of a racial caste system and systemic subordination in the United States, despite formal legal equality, which prevented full and equal citizenship for all.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, social scientists, and legal scholars outside of the direct beneficiaries (e.g., the NAACP Legal Defense and Educational Fund, scholars of critical race theory, sociological studies on racial inequality) consistently attest to the ongoing existence of systemic subordination and the need for remedial measures. This corroboration supports the claim that the founding problem remains live.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is substantial because remedial measures reallocate opportunities, which is perceived as a cost by those who previously benefited from the status quo. Suppression (0.70) is high because dismantling systemic subordination and implementing race-conscious remedies often requires active enforcement against resistance from those who benefit from existing hierarchies. The theater ratio (0.20) is low, reflecting that the measures are generally intended to be functional and achieve real-world changes, though some performative aspects may exist. Resistance (0.80) is high, as these measures are frequently challenged legally and politically. Accessibility collapse (0.65) is moderately high for historically privileged groups, as their alternatives to accepting the reallocation of opportunities are limited by legal mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and state actors implementing remedies, this constraint is a necessary mechanism for justice and substantive equality. From the perspective of historically privileged groups, it is an unfair imposition that extracts from them. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Rope or Scaffold, and payers experiencing it as a Snare or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups and state actors implementing remedies are beneficiaries (low d), as the constraint actively works to improve their position or enable their mission. Historically privileged groups denied preferential access are targets/payers (high d), as the constraint directly reallocates resources away from them. Colorblind and diversity advocates are excluded, as their core rationales are not aligned with this reading's anti-subordination premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Snare (ignoring its genuine coordination function in dismantling subordination) or a pure Rope (ignoring the asymmetric extraction from historically privileged groups). It acknowledges both the coordination of anti-subordination efforts and the inherent costs borne by specific groups, which requires active enforcement to maintain. The 'live' status of the founding problem (systemic subordination) indicates that mandatrophy has not occurred, though the specific mechanisms and scope of remedies are continually contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_subordination,
    'What constitutes ''subordination'' and when is it sufficiently remedied to no longer justify race-conscious measures?',
    'Empirical sociological and economic studies demonstrating the absence of significant racial disparities in key life outcomes (e.g., wealth, health, education, incarceration rates), combined with a legal consensus on the cessation of systemic discrimination.',
    'If subordination is deemed fully remedied, the justification for race-conscious measures under this reading would diminish, potentially leading to a reclassification towards a more colorblind approach or a Piton if measures persist without justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_subordination, conceptual, 'Ambiguity in the definition and duration of systemic racial subordination.').

omega_variable(
    boundary_of_remedy_vs_reverse_discrimination,
    'At what point do race-conscious remedial measures cross the line into ''reverse discrimination'' or create new forms of impermissible racial classification?',
    'Further judicial clarification and legislative action establishing clear, measurable thresholds for the scope and duration of remedial programs, balancing anti-subordination goals with individual rights.',
    'If a clear boundary is established, it would refine the scope of permissible extraction from historically privileged groups. If the boundary is consistently challenged or found to be overreached, it could weaken the legitimacy and persistence of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_of_remedy_vs_reverse_discrimination, preference, 'The contested boundary between legitimate remedial action and impermissible reverse discrimination.').

omega_variable(
    efficacy_of_race_conscious_measures,
    'Are race-conscious measures truly effective at dismantling systemic subordination, or do they produce unintended negative consequences that undermine their stated goals?',
    'Longitudinal empirical studies evaluating the causal impact of specific race-conscious policies on racial disparities and social cohesion, compared to race-neutral alternatives.',
    'If measures are found to be ineffective or counterproductive, the instrumental grounding for this reading would be challenged, potentially leading to a shift towards alternative approaches or a re-evaluation of the constraint''s claimed coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_race_conscious_measures, empirical, 'Empirical efficacy of race-conscious measures in achieving anti-subordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__remedial_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1970, equal_protection_commitment__remedial_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(equa_tr_t1986, equal_protection_commitment__remedial_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(equa_tr_t2002, equal_protection_commitment__remedial_reading, theater_ratio, 2002, 0.22).
narrative_ontology:measurement(equa_tr_t2015, equal_protection_commitment__remedial_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__remedial_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__remedial_reading, base_extractiveness, 1954, 0.45).
narrative_ontology:measurement(equa_be_t1970, equal_protection_commitment__remedial_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(equa_be_t1986, equal_protection_commitment__remedial_reading, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(equa_be_t2002, equal_protection_commitment__remedial_reading, base_extractiveness, 2002, 0.58).
narrative_ontology:measurement(equa_be_t2015, equal_protection_commitment__remedial_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__remedial_reading, base_extractiveness, 2023, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__remedial_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(equa_su_t1970, equal_protection_commitment__remedial_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(equa_su_t1986, equal_protection_commitment__remedial_reading, suppression_requirement, 1986, 0.75).
narrative_ontology:measurement(equa_su_t2002, equal_protection_commitment__remedial_reading, suppression_requirement, 2002, 0.72).
narrative_ontology:measurement(equa_su_t2015, equal_protection_commitment__remedial_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__remedial_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'equal_protection_commitment' kernel. Each reading has a unique structural profile and set of stakeholders, and they interact through logical foreclosure or coexistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
