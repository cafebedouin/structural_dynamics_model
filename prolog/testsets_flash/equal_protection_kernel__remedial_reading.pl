% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause: Remedial Race-Conscious Action
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'remedial' reading of the Equal Protection
 *   Clause, which permits race-conscious state action when narrowly tailored
 *   to remedy documented historical exclusion or achieve a compelling
 *   diversity interest. This reading allows universities to consider race as
 *   a 'plus factor' in admissions. It is one of three major interpretations
 *   of the Equal Protection Clause kernel, alongside the 'colorblind' and
 *   'antisubordination' readings. The metrics reflect the moderate extraction
 *   and suppression inherent in balancing competing interests and the active
 *   legal defense required to maintain this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.4).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.3).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause: Remedial Race-Conscious Action").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, 'f926949c-20d1-4378-ad18-14f877dd1b9d').
narrative_ontology:cs_kernel_codification('f926949c-20d1-4378-ad18-14f877dd1b9d', fixed_text).
narrative_ontology:cs_authority_grounding('f926949c-20d1-4378-ad18-14f877dd1b9d', lineage).
narrative_ontology:cs_interpretation_layer_present('f926949c-20d1-4378-ad18-14f877dd1b9d').
narrative_ontology:cs_reading_relation('f926949c-20d1-4378-ad18-14f877dd1b9d', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('f926949c-20d1-4378-ad18-14f877dd1b9d', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('f926949c-20d1-4378-ad18-14f877dd1b9d', foundational, racial_classifications_permissible_for_remedy_or_diversity).
narrative_ontology:cs_axiom_status(racial_classifications_permissible_for_remedy_or_diversity, holdable).
narrative_ontology:cs_axiom_grounding('f926949c-20d1-4378-ad18-14f877dd1b9d', racial_classifications_permissible_for_remedy_or_diversity, conventional).
narrative_ontology:cs_axiom('f926949c-20d1-4378-ad18-14f877dd1b9d', foundational, state_has_compelling_interest_in_diversity).
narrative_ontology:cs_axiom_status(state_has_compelling_interest_in_diversity, holdable).
narrative_ontology:cs_axiom_grounding('f926949c-20d1-4378-ad18-14f877dd1b9d', state_has_compelling_interest_in_diversity, instrumental).
narrative_ontology:cs_reference_frame('f926949c-20d1-4378-ad18-14f877dd1b9d', post_bakke_jurisprudence).
narrative_ontology:cs_drift_state('f926949c-20d1-4378-ad18-14f877dd1b9d', post_sfafa_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f926949c-20d1-4378-ad18-14f877dd1b9d', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_seeking_diversity).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, equal_opportunity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policies designed to remedy past discrimination, gaining access to educational and economic opportunities that might otherwise be denied. Their identity is often tied to the historical struggle for equality.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_groups, beneficiary,
    organized, generational, identity_locked, national).

% Implement race-conscious admissions policies to achieve a diverse student body, believing it enriches the educational experience. They bear the administrative burden and legal risk of defending these policies.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_seeking_diversity, agenda_setter,
    institutional, generational, constrained, national).

% Are denied admission to institutions where they might have been accepted under a strictly race-blind process. They bear the direct cost of foregone educational opportunities and often feel unjustly treated.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process, payer,
    powerless, biographical, constrained, national).

% Adjudicate the constitutionality of race-conscious state action, setting the legal boundaries for what constitutes 'narrowly tailored' and 'compelling interest.' Their rulings shape the constraint's application.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Support and defend race-conscious policies as necessary tools to achieve substantive equality and remedy historical injustices. They mobilize public opinion and legal resources.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Oppose race-conscious policies, arguing they violate the principle of individual merit and lead to reverse discrimination. They advocate for colorblind policies and often represent rejected applicants.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, equal_opportunity_advocates, payer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to address the societal effects of historical racial discrimination and achieve educational diversity, providing a framework for institutions to pursue these goals within constitutional limits.
% TRANSFER_FUNCTION: Transfers educational opportunities and social capital from individuals who might otherwise be admitted (often majority-group applicants) to individuals from historically excluded groups, in pursuit of remedial or diversity goals.
% ABSENT_VOICES: Future generations who will inherit the societal structures shaped by these policies, and those who believe that any racial classification is inherently harmful, regardless of intent, are not directly represented in the legal and policy debates.
% DISAPPEARANCE_RATIONALE: If this reading of the Equal Protection Clause vanished, universities would likely revert to strictly race-blind admissions, significantly altering the racial composition of student bodies and potentially exacerbating existing inequalities. The legal landscape for civil rights would be fundamentally reshaped.
% FOUNDING_PROBLEM: The problem of persistent racial inequality and the legacy of systemic discrimination, particularly in access to education, which was not adequately addressed by formally colorblind policies alone.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists, historians, and educational researchers, along with civil rights organizations, consistently document ongoing disparities and the lingering effects of historical exclusion, corroborating that the founding problem remains live. This is often contested by those who believe formal equality is sufficient.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).
:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.4) is moderate, reflecting the transfer of opportunity from some applicants to others, but within a framework that aims for a broader societal good. Suppression (0.3) is also moderate, as the state must actively defend these policies against legal challenges, but it does not involve overt coercion against individuals. The theater ratio (0.1) is low, as the policies genuinely aim to achieve their stated goals, though the 'narrowly tailored' and 'compelling interest' justifications can sometimes become performative in legal arguments. The slight increase and then decrease in extractiveness and suppression over time reflect the shifting legal landscape and public debate, culminating in recent Supreme Court decisions that have narrowed the scope of this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically excluded groups, this reading is a necessary tool for justice and equity. From the perspective of rejected applicants, it is an unfair imposition. Universities view it as a means to achieve educational excellence, while those advocating for colorblindness see it as a violation of individual rights. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and universities seeking diversity are beneficiaries, as the constraint enables policies that serve their interests. Rejected applicants who would have been admitted under a race-blind process are victims, bearing the direct cost of these policies. Courts and civil rights advocates act as agenda-setters and beneficiaries, respectively, shaping and defending the interpretation. Equal opportunity advocates, who oppose race-conscious policies, are effectively payers, as their preferred 'colorblind' outcome is suppressed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_colorblind_framing,
    'Is the Equal Protection Clause primarily a remedial tool to address historical injustice and achieve diversity (this reading), or a categorical prohibition against all racial classifications (colorblind reading)?',
    'Further Supreme Court jurisprudence or constitutional amendment explicitly adopting one interpretation over the others.',
    'If the colorblind reading prevails, this constraint would be reclassified as a snare, as its coordination function would be deemed illegitimate and its extraction purely coercive. If the antisubordination reading prevails, this constraint might be seen as too limited in its scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_vs_colorblind_framing, conceptual, 'Ambiguity in the core purpose of the Equal Protection Clause.').

omega_variable(
    diversity_compelling_interest_empirical_basis,
    'Is the ''compelling interest'' in diversity, as articulated by this reading, empirically supported as a genuine educational benefit, or is it a legal fiction to justify race-conscious policies?',
    'Longitudinal studies on educational outcomes and societal benefits of diverse student bodies, or judicial re-evaluation of the evidentiary standard for ''compelling interest''.',
    'If the empirical basis for diversity as a compelling interest is found weak, the justification for race-conscious policies would erode, potentially leading to a reclassification towards a snare due to diminished coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_compelling_interest_empirical_basis, empirical, 'Empirical validity of the ''compelling interest'' in diversity.').

omega_variable(
    narrow_tailoring_effectiveness,
    'Are the ''narrowly tailored'' requirements for race-conscious policies genuinely effective at minimizing harm to non-beneficiaries, or are they largely performative legal hurdles?',
    'Detailed analysis of admissions processes and their outcomes in various institutions, assessing whether less restrictive means are truly considered and implemented.',
    'If ''narrow tailoring'' is found to be largely performative, the constraint''s legitimacy would be undermined, increasing its effective extractiveness and potentially shifting its classification towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_effectiveness, empirical, 'Effectiveness of ''narrow tailoring'' in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__remedial_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__remedial_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__remedial_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.3).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__remedial_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__remedial_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__remedial_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__remedial_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Equal Protection Clause kernel, alongside the colorblind and antisubordination readings. Each reading represents a distinct structural constraint with different beneficiaries, victims, and operational logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
