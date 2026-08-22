% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Remedial Race-Conscious Remediation
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause holds that the
 *   Constitution not only permits but requires race-conscious government
 *   action to remedy the effects of historical racial subordination. This
 *   reading animates affirmative action in higher education, disparate impact
 *   enforcement, minority set-asides in public contracting, and court-ordered
 *   desegregation remedies. It is structurally a scaffold: the remedial
 *   mandate carries a sunset logic (remediation ends when the effects of
 *   subordination are eliminated), but the sunset condition has proven
 *   contested and repeatedly extended. The constraint extracts from
 *   individuals in non-preferred groups (reduced access to competitive
 *   positions) to benefit historically marginalized racial groups. The
 *   coordination function is dismantling self-reinforcing exclusion; the
 *   transfer function moves concrete opportunities across group lines. The
 *   kernel is the Equal Protection Clause; sibling readings are the
 *   colorblind reading (all racial classifications forbidden) and the
 *   diversity reading (race-consciousness permitted for educational diversity
 *   benefits to all students).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.62).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.58).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Remedial Race-Conscious Remediation").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, 'fe13b683-8f4b-416b-992a-6c142a054624').
narrative_ontology:cs_kernel_codification('fe13b683-8f4b-416b-992a-6c142a054624', fixed_text).
narrative_ontology:cs_authority_grounding('fe13b683-8f4b-416b-992a-6c142a054624', extraction).
narrative_ontology:cs_interpretation_layer_present('fe13b683-8f4b-416b-992a-6c142a054624').
narrative_ontology:cs_reading_relation('fe13b683-8f4b-416b-992a-6c142a054624', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('fe13b683-8f4b-416b-992a-6c142a054624', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('fe13b683-8f4b-416b-992a-6c142a054624', foundational, race_conscious_remediation_constitutionally_required).
narrative_ontology:cs_axiom_status(race_conscious_remediation_constitutionally_required, holdable).
narrative_ontology:cs_axiom_grounding('fe13b683-8f4b-416b-992a-6c142a054624', race_conscious_remediation_constitutionally_required, deontological).
narrative_ontology:cs_axiom('fe13b683-8f4b-416b-992a-6c142a054624', foundational, substantive_equality_requires_group_remediation).
narrative_ontology:cs_axiom_status(substantive_equality_requires_group_remediation, holdable).
narrative_ontology:cs_axiom_grounding('fe13b683-8f4b-416b-992a-6c142a054624', substantive_equality_requires_group_remediation, deontological).
narrative_ontology:cs_axiom('fe13b683-8f4b-416b-992a-6c142a054624', secondary, remediation_sunset_when_effects_eliminated).
narrative_ontology:cs_axiom_status(remediation_sunset_when_effects_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('fe13b683-8f4b-416b-992a-6c142a054624', remediation_sunset_when_effects_eliminated, instrumental).
narrative_ontology:cs_reference_frame('fe13b683-8f4b-416b-992a-6c142a054624', anti_subordination_constitutionalism).
narrative_ontology:cs_drift_state('fe13b683-8f4b-416b-992a-6c142a054624', post_students_for_fair_admissions, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fe13b683-8f4b-416b-992a-6c142a054624', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_racial_minorities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, civil_rights_enforcement_agencies).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individuals_in_non_preferred_groups_education).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individuals_in_non_preferred_groups_employment).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individuals_in_non_preferred_groups_contracting).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, anti_subordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that experienced legal subordination (slavery, Jim Crow, exclusion from housing, education, employment). Race-conscious remediation provides access to institutions, contracts, and opportunities from which they were systematically excluded. Exit is constrained because the structural deficits the remedy addresses persist regardless of individual action; the remedy is a structural intervention, not an individual opt-in.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_marginalized_racial_minorities, beneficiary,
    organized, generational, constrained, national).

% DOJ Civil Rights Division, EEOC, OCR, and state counterparts that design, monitor, and enforce remedial orders (consent decrees, affirmative action plans, desegregation orders). They hold statutory authority to impose race-conscious remedies and their institutional mission and budgets are tied to active enforcement. They can shift enforcement priorities across administrations but the statutory mandate persists.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, civil_rights_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, civil_rights_enforcement_agencies, beneficiary).

% Applicants to competitive educational programs (college, graduate, magnet K-12) who are not members of preferred groups under remedial plans. They bear the cost of racial preferences in admissions as reduced probability of acceptance. Their exit options are constrained: they can apply elsewhere, forego the credential, or litigate — but the constraint applies across the institutional tier they aim for.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individuals_in_non_preferred_groups_education, payer,
    moderate, biographical, constrained, national).

% Workers and applicants in public employment and federal contracting subject to race-conscious hiring, promotion, or set-aside goals. They bear opportunity costs when race is a factor in decisions. Exit is constrained because the constraint operates across the public-sector and federally funded labor market segment; moving to private sector avoids it but may entail wage or mission tradeoffs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individuals_in_non_preferred_groups_employment, payer,
    moderate, biographical, constrained, national).

% Non-minority-owned firms competing for government contracts with set-asides, subcontracting goals, or race-conscious evaluation criteria. They bear the cost of reserved shares and preference points. Exit is constrained because the federal/state procurement market is large and the constraint is a condition of participation; declining to compete means foregoing a major revenue stream.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individuals_in_non_preferred_groups_contracting, payer,
    moderate, biographical, constrained, national).

% Judges, scholars, and advocates who read the Equal Protection Clause as forbidding all racial classifications. They would object to any race-conscious remedy as a violation of the constitutional text and original understanding. They are excluded from the remedial framework's design because the framework treats their reading as foreclosed; their voice enters only through litigation challenging the remedy.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, constitutional_originalists, excluded,
    organized, generational, identity_locked, national).

% Academic researchers studying the effects of race-conscious remedies on educational outcomes, labor markets, and intergroup attitudes. They analyze whether remediation narrows disparities, whether effects persist after sunset, and what alternative mechanisms exist. They neither collect nor pay; their structural position is external evaluation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, legal_scholars_empirical, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dismantles persistent, group-based exclusion from major social institutions (education, employment, public contracting) by temporarily using race-conscious allocation to offset the continuing effects of past legal subordination.
% TRANSFER_FUNCTION: Moves admissions seats, hiring slots, promotion opportunities, and contract awards from individuals in non-preferred groups to individuals in historically marginalized racial groups, as a structural redress for group subordination.
% ABSENT_VOICES: Future generations of both beneficiary and non-preferred groups who will inherit the post-remediation equilibrium but have no voice in the current design; individuals in non-preferred groups who support remediation in principle but bear its costs in specific competitions; communities that fall outside the recognized preferred/non-preferred binary (e.g., Asian American subgroups with divergent positions on affirmative action).
% DISAPPEARANCE_RATIONALE: If the remedial reading vanished overnight, race-conscious admissions, hiring, and contracting programs would be legally impermissible across the board. Institutions would shift to race-neutral alternatives (socioeconomic preferences, percentage plans, pipeline investments). Disparities in representation would likely widen in the short term; the political coalition sustaining civil rights enforcement would fracture. The world rearranges.
% FOUNDING_PROBLEM: After formal legal equality was established (14th Amendment, Civil Rights Acts), racial disparities in education, employment, wealth, and health persisted because the effects of centuries of legal subordination were self-reinforcing. The remedial reading was built to solve the problem that colorblind law alone could not undo the structural deficits created by color-conscious oppression.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations and enforcement agencies attest the founding problem remains live — disparities persist and race-neutral tools have not closed them. Opponents (originalist scholars, conservative legal movement) attest the founding problem is dead — formal barriers are gone, remaining disparities reflect non-discriminatory factors. Social science research (Chetty et al. on mobility, Card on college access) corroborates from outside the benefiting parties that group disparities persist but is contested on causation and remedy.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the constraint moves substantial, rivalrous goods (elite admissions seats, public contracts, civil service positions) across group lines by race-conscious rule. Suppression (0.58) is moderate-high: the constraint persists through active judicial and administrative enforcement; alternatives (race-neutral policies) are legally disfavored under this reading. Theater ratio (0.28) is modest: the remedial function is real and measurable (disparity reduction in targeted domains), but a growing share of enforcement activity maintains the framework after initial disparity reduction plateaus. Accessibility collapse (0.42) is moderate: race-neutral alternatives exist and are litigated, but this reading treats them as insufficient. Resistance (0.71) is high: sustained political, legal, and intellectual opposition across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seats, the constraint is genuine coordination solving a collective-action problem (dismantling exclusion). From the payer seats, it is extraction (race-conscious transfer of rival goods). From the excluded seats, it is constitutional violation. The engine computes this divergence from the structural data — the claimed_type (scaffold) reflects the authoring seat's judgment that the remedial logic is structurally transitional, but the metrics show substantial extraction and enforcement persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized racial minorities are structural beneficiaries (d low): the constraint subsidizes their access. Civil rights enforcement agencies are agenda-setters with arbitrage-grade exit (institutional mission alignment, budgetary dependence on active enforcement). Individuals in non-preferred groups across education, employment, and contracting are structural payers (d high): they bear the opportunity cost of race-conscious allocation, with constrained exit because the constraint operates across the institutional tier they seek. Originalists are excluded and identity-locked (their professional and ideological identity is constituted through the colorblind reading; exit means abandoning their interpretive framework). Empirical legal scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (persistent effects of legal subordination) is contested: one side says it persists, the other says formal equality has done its work. The scaffold classification captures the tension: the mandate claims sunset logic, but the sunset condition is not met on either side's terms. If the founding problem is dead, the constraint is a piton (persisting by inertia/theater). If live, it remains a scaffold. The mandated sunset clause (Grutter's 25-year expectation, consent decree termination standards) is the structural signature of a scaffold, but the repeated extension of remedial authority suggests mandatrophy risk. The classification prevents mislabeling: it is not a snare (the coordination function is real, not cover) nor a tangled rope (the extraction is not the point; the sunset logic distinguishes it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_criterion,
    'What empirical or institutional condition would satisfy the sunset clause — when is remediation ''complete''?',
    'Judicial articulation of termination standards for consent decrees; legislative sunset triggers; social scientific consensus on disparity elimination.',
    'If no coherent completion criterion exists, the scaffold''s sunset logic is performative and the constraint drifts toward piton. If a criterion exists but is perpetually deferred, the constraint is a scaffold with a receding horizon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remediation_completion_criterion, conceptual, 'Whether the scaffold''s sunset condition is structurally realizable or indefinitely deferred.').

omega_variable(
    colorblind_foreclosure_structure,
    'Does the colorblind reading''s core premise (all racial classifications forbidden) logically foreclose the remedial reading in any single constitutional framework, or do they coexist as competing interpretations?',
    'Doctrinal analysis of whether a court adopting the colorblind reading as binding precedent would leave any doctrinal space for race-conscious remediation; historical analysis of whether any jurisdiction has held both simultaneously.',
    'If forecloses, the readings are mutually exclusive at the framework level (cs_structure relation = forecloses). If coexists_with, they are competing live positions in a pluralist legal system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_foreclosure_structure, conceptual, 'Structural relationship between remedial and colorblind readings at the framework level.').

omega_variable(
    diversity_remediation_boundary,
    'Are the diversity reading''s ''educational diversity benefits'' and the remedial reading''s ''substantive equality for marginalized groups'' structurally distinct coordination functions, or does the diversity reading subsume the remedial reading?',
    'Case law analysis: do diversity-justified programs produce the same distributive effects as remedial programs? Empirical analysis of whether diversity benefits accrue to the same groups remedial programs target.',
    'If distinct, the readings have different beneficiary/victim structures and different ε. If the diversity reading subsumes the remedial, the remedial reading is a subset of the diversity reading''s coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_remediation_boundary, empirical, 'Whether diversity and remedial justifications are structurally separable constraint types.').

omega_variable(
    kernel_commitment_stability,
    'Is the Equal Protection Clause kernel a fixed_text (Fourteenth Amendment text) with interpretation_layer, or has the kernel itself become distributed/ambiguous across readings?',
    'Analyze whether the authority structure (Supreme Court) treats the text as a stable kernel with competing readings, or whether the text''s meaning has become constituted by the readings themselves.',
    'If fixed_text + interpretation_layer, cs_structure.kernel_codification = fixed_text and authority_grounding = lineage/extraction. If distributed, kernel_codification = distributed and authority_grounding = distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_commitment_stability, conceptual, 'Codification status of the kernel and whether an authoritative interpretive layer exists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1978, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_remedial_tr_t1978, equal_protection_clause__remedial_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(epc_remedial_tr_t1988, equal_protection_clause__remedial_reading, theater_ratio, 1988, 0.18).
narrative_ontology:measurement(epc_remedial_tr_t1998, equal_protection_clause__remedial_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(epc_remedial_tr_t2008, equal_protection_clause__remedial_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement(epc_remedial_tr_t2018, equal_protection_clause__remedial_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(epc_remedial_tr_t2028, equal_protection_clause__remedial_reading, theater_ratio, 2028, 0.28).

% Extraction over time
narrative_ontology:measurement(epc_remedial_be_t1978, equal_protection_clause__remedial_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(epc_remedial_be_t1988, equal_protection_clause__remedial_reading, base_extractiveness, 1988, 0.52).
narrative_ontology:measurement(epc_remedial_be_t1998, equal_protection_clause__remedial_reading, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(epc_remedial_be_t2008, equal_protection_clause__remedial_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(epc_remedial_be_t2018, equal_protection_clause__remedial_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(epc_remedial_be_t2028, equal_protection_clause__remedial_reading, base_extractiveness, 2028, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(epc_remedial_su_t1978, equal_protection_clause__remedial_reading, suppression_requirement, 1978, 0.48).
narrative_ontology:measurement(epc_remedial_su_t1988, equal_protection_clause__remedial_reading, suppression_requirement, 1988, 0.52).
narrative_ontology:measurement(epc_remedial_su_t1998, equal_protection_clause__remedial_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(epc_remedial_su_t2008, equal_protection_clause__remedial_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(epc_remedial_su_t2018, equal_protection_clause__remedial_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(epc_remedial_su_t2028, equal_protection_clause__remedial_reading, suppression_requirement, 2028, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the equal_protection_clause kernel. The remedial reading claims race-conscious remediation is constitutionally required; the colorblind reading claims it is constitutionally forbidden; the diversity reading claims it is constitutionally permitted for educational diversity. Each reading has a different beneficiary/victim structure and different ε. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__remedial_reading, organized, 0.15).
constraint_indexing:directionality_override(equal_protection_clause__remedial_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
