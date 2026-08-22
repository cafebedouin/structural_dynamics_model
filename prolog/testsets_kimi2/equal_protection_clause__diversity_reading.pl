% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__diversity_reading, []).

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
 *   constraint_id: equal_protection_clause__diversity_reading
 *   human_readable: Equal Protection Clause â Diversity Reading
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the diversity reading of the Equal
 *   Protection Clause kernel: the interpretive claim that the Constitution
 *   permits race-conscious governmental policies when they serve a compelling
 *   interest in educational diversity benefiting all students, including
 *   white students. It stands in contest with the colorblind reading
 *   (forbidding all racial classifications) and the remedial reading
 *   (permitting race-consciousness only to remedy historical discrimination).
 *   The reading treats minority students as instrumental means to produce
 *   diversity benefits for the broader student body, generating asymmetric
 *   extraction within a genuine coordination function.
 *
 * KEY AGENTS:
 *   - higher_education_institutions: Agenda-setter (institutional/constrained) â defends and implements the diversity rationale under judicial oversight.
 *   - student_body: Primary beneficiary (organized/constrained) â receives asserted educational benefits of diversity.
 *   - minority_students: Instrumentalized payer (moderate/constrained) â admitted as demographic tokens to produce diversity, bearing instrumentalization costs.
 *   - overrepresented_group_applicants: Payer (moderate/constrained) â denied individualized equal consideration when race is used as a negative factor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, 0.58).
domain_priors:suppression_score(equal_protection_clause__diversity_reading, 0.7).
domain_priors:theater_ratio(equal_protection_clause__diversity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Clause â Diversity Reading").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'a2499ed8-3057-4466-9e90-5bfb1c5308ae').
narrative_ontology:cs_kernel_codification('a2499ed8-3057-4466-9e90-5bfb1c5308ae', fixed_text).
narrative_ontology:cs_authority_grounding('a2499ed8-3057-4466-9e90-5bfb1c5308ae', lineage).
narrative_ontology:cs_interpretation_layer_present('a2499ed8-3057-4466-9e90-5bfb1c5308ae').
narrative_ontology:cs_reading_relation('a2499ed8-3057-4466-9e90-5bfb1c5308ae', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('a2499ed8-3057-4466-9e90-5bfb1c5308ae', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('a2499ed8-3057-4466-9e90-5bfb1c5308ae', foundational, educational_diversity_is_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('a2499ed8-3057-4466-9e90-5bfb1c5308ae', educational_diversity_is_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('a2499ed8-3057-4466-9e90-5bfb1c5308ae', foundational, all_students_benefit_from_diversity).
narrative_ontology:cs_axiom_status(all_students_benefit_from_diversity, holdable).
narrative_ontology:cs_axiom_grounding('a2499ed8-3057-4466-9e90-5bfb1c5308ae', all_students_benefit_from_diversity, empirically_contingent).
narrative_ontology:cs_reference_frame('a2499ed8-3057-4466-9e90-5bfb1c5308ae', diversity_as_compelling_state_interest).
narrative_ontology:cs_drift_state('a2499ed8-3057-4466-9e90-5bfb1c5308ae', post_sffa_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('a2499ed8-3057-4466-9e90-5bfb1c5308ae', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, student_body).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, higher_education_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_students).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, overrepresented_group_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, administer, and litigate in defense of race-conscious admissions policies under the diversity rationale. They bear litigation costs, reputational risk, and compliance burdens while gaining legal latitude to pursue compositional diversity goals.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, higher_education_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Receives the asserted educational benefits of a racially diverse learning environment, including cross-racial understanding, broader perspectives, and preparation for a pluralistic society.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, student_body, beneficiary,
    organized, biographical, constrained, national).

% Admitted or placed partly because their racial identity serves the institution's diversity goals; their individual merit and narrative are subordinated to group demographic function, incurring instrumentalization and tokenization costs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_students, payer,
    moderate, biographical, constrained, national).

% Face reduced probability of admission or specific educational placements when race-conscious criteria disadvantage their group; their individualized academic records are weighed against institutional diversity targets.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, overrepresented_group_applicants, payer,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of homogeneous self-selection in educational environments by permitting institutions to use race as a plus-factor to construct a diverse student body, ostensibly producing educational benefits that individual applicants would not secure through uncoordinated choice.
% TRANSFER_FUNCTION: Moves admissions preference and institutional legitimacy from applicants judged solely on individual academic metrics to a framework where racial identity is used as a compositional tool; the costs fall on overrepresented-group applicants denied individualized equal consideration and on minority students whose group membership is instrumentalized to produce the diversity.
% ABSENT_VOICES: Applicants from overrepresented groups who are silently denied admission despite stronger academic metrics, and minority students who would prefer not to be treated as diversity instruments, are structurally muted because the policy frames objection as rejection of diversity itself rather than of instrumentalization.
% DISAPPEARANCE_RATIONALE: If the diversity reading vanished overnight, universities would lose the primary legal justification for explicit racial preferences in admissions; they would retreat to race-neutral proxies or remedial frameworks, and the composition of selective student bodies would shift substantially.
% FOUNDING_PROBLEM: Homogeneous educational environments that fail to prepare students for a pluralistic society and that reproduce racial isolation, lacking a mechanism to secure the educational benefits of diversity.
% FOUNDING_PROBLEM_CORROBORATION: University administrators and educational researchers within the benefiting institutions attest the problem is live. Litigants and legal foundations outside the beneficiary set (e.g., SFFA, empirical critics) contest both the severity of the problem and the necessity of race-conscious means, arguing that race-neutral alternatives suffice.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__diversity_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__diversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__diversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because narrow tailoring and individualized review requirements limit but do not eliminate the use of race as a decisive factor. Suppression is substantial (0.70) because the constraint's persistence requires active judicial defense and the exclusion of colorblind alternatives from legal legitimacy in participating institutions. Theater ratio (0.45) reflects growing performative justification as empirical challenges mount. Resistance is high (0.75) due to sustained litigation and political contestation. The measurement series share one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The student_body seat computes the constraint as coordination (receiving diversity benefits), while the overrepresented_group_applicants and minority_students seats compute it as extraction (denied equal consideration or instrumentalized). The higher_education_institutions seat experiences both coordination latitude and enforcement burden. The engine captures this divergence from the structural role declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (student_body, institutions) sit near the low-d end: they receive the coordination goods. Victims (minority_students as instrumental means, overrepresented_group_applicants as displaced competitors) sit near the high-d end: they bear the costs of the arrangement. The engine will amplify effective extraction for the payer seats and damp it for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a snare because the coordination functionâeducational diversityâis structurally genuine and the beneficiary set is broad (all students), not a narrow rentier class. It is not a scaffold because the reading's own logic treats diversity as a permanent, ongoing educational necessity with no sunset. It is not a mountain because it does not emerge naturally; it depends entirely on judicial interpretation and active institutional defense. The moderate Îµ and active enforcement profile place it in the tangled_rope category: coordination and extraction are braided through the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_diversity_benefit,
    'Do race-conscious admissions actually produce measurable educational benefits for all students, or is the effect illusory or concentrated?',
    'Systematic meta-analysis of classroom-level diversity effects with controls for selection bias and institutional heterogeneity.',
    'If benefits are negligible, the coordination story collapses and the constraint reclassifies toward snare; if robust, the tangled_rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_diversity_benefit, empirical, 'Whether educational diversity benefits are empirically real.').

omega_variable(
    instrumentalization_as_extraction,
    'Does the instrumental use of minority students'' racial identity constitute extractive cost sufficient to qualify as victimization, or is it a legitimate means to a collective educational end?',
    'Qualitative and quantitative analysis of minority-student self-reported experience, academic outcomes, and identity-development metrics under race-conscious policies.',
    'If instrumentalization imposes significant non-material costs, victim status is reinforced and directionality for minority_students remains high; if costless, they migrate toward beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_as_extraction, conceptual, 'Whether instrumentalization of minority students is extractive.').

omega_variable(
    colorblind_alternative_viability,
    'Can race-neutral alternatives (percentage plans, socioeconomic proxies) produce comparable diversity without racial classification?',
    'Comparative institutional studies of race-neutral plans in states where race-conscious admissions are barred.',
    'If viable, the constraint''s accessibility_collapse is overstated and the coordination justification weakens; if non-viable, the suppression of alternatives is more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_alternative_viability, empirical, 'Whether race-neutral alternatives achieve similar diversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_div_tr_t0, equal_protection_clause__diversity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(epc_div_tr_t8, equal_protection_clause__diversity_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(epc_div_tr_t16, equal_protection_clause__diversity_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(epc_div_tr_t24, equal_protection_clause__diversity_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(epc_div_tr_t32, equal_protection_clause__diversity_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(epc_div_tr_t40, equal_protection_clause__diversity_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(epc_div_tr_t45, equal_protection_clause__diversity_reading, theater_ratio, 45, 0.45).

% Extraction over time
narrative_ontology:measurement(epc_div_be_t0, equal_protection_clause__diversity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(epc_div_be_t8, equal_protection_clause__diversity_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(epc_div_be_t16, equal_protection_clause__diversity_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(epc_div_be_t24, equal_protection_clause__diversity_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(epc_div_be_t32, equal_protection_clause__diversity_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(epc_div_be_t40, equal_protection_clause__diversity_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(epc_div_be_t45, equal_protection_clause__diversity_reading, base_extractiveness, 45, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(epc_div_su_t0, equal_protection_clause__diversity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(epc_div_su_t8, equal_protection_clause__diversity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(epc_div_su_t16, equal_protection_clause__diversity_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(epc_div_su_t24, equal_protection_clause__diversity_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(epc_div_su_t32, equal_protection_clause__diversity_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(epc_div_su_t40, equal_protection_clause__diversity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(epc_div_su_t45, equal_protection_clause__diversity_reading, suppression_requirement, 45, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equal_protection_clause kernel. The kernel decomposes into three structurally distinct readings (colorblind, diversity, remedial) because each assigns a different beneficiary/victim structure and a different Îµ to the same constitutional text. They are linked as a constraint family via cs_structure.reading_relations and network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
