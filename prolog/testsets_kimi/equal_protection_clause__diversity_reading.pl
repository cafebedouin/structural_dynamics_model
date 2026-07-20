% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [HISTORICAL]
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Equal Protection Diversity Reading: Race-Conscious Admissions for Educational Diversity
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   This constraint instantiates the diversity reading of the Equal
 *   Protection Clause: the constitutional claim that race-conscious
 *   admissions policies are permissible when they serve a compelling interest
 *   in educational diversity benefiting all students. It is one of three
 *   contested readings of the equal_protection_clause kernel, distinct from
 *   the colorblind reading (which forbids all racial classifications) and the
 *   remedial reading (which requires remediation of historical
 *   subordination). The structural delta of this reading is moderate
 *   extractiveness: it genuinely coordinates a diverse educational
 *   environment but instrumentally extracts from minority applicants and
 *   disadvantages non-preferred applicants in a zero-sum admissions
 *   competition. Active judicial enforcement (strict scrutiny, narrow
 *   tailoring) is required to maintain the boundary between permitted 'plus
 *   factor' consideration and forbidden quotas. The constraint is claimed as
 *   Tangled Rope to capture this hybrid character.
 *
 * KEY AGENTS:
 *   - judiciary (institutional/analytical): Sets and enforces the constitutional standard through strict scrutiny jurisprudence
 *   - selective_universities (institutional/constrained): Administer race-conscious admissions within narrow tailoring requirements
 *   - all_students (organized/constrained): Receive purported educational diversity benefits
 *   - minority_students (moderate/identity_locked): Instrumentalized as diversity producers while gaining individual access
 *   - disadvantaged_applicants (moderate/constrained): Bear the statistical cost of reduced admission probability
 *   - colorblind_advocates (organized/constrained): Excluded from policy design by the diversity framework's constitutional legitimacy
 *   - remedial_justice_advocates (organized/constrained): Excluded because the diversity rationale displaces remedial-justice framing
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
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(equal_protection_clause__diversity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__diversity_reading, "Equal Protection Diversity Reading: Race-Conscious Admissions for Educational Diversity").
narrative_ontology:topic_domain(equal_protection_clause__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__diversity_reading, 'c1950358-6aba-410d-8952-f1223e55357f').
narrative_ontology:cs_kernel_codification('c1950358-6aba-410d-8952-f1223e55357f', fixed_text).
narrative_ontology:cs_authority_grounding('c1950358-6aba-410d-8952-f1223e55357f', lineage).
narrative_ontology:cs_interpretation_layer_present('c1950358-6aba-410d-8952-f1223e55357f').
narrative_ontology:cs_reading_relation('c1950358-6aba-410d-8952-f1223e55357f', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('c1950358-6aba-410d-8952-f1223e55357f', equal_protection_clause__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('c1950358-6aba-410d-8952-f1223e55357f', foundational, educational_diversity_compelling_interest).
narrative_ontology:cs_axiom_status(educational_diversity_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('c1950358-6aba-410d-8952-f1223e55357f', educational_diversity_compelling_interest, instrumental).
narrative_ontology:cs_axiom('c1950358-6aba-410d-8952-f1223e55357f', foundational, race_as_plus_factor_not_stereotype).
narrative_ontology:cs_axiom_status(race_as_plus_factor_not_stereotype, holdable).
narrative_ontology:cs_axiom_grounding('c1950358-6aba-410d-8952-f1223e55357f', race_as_plus_factor_not_stereotype, deontological).
narrative_ontology:cs_reference_frame('c1950358-6aba-410d-8952-f1223e55357f', compelling_interest_diversity_framework).
narrative_ontology:cs_drift_state('c1950358-6aba-410d-8952-f1223e55357f', post_sffa_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('c1950358-6aba-410d-8952-f1223e55357f', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__diversity_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, selective_universities).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, disadvantaged_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__diversity_reading, minority_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__diversity_reading, minority_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and refines the constitutional standard through case-by-case adjudication, balancing diversity benefits against individualized review requirements and enforcing narrow tailoring through strict scrutiny.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Design and defend holistic admissions policies that use race as one factor among many, justifying the practice through diversity-benefits research and facing litigation when they deviate from narrow tailoring.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, selective_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, selective_universities, beneficiary).

% Receive the purported educational benefits of cross-racial interaction and diverse classroom environments in selective institutions; they do not choose the admissions framework but are its declared beneficiaries.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, all_students, beneficiary,
    organized, biographical, constrained, national).

% Gain admission to selective institutions through race-conscious policies but find their individual academic identities subordinated to the demographic diversity they represent; their presence is cited as producing educational benefits for others.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, minority_students, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__diversity_reading, minority_students, beneficiary).

% Face statistically reduced admission probabilities at selective institutions operating under diversity-reading regimes; their academic credentials are weighed against institutional diversity goals that discount purely meritocratic ranking.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, disadvantaged_applicants, payer,
    moderate, biographical, constrained, national).

% Advance constitutional and policy arguments that all governmental racial classifications violate equal protection; they are structurally excluded from admissions policy design because the diversity reading legitimizes the race-conscious frameworks they oppose.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% Advance the remedial reading of Equal Protection, arguing race-conscious policies should correct historical subordination rather than serve instrumental diversity; their framework is displaced when institutions adopt the diversity rationale to avoid remedial-justice scrutiny.
narrative_ontology:constraint_stakeholder(equal_protection_clause__diversity_reading, remedial_justice_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__diversity_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional framework that permits universities to consider race in admissions to achieve the educational benefits of diversity, including cross-racial understanding, breakdown of stereotypes, and preparation for civic engagement in a pluralistic society.
% TRANSFER_FUNCTION: Transfers competitive admissions advantage among racial groups and transfers the burden of producing educational diversity onto minority applicants whose presence is instrumentalized for the benefit of all students, while universities receive legal cover and institutional legitimacy.
% ABSENT_VOICES: Applicants from non-preferred groups who bear the statistical burden of reduced admission rates; minority students whose individual academic narratives are collapsed into demographic tokens; colorblind constitutionalists who reject all racial classifications; and remedial-justice advocates who argue for historical redress rather than instrumental diversity are all structurally marginalized in the diversity-benefits discourse.
% DISAPPEARANCE_RATIONALE: If the diversity-reading constraint vanished overnight, universities would lose their primary constitutional justification for race-conscious admissions, forcing immediate restructuring of admissions criteria toward purely colorblind or class-based alternatives, and the composition and pedagogical model of selective institutions would shift significantly.
% FOUNDING_PROBLEM: Racially homogeneous elite educational environments were thought to produce narrow perspectives, weak cross-cultural competence, and illegitimate social stratification; the Equal Protection Clause was read to permit race-conscious remedies that served the educational mission itself rather than compensating for past discrimination.
% FOUNDING_PROBLEM_CORROBORATION: University administrators and some justices attest the problem is live, citing educational research on diversity benefits. Critics, including dissenting justices and social scientists, contest that the benefits are substantial or unique to racial diversity; no neutral empirical consensus exists outside the beneficiary institutions, and the remedial-justice tradition explicitly disputes the framing.
narrative_ontology:disappearance_verdict(equal_protection_clause__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__diversity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.58) because the constraint genuinely coordinates educational diversity (a real collective good in the reading's own terms) while asymmetrically extracting from minority students through instrumentalization and from non-preferred applicants through competitive displacement. Suppression is substantial (0.70) because the constraint's persistence depends on active judicial enforcement of narrow tailoring and on suppressing colorblind alternatives. Theater ratio rises to 0.45 by interval end as narrow tailoring became increasingly performative and legally formalistic. Accessibility_collapse is moderate (0.48) because colorblind and class-based alternatives remain structurally available but are legally suppressed. Resistance is high (0.72) due to persistent legal challenges (Bakke dissent, Grutter dissent, Fisher litigation, SFFA) and political opposition. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and universities experience this constraint as a necessary coordination mechanism for educational mission and constitutional order (low d, beneficiary-side). Disadvantaged applicants and colorblind advocates experience it as active extraction that subordinates individual merit to group demographics (high d, target-side). Minority students occupy a split seat: they gain access but are instrumentalized, producing a directionality near 0.5 that masks the extraction embedded in their identity-locked position. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (all_students, selective_universities) derive low directionality because the constraint subsidizes their educational environment and legal framework. Victims (disadvantaged_applicants, minority_students) derive high directionality because the constraint extracts admission probability from the former and identity-autonomy from the latter. The judiciary sits near symmetric (0.5) because it both creates and is bound by the doctrine. Minority students' identity_locked exit amplifies their effective extraction despite their individual admission gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both genuine coordination (beneficiaries exist: all students, universities) and asymmetric extraction (victims exist: disadvantaged applicants, instrumentalized minority students). A pure Snare reading would ignore the documented educational coordination and treat the diversity rationale as pure cover; a pure Rope reading would ignore the zero-sum admissions extraction and instrumentalization. Tangled Rope captures the hybrid: the coordination is real but inseparable from the extraction. The mandate is contested â the founding problem (homogeneous elite education) is not resolved, but the constraint was ultimately terminated by external judicial action (SFFA v. Harvard) rather than internal mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_reading_kernel_location,
    'Is the Equal Protection Clause''s permission of race-conscious policies genuinely rooted in a diversity interest discoverable in the constitutional text, or is this a constructed judicial reading that instrumentalizes minority students for institutional legitimacy?',
    'Comparative analysis across sibling readings: if the same constitutional text sustains the colorblind reading (forbidding all racial classifications) without logical contradiction, the diversity reading is a constructed interpretation rather than a discovered natural-law feature of the clause.',
    'If constructed, the constraint is a Tangled Rope or Snare rather than a Mountain; classification shifts from natural-law discovery to active enforcement-dependent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_reading_kernel_location, conceptual, 'Whether the diversity reading is a natural feature of EP or a constructed judicial reading').

omega_variable(
    instrumentalization_vs_benefit,
    'Do minority students in diversity-reading regimes experience net benefit or net extraction when their admissions utility is framed as serving the educational interests of white and Asian students?',
    'Longitudinal outcome studies comparing academic performance, belonging metrics, and stereotype threat for minority students admitted under diversity rationales versus class-based or colorblind regimes.',
    'If net extraction (instrumentalization without corresponding benefit), the victim set expands and the constraint''s coordination story weakens; if net benefit, the Tangled Rope classification tightens toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_vs_benefit, empirical, 'Whether minority students are net beneficiaries or instrumentalized victims').

omega_variable(
    permanence_vs_temporary_scaffold,
    'Does the ongoing educational value of diversity justify a permanent race-conscious regime, or is the diversity reading actually a scaffold that should have sunset once a critical mass of diversity was achieved?',
    'Tracking whether universities ever declare their diversity objectives satisfied and voluntarily move to race-neutral alternatives; Grutter''s expectation of a 25-year horizon provides a natural experiment.',
    'If permanent, the constraint is Tangled Rope; if transitional, it reclassifies as Scaffold and the absence of a sunset clause becomes a metric/claim mismatch.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_vs_temporary_scaffold, conceptual, 'Whether the diversity rationale is genuinely permanent or a scaffold without sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__diversity_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_diversity_tr_t0, equal_protection_clause__diversity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(epc_diversity_tr_t10, equal_protection_clause__diversity_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(epc_diversity_tr_t20, equal_protection_clause__diversity_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(epc_diversity_tr_t25, equal_protection_clause__diversity_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(epc_diversity_tr_t35, equal_protection_clause__diversity_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(epc_diversity_tr_t45, equal_protection_clause__diversity_reading, theater_ratio, 45, 0.45).

% Extraction over time
narrative_ontology:measurement(epc_diversity_be_t0, equal_protection_clause__diversity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(epc_diversity_be_t10, equal_protection_clause__diversity_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(epc_diversity_be_t20, equal_protection_clause__diversity_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(epc_diversity_be_t25, equal_protection_clause__diversity_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(epc_diversity_be_t35, equal_protection_clause__diversity_reading, base_extractiveness, 35, 0.55).
narrative_ontology:measurement(epc_diversity_be_t45, equal_protection_clause__diversity_reading, base_extractiveness, 45, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(epc_diversity_su_t0, equal_protection_clause__diversity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(epc_diversity_su_t10, equal_protection_clause__diversity_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(epc_diversity_su_t20, equal_protection_clause__diversity_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(epc_diversity_su_t25, equal_protection_clause__diversity_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(epc_diversity_su_t35, equal_protection_clause__diversity_reading, suppression_requirement, 35, 0.64).
narrative_ontology:measurement(epc_diversity_su_t45, equal_protection_clause__diversity_reading, suppression_requirement, 45, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__diversity_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equal_protection_clause kernel. The colorblind reading and remedial reading are structurally distinct siblings with different epsilon values, beneficiary structures, and victim sets. The diversity reading's moderate extractiveness distinguishes it from the colorblind reading (low extraction, high coordination of individual rights) and the remedial reading (potentially higher extraction through permanent group-based redistribution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
