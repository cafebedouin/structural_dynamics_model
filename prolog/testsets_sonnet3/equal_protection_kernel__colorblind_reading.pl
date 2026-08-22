% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Equal Protection Clause — Colorblind (Anti-Classification) Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the colorblind (anti-classification) reading of
 *   the Equal Protection Clause's kernel: state use of racial classification
 *   is categorically forbidden regardless of remedial purpose. Under this
 *   reading, admissions programs weighing race as a factor — even to remedy
 *   documented historical exclusion or pursue diversity — are per se
 *   unconstitutional. This is one of three structurally distinct constraints
 *   emitted from the same textual kernel; the remedial reading and the
 *   antisubordination reading are separate constraint stories with their own
 *   ε, beneficiaries, and victims, linked here via
 *   network.affects_constraints. Do not read this file as describing the
 *   clause itself — it describes the doctrinal arrangement that obtains WHEN
 *   this specific reading is the controlling one.
 *
 * KEY AGENTS:
 *   - selective_university_admissions_offices: agenda_setter/beneficiary — administers compliance and gains litigation insulation
 *   - white_and_asian_applicant_pools_near_admission_thresholds: beneficiary — marginal admissions shift favorably
 *   - formal_equality_legal_movement: beneficiary — doctrinal victory and institutional standing
 *   - historically_excluded_black_and_latino_applicants: payer — loses remedial admissions pathway
 *   - first_generation_and_low_income_applicants_of_code: payer — residual disadvantage uncaptured by race-neutral proxies
 *   - hbcu_and_minority_serving_institution_pipeline_advocates: excluded — alternate remedial infrastructure with no doctrinal voice
 *   - federal_and_state_courts: observer — adjudicates proxy-classification boundary disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.58).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.62).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Equal Protection Clause — Colorblind (Anti-Classification) Reading").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, 'a74e9365-3735-4437-8e0f-129a24055884').
narrative_ontology:cs_kernel_codification('a74e9365-3735-4437-8e0f-129a24055884', fixed_text).
narrative_ontology:cs_authority_grounding('a74e9365-3735-4437-8e0f-129a24055884', lineage).
narrative_ontology:cs_interpretation_layer_present('a74e9365-3735-4437-8e0f-129a24055884').
narrative_ontology:cs_reading_relation('a74e9365-3735-4437-8e0f-129a24055884', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('a74e9365-3735-4437-8e0f-129a24055884', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('a74e9365-3735-4437-8e0f-129a24055884', foundational, racial_classification_categorically_suspect_regardless_of_purpose).
narrative_ontology:cs_axiom_status(racial_classification_categorically_suspect_regardless_of_purpose, holdable).
narrative_ontology:cs_axiom_grounding('a74e9365-3735-4437-8e0f-129a24055884', racial_classification_categorically_suspect_regardless_of_purpose, deontological).
narrative_ontology:cs_axiom('a74e9365-3735-4437-8e0f-129a24055884', secondary, formal_symmetry_satisfies_equal_protection).
narrative_ontology:cs_axiom_status(formal_symmetry_satisfies_equal_protection, holdable).
narrative_ontology:cs_axiom_grounding('a74e9365-3735-4437-8e0f-129a24055884', formal_symmetry_satisfies_equal_protection, conventional).
narrative_ontology:cs_reference_frame('a74e9365-3735-4437-8e0f-129a24055884', reconstruction_era_anti_caste_prohibition).
narrative_ontology:cs_drift_state('a74e9365-3735-4437-8e0f-129a24055884', post_1978_bakke_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a74e9365-3735-4437-8e0f-129a24055884', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, selective_university_admissions_offices).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, white_and_asian_applicant_pools_near_admission_thresholds).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, formal_equality_legal_movement).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_black_and_latino_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, first_generation_and_low_income_applicants_of_color).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, hbcu_and_minority_serving_institution_pipeline_advocates).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, constitutional_colorblindness_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must redesign admissions criteria to strip race from consideration entirely, administer compliance audits, and defend against litigation testing whether facially neutral proxies (essays, geography, socioeconomic status) function as race-conscious substitutes. Gains legal cover and insulation from disparate-impact-based lawsuits by adopting the bright-line rule.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, selective_university_admissions_offices, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, selective_university_admissions_offices, beneficiary).

% Applicants at the margin who previously competed against race-conscious set-asides now compete under formally identical criteria; this reading removes race as a factor that could have counted against them, shifting seats their way at the most selective institutions. Their exit options remain broad — many alternative institutions are available regardless of outcome.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, white_and_asian_applicant_pools_near_admission_thresholds, beneficiary,
    organized, biographical, mobile, national).

% Litigation organizations and legal scholars whose theory of the clause is vindicated by this reading becoming controlling doctrine; they gain precedent, funding, and institutional standing each time a court adopts anti-classification reasoning. They face essentially no exit cost — the reading's dominance is their product.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, formal_equality_legal_movement, beneficiary,
    organized, generational, arbitrage, national).

% Applicants whose K-12 and family wealth trajectories were shaped by de jure and de facto segregation lose the one admissions mechanism designed to weigh that history; under formal equality their applications are evaluated as if the disparity in preparation and resources were unrelated to state action, with no colorblind institutional mechanism authorized to correct for it. Exit is effectively foreclosed — the affected pool cannot relocate out of the historical disadvantage the reading declines to recognize.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_black_and_latino_applicants, payer,
    powerless, biographical, trapped, national).

% Bear the compounding effect of both class and race disadvantage but can no longer have race weighed as a factor; colorblind proxies (income, first-gen status) capture only part of the disadvantage, leaving a residual gap unaddressed. Few practical alternative pathways to elite credentialing exist.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, first_generation_and_low_income_applicants_of_color, payer,
    powerless, biographical, trapped, national).

% Represent an alternate remedial infrastructure that is neither consulted nor empowered by this reading; they would argue the clause should permit — not forbid — race-conscious remediation, but their institutional voice is not part of the colorblind doctrinal conversation, which treats the question as settled by formal symmetry rather than by input from those administering existing remedial pathways.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, hbcu_and_minority_serving_institution_pipeline_advocates, excluded,
    moderate, generational, constrained, national).

% Adjudicate whether specific admissions practices comply with the anti-classification rule, developing doctrine on facially neutral proxies, disparate impact, and standing. Their rulings determine how far the colorblind principle extends into adjacent domains (employment, contracting, redistricting).
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federal_and_state_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__colorblind_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable bright-line rule — no racial classification, ever — that courts and institutions can apply without adjudicating contested empirical claims about ongoing discrimination or measuring diversity's educational value case by case.
% TRANSFER_FUNCTION: Moves admissions capacity at selective institutions away from applicants whose disadvantage is tied to historically state-sanctioned racial exclusion and toward applicants evaluated under facially race-neutral criteria; moves legal and reputational risk away from institutions that adopt colorblind criteria and toward those that attempt race-conscious remediation.
% ABSENT_VOICES: Communities whose disadvantage traces to documented state action (redlining, school segregation, discriminatory lending) are not party to the doctrinal debate over whether the clause forbids or permits addressing that specific harm; the doctrine is argued almost entirely in appellate briefs and academic literature, not by the populations whose remedial pathway disappears under this reading.
% DISAPPEARANCE_RATIONALE: If the colorblind reading were displaced by a rival reading overnight, admissions offices could reintroduce race-conscious criteria immediately, litigation posture would flip, applicant pool composition at selective institutions would shift within one to two admissions cycles, and the formal-equality legal movement would lose its primary doctrinal victory.
% FOUNDING_PROBLEM: The Equal Protection Clause was ratified to prevent states from using race to entrench a caste system following the abolition of slavery; the colorblind reading answers the interpretive question of what a state's subsequent USE of race — for any purpose, remedial or otherwise — should mean under that clause.
% FOUNDING_PROBLEM_CORROBORATION: The formal-equality legal movement and beneficiary applicant pools attest the colorblind reading is the clause's original and correct meaning. Civil rights historians, some sitting justices in dissent, and social-science research on persistent racial wealth and education gaps — sources outside the reading's beneficiary set — attest the founding problem (dismantling caste-based subordination) remains unresolved and that formal symmetry without regard to historical position can entrench rather than dismantle the hierarchy the clause was written to end.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) and suppression (0.62) are authored at moderate-to-substantial levels because the coordination function (a bright-line rule reducing case-by-case adjudication cost) is genuine, but it operates by categorically foreclosing a remedial mechanism whose absence falls asymmetrically on historically excluded groups — the classic tangled-rope signature: real coordination value AND asymmetric extraction through the same rule. Suppression is unscaled and reflects the doctrine's increasing enforcement reach into facially-neutral-proxy litigation (essay content review, geographic redistricting challenges) over the interval, not merely the original holding. Theater ratio (0.4) reflects genuine adjudicative activity mixed with an increasing share of litigation testing proxies that function substantively like the forbidden classification — theater in the sense that the rule's formal symmetry is maintained while its practical bite is negotiated at the margins.
 *
 * PERSPECTIVAL GAP:
 *   From the admissions-office and formal-equality-movement seats, this reading is coordination: a stable, judicially administrable rule that avoids race-conscious line-drawing entirely. From the excluded-applicant-pool seats, the identical rule is extraction: a formal symmetry that freezes in place the distributional consequences of past state action while forbidding any state mechanism to address them. The engine computes these as different seat-level classifications from the same structural data — this is the seat divergence the tangled_rope classification is meant to surface, not a contradiction to be resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Selective admissions offices and the formal-equality legal movement sit near the beneficiary end: they gain either doctrinal insulation or doctrinal vindication, with mobile-to-arbitrage exit options (institutions can restructure processes; legal movements are not personally exposed to admissions outcomes). Historically excluded and low-income applicants of color sit near the full-target end: trapped exit (a single applicant cannot relocate out of a multi-generational disadvantage pattern the doctrine declines to address), powerless power atom, and direct bearing of the transfer. HBCU/pipeline advocates are excluded rather than coordinated or extracted from directly — their structural position is omission from the doctrinal conversation, which the six_questions absent_voices field captures; this is deliberately NOT treated as a victim declaration since their harm is representational rather than a direct extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dismantling state-enforced racial caste) is authored as contested rather than resolved: the colorblind reading's proponents hold the problem solved by formal symmetry itself, while corroborating sources outside the beneficiary set (civil rights historians, dissenting justices, persistent-gap research) hold the underlying disparity unaddressed. This divergence is exactly what founding_problem_status=contested is designed to register — it prevents this reading from being mislabeled as either pure coordination (ignoring the asymmetric cost) or pure extraction (ignoring the genuine value of an administrable, non-race-classifying rule).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_reading_as_controlling_doctrine_vs_contested_theory,
    'Is the colorblind reading the settled, correct interpretation of the Equal Protection Clause''s original meaning, or one contested theory among several live judicial and academic positions?',
    'Track the reading''s doctrinal status across Supreme Court composition changes, state constitutional provisions, and comparative treatment in circuit courts; a reading that flips with court composition is functioning as a contested political commitment rather than a settled legal fact.',
    'If genuinely settled and correct, the extraction registered here reflects an unavoidable consequence of the correct constitutional rule. If contested and reading-dependent, the extraction reflects a choice among live doctrinal options — closer to a constructed distributional outcome than a natural-law-like constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_reading_as_controlling_doctrine_vs_contested_theory, conceptual, 'Whether the colorblind reading''s authority is settled interpretation or one contested framework among live alternatives.').

omega_variable(
    proxy_classification_functional_equivalence,
    'When institutions adopt facially race-neutral proxies (geography, essay content, socioeconomic status) that predictably reproduce racial composition patterns, does the colorblind reading''s anti-classification logic apply to the proxy, or only to explicit racial classification?',
    'Track post-2023 circuit court and Supreme Court treatment of proxy-based admissions litigation; a ruling extending anti-classification logic to functionally-equivalent proxies would substantially raise the reading''s effective suppression and accessibility_collapse beyond what is authored here.',
    'If proxies are permitted, the reading''s practical extraction is bounded by explicit-classification-only reach; if proxies are also forbidden when functionally equivalent, suppression and extraction increase substantially and the reading''s reach approaches near-total foreclosure of race-conscious remediation by any means.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_classification_functional_equivalence, empirical, 'Whether the anti-classification rule extends from explicit racial classification to functionally equivalent facially neutral proxies.').

omega_variable(
    framing_underdetermination_committer_vs_effects,
    'Should this constraint be evaluated by the committer''s own framework (formal equality as the coordination good) or by its downstream distributional effects (who actually loses access)?',
    'This omega documents the CS-framing choice made in this story: the obvious framing (a judicially administrable bright-line rule) was chosen as the coordination_function, while the less obvious framing (a legitimacy claim about which historical harms the state is permitted to notice) was routed to founding_problem_status=contested rather than folded into extractiveness. A story that instead treated the legitimacy claim as the coordination function would likely author lower extractiveness and no tangled_rope classification.',
    'Adopting the alternative framing (formal equality as the sole legitimate coordination good, full stop) would likely reclassify this story toward rope; the framing chosen here treats the excluded remedial function as a genuine cost that the bright-line rule imposes, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_underdetermination_committer_vs_effects, conceptual, 'Alternative framing of coordination function (administrability vs. legitimacy-of-noticing-history) that would change the computed classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__colorblind_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__colorblind_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__colorblind_reading, theater_ratio, 2013, 0.32).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_kernel__colorblind_reading, theater_ratio, 2018, 0.36).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.32).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__colorblind_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__colorblind_reading, base_extractiveness, 2013, 0.5).
narrative_ontology:measurement(equa_be_t2018, equal_protection_kernel__colorblind_reading, base_extractiveness, 2018, 0.54).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__colorblind_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.46).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__colorblind_reading, suppression_requirement, 2013, 0.52).
narrative_ontology:measurement(equa_su_t2018, equal_protection_kernel__colorblind_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__colorblind_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories sharing the equal_protection_kernel. The colorblind reading forecloses the remedial reading's core premise (a rule that forbids race-conscious remediation per se cannot coexist with a rule permitting it when narrowly tailored, within one controlling framework) while coexisting with the antisubordination reading as a rival normative framework held by different coalitions. Each reading has its own ε, beneficiary/victim structure, and claimed_type — they are not the same constraint viewed from different angles; they are structurally distinct arrangements that happen to cite the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
