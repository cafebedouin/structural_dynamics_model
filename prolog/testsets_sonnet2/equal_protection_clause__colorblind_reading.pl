% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: equal_protection_clause__colorblind_reading
 *   human_readable: Equal Protection as Categorical Prohibition on Racial Classification (Colorblind Reading)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story instantiates the colorblind reading of the equal protection
 *   kernel: the clause is read as a categorical prohibition on all
 *   governmental racial classification, grounding rights in the individual
 *   rather than the group. Under this reading, race-conscious admissions,
 *   contracting, and remediation programs are not exceptions requiring
 *   careful balancing — they are themselves presumptive constitutional
 *   violations, subject to strict scrutiny that is, in practice, nearly
 *   always fatal outside narrow remedial contexts tied to specific identified
 *   discrimination by the specific actor. The reading's ε is authored very
 *   low: applying a formal rule ('no racial classification') is, by the
 *   reading's own lights, close to mechanical rule-application rather than
 *   extraction, since the rule's proponents understand it as removing
 *   government's hand from racial sorting altogether rather than transferring
 *   value between racial groups. The rising suppression trajectory reflects
 *   the doctrine's increasing enforcement reach — from narrow desegregation
 *   contexts in the 1950s-70s to comprehensive prohibition on race-conscious
 *   admissions and much affirmative action by the 2020s — without any
 *   corresponding rise in ε, because the reading treats broader enforcement
 *   as fuller vindication of the same individual right, not as escalating
 *   extraction.
 *
 * KEY AGENTS:
 *   - white_and_asian_american_applicants: primary beneficiary of the categorical rule (moderate/constrained)
 *   - underrepresented_minority_applicants_in_holistic_review: bears the cost of losing race-conscious consideration (moderate/constrained)
 *   - diversity_program_administrators: institutional payer forced to redesign programs (institutional/constrained)
 *   - civil_rights_litigators and federal_courts: agenda-setters who administer and extend the doctrine
 *   - historically_subordinated_racial_groups: excluded from the doctrine's own unit of analysis (powerless/trapped)
 *   - constitutional_law_scholars: analytical observer of doctrinal coherence and foreclosure effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.14).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.42).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, rope).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection as Categorical Prohibition on Racial Classification (Colorblind Reading)").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'd5cfca21-d80c-4f55-b04d-b16414682645').
narrative_ontology:cs_kernel_codification('d5cfca21-d80c-4f55-b04d-b16414682645', fixed_text).
narrative_ontology:cs_authority_grounding('d5cfca21-d80c-4f55-b04d-b16414682645', lineage).
narrative_ontology:cs_interpretation_layer_present('d5cfca21-d80c-4f55-b04d-b16414682645').
narrative_ontology:cs_reading_relation('d5cfca21-d80c-4f55-b04d-b16414682645', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('d5cfca21-d80c-4f55-b04d-b16414682645', equal_protection_clause__diversity_reading, forecloses).
narrative_ontology:cs_axiom('d5cfca21-d80c-4f55-b04d-b16414682645', foundational, race_is_never_a_permissible_classification_criterion).
narrative_ontology:cs_axiom_status(race_is_never_a_permissible_classification_criterion, holdable).
narrative_ontology:cs_axiom_grounding('d5cfca21-d80c-4f55-b04d-b16414682645', race_is_never_a_permissible_classification_criterion, deontological).
narrative_ontology:cs_axiom('d5cfca21-d80c-4f55-b04d-b16414682645', foundational, individual_is_the_sole_unit_of_constitutional_concern).
narrative_ontology:cs_axiom_status(individual_is_the_sole_unit_of_constitutional_concern, holdable).
narrative_ontology:cs_axiom_grounding('d5cfca21-d80c-4f55-b04d-b16414682645', individual_is_the_sole_unit_of_constitutional_concern, deontological).
narrative_ontology:cs_reference_frame('d5cfca21-d80c-4f55-b04d-b16414682645', individual_rights_bearer_framework).
narrative_ontology:cs_drift_state('d5cfca21-d80c-4f55-b04d-b16414682645', post_2023_admissions_decisions_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d5cfca21-d80c-4f55-b04d-b16414682645', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individuals_denied_race_conscious_benefit).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, white_and_asian_american_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, formal_equality_litigants).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, beneficiaries_of_race_conscious_remediation).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, underrepresented_minority_applicants_in_holistic_review).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, diversity_program_administrators).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, individual_rights_bearer_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, anti_classification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compete for admission or employment slots under a rule that forbids institutions from weighing race as a plus-factor for other applicants. Under this reading, the clause secures their claim to be judged without racial classification burdening their file; they experience the rule as the vindication of an individual right, not as a group entitlement.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, white_and_asian_american_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Previously benefited from race-conscious consideration intended to offset structural disadvantage. Under the colorblind reading, that consideration itself becomes the constitutional violation, so this population loses a specific admissions or contracting advantage; from inside this reading, that loss is not counted as harm to a group interest because groups are not the relevant unit.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, underrepresented_minority_applicants_in_holistic_review, payer,
    moderate, biographical, constrained, national).

% Universities, employers, and agencies that built admissions, hiring, and contracting frameworks around race-conscious diversity goals must dismantle or redesign those frameworks under litigation and enforcement pressure, absorbing compliance costs and losing a policy tool they consider central to their institutional mission.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, diversity_program_administrators, payer,
    institutional, generational, constrained, national).

% Bring and win cases establishing that any racial classification, remedial or otherwise, triggers strict scrutiny and is presumptively invalid. They administer the doctrine forward through litigation strategy, amicus coordination, and test-case selection, and could in principle shift strategy toward a different reading if the coalition's priorities changed.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, civil_rights_litigators, agenda_setter,
    organized, generational, mobile, national).

% Adjudicate whether a given classification survives strict scrutiny, applying the colorblind reading's formal rule that race is never a permissible sorting criterion absent an extraordinarily narrow remedial showing tied to specific identified discrimination. Courts enforce the rule but do not personally collect from its operation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, federal_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Would argue that group-based historical subordination created durable structural disadvantage that a purely individual, ahistorical rule cannot see or remedy. Their group-level claim is precisely what this reading declines to recognize as a cognizable unit of analysis, so their objection is structurally excluded from the doctrine's own terms rather than answered by it.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historically_subordinated_racial_groups, excluded,
    powerless, generational, trapped, national).

% Analyze whether the colorblind reading is a coherent extension of the Fourteenth Amendment's original meaning or a doctrinal innovation that forecloses remedial and diversity-based readings once dominant elsewhere in equal protection jurisprudence.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable, rule-like standard — no governmental actor may classify individuals by race, for any purpose — that lets courts, legislatures, and institutions apply equal protection without case-by-case balancing of group histories or compelling-interest showings.
% TRANSFER_FUNCTION: Moves the benefit of formal non-classification to individuals who would otherwise be disadvantaged by race-conscious policies, and moves the cost of losing race-conscious remediation or diversity consideration onto individuals and institutions who relied on it.
% ABSENT_VOICES: Historically subordinated groups seeking group-conscious remedy for group-level harm are not heard on their own terms within this reading, because the reading's premise is that race-conscious argument is itself the constitutional wrong being cured; their objection can only be voiced as an argument against the reading, not within it.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished as the controlling doctrine, race-conscious admissions, contracting set-asides, and remedial programs currently prohibited or curtailed under strict scrutiny could resume or expand; entire compliance regimes built around 'race-neutral means first' would be rebuilt, and litigation strategy across two generations of civil rights law would need to reorient around a different governing premise.
% FOUNDING_PROBLEM: Built to solve the problem of government sorting citizens into racial castes — the founding-era problem of state-mandated racial subordination (slavery's aftermath, Jim Crow classification) that equal protection was designed to abolish by making race constitutionally irrelevant to legal treatment.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and the litigating coalition attest the problem — government racial sorting — remains live in any form, including remedial forms, and that colorblindness is the only stable cure. Historians of Reconstruction and scholars of the Fourteenth Amendment's remedial purpose, writing from outside the colorblind litigation coalition, attest the founding problem was specifically the subordination of Black Americans and that a rule barring remedial race-consciousness inverts rather than fulfills that original purpose — a dispute unresolved outside the reading's own proponents.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.14, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.14 at interval end) because, from this reading's own vantage, applying a categorical non-classification rule is formal rights vindication, not value transfer — the reading does not recognize a cognizable group loss when race-conscious programs are struck down, since groups are not the unit of constitutional concern. Suppression is authored moderate and rising (0.15 to 0.42) because the doctrine requires increasingly active judicial and executive enforcement — strict scrutiny review, disparate-treatment litigation, agency guidance rescission — to dismantle the alternative (race-conscious) infrastructure that grew up under sibling readings. Accessibility collapse is high (0.72): once the categorical rule is accepted, race-conscious policy design is nearly foreclosed as a legal option regardless of institutional intent. Resistance is substantial (0.60) because affected institutions and the excluded population actively contest the doctrine's premises in litigation, scholarship, and legislative proposals.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals who would be disadvantaged by race-conscious sorting are the structural beneficiaries — the rule's entire function, on this reading, is to secure their claim to non-classification, so their directionality sits near the beneficiary end. Individuals and institutions who relied on race-conscious remediation or diversity consideration are the structural targets — the doctrine's expansion directly removes a benefit they previously received, so their directionality sits near the target end. Civil rights litigators and federal courts are agenda-setters, not direct financial beneficiaries; they administer and extend the rule. Historically subordinated groups are excluded rather than merely disadvantaged — the doctrine's premise denies the relevance of the very group-level fact they would raise.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading is authored as a rope (a genuine coordination function: a clear, administrable rule reducing case-by-case racial balancing) rather than a snare, because from within this reading the rule is not extracting from a group but protecting individuals from group-based state action. This prevents mislabeling the doctrine as pure extraction. But the founding-problem mismatch (status: contested; corroboration split between litigating coalition and outside historians) is exactly the signal the R5 genealogy interview is built to surface: proponents attest the founding problem (state racial sorting in any direction) remains live and is solved by colorblindness; historians attest the founding problem was the specific subordination of Black Americans, a materially different target, and that extending 'no racial classification' to remedial measures may re-entrench rather than dissolve that subordination. The story does not resolve this — it routes it to omega and to the disappearance/founding_problem interview, which is where a contested-kernel reading's genealogy belongs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_vs_group_unit_of_analysis,
    'Is the individual the correct exclusive unit of constitutional analysis for a clause enacted specifically to address group-based subordination (the Reconstruction Amendments'' historical target), or does exclusive individual-level analysis erase the clause''s original remedial purpose?',
    'Historical and originalist analysis of the Fourteenth Amendment''s drafting history, floor debates, and contemporaneous Reconstruction-era legislation (e.g., the Freedmen''s Bureau Acts) enacted alongside it by the same Congress, which explicitly used racial classification for remedial purposes.',
    'If the amendment''s own framers used race-conscious remedy contemporaneously with ratification, the colorblind reading''s claim to sole fidelity to original meaning weakens substantially, supporting the remedial reading instead; if the framers intended a strict non-classification norm even for their own remedial actions, the colorblind reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_group_unit_of_analysis, conceptual, 'Whether original constitutional meaning supports individual-only or group-conscious remedial analysis.').

omega_variable(
    colorblind_reading_beneficiary_symmetry,
    'Does the colorblind reading, in practice, benefit racial groups symmetrically, or does its formal neutrality produce systematically asymmetric real-world effects favoring historically advantaged groups?',
    'Empirical study of admissions, contracting, and employment outcomes before and after colorblind-doctrine enforcement, disaggregated by race, to test whether formally neutral treatment produces materially neutral outcomes given unequal starting conditions.',
    'If outcomes are asymmetric despite formal neutrality, the reading''s low authored ε may understate its real-world extractive effect on historically disadvantaged groups even though the reading''s own internal logic does not recognize that effect as extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_reading_beneficiary_symmetry, empirical, 'Whether formal colorblindness produces symmetric or asymmetric real-world racial outcomes.').

omega_variable(
    kernel_reading_selection_pressure,
    'Which of the three sibling readings (colorblind, diversity, remedial) will control equal protection doctrine going forward, and what does that selection reveal about which reading the current judicial coalition treats as canonical?',
    'Track citation patterns, circuit splits, and Supreme Court composition/voting alignment across equal protection cases over the coming decade; a reading that consistently wins narrow majorities while facing sustained dissent is being selected by coalition power, not by uncontested doctrinal consensus.',
    'If the colorblind reading becomes fully dominant while the other two readings are formally foreclosed rather than merely losing, that is evidence the kernel''s ambiguity was resolved by judicial power rather than principled convergence — relevant to whether this reading''s permanence claim is doctrinally stable or contingent on court composition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether reading dominance reflects principled convergence or coalition power within the judiciary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1954, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_clause__colorblind_reading, theater_ratio, 1954, 0.03).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_clause__colorblind_reading, theater_ratio, 1995, 0.06).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__colorblind_reading, theater_ratio, 2003, 0.07).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_clause__colorblind_reading, theater_ratio, 2016, 0.08).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__colorblind_reading, theater_ratio, 2023, 0.09).
narrative_ontology:measurement(equa_tr_t2028, equal_protection_clause__colorblind_reading, theater_ratio, 2028, 0.1).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_clause__colorblind_reading, base_extractiveness, 1954, 0.05).
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.07).
narrative_ontology:measurement(equa_be_t1995, equal_protection_clause__colorblind_reading, base_extractiveness, 1995, 0.09).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__colorblind_reading, base_extractiveness, 2003, 0.1).
narrative_ontology:measurement(equa_be_t2016, equal_protection_clause__colorblind_reading, base_extractiveness, 2016, 0.12).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__colorblind_reading, base_extractiveness, 2023, 0.13).
narrative_ontology:measurement(equa_be_t2028, equal_protection_clause__colorblind_reading, base_extractiveness, 2028, 0.14).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_clause__colorblind_reading, suppression_requirement, 1954, 0.15).
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.22).
narrative_ontology:measurement(equa_su_t1995, equal_protection_clause__colorblind_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__colorblind_reading, suppression_requirement, 2003, 0.32).
narrative_ontology:measurement(equa_su_t2016, equal_protection_clause__colorblind_reading, suppression_requirement, 2016, 0.36).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__colorblind_reading, suppression_requirement, 2023, 0.4).
narrative_ontology:measurement(equa_su_t2028, equal_protection_clause__colorblind_reading, suppression_requirement, 2028, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equal_protection_clause kernel (colorblind, diversity, remedial). Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct claimed type: colorblind authors very low ε (0.14) and claims rope; the remedial reading is expected to author higher ε reflecting active redistribution toward historically subordinated groups and likely claims tangled_rope or rope depending on scope of remedy; the diversity reading sits structurally between, deriving its coordination function from compelling educational interest rather than either individual rights or group remediation. All three should be read together as competing constructions of the same textual kernel, not as one constraint measured three ways — per the ε-invariance principle, differing ε values across these files reflect genuinely different constraints, not measurement noise on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
