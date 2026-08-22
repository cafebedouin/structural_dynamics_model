% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial/Diversity Reading (Race-Conscious Admissions)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the remedial/diversity reading of the equal
 *   protection kernel, tracked from Bakke (1978) through the 2023 rulings
 *   that substantially foreclosed its application to explicit race-conscious
 *   admissions. Under this reading, the clause is read to permit, not forbid,
 *   race-conscious state action where narrowly tailored to a compelling
 *   interest — either remedying documented historical exclusion or achieving
 *   educational diversity. The reading created a durable but increasingly
 *   contested admissions practice at selective institutions: a genuine
 *   coordination function (redressing measurable historical exclusion,
 *   pursuing pedagogical diversity benefits) operating alongside a real
 *   transfer of marginal admissions slots away from applicants who would have
 *   been admitted under a race-blind baseline. This is a distinct constraint
 *   from the colorblind_reading (which finds the clause forbids any racial
 *   classification, epsilon much lower for beneficiaries, near-zero
 *   coordination function, framed instead as a mountain-like categorical
 *   rule) and from the antisubordination_reading (which locates the
 *   compelling interest in dismantling caste hierarchy specifically, not
 *   diversity generically, and would have a different beneficiary/victim
 *   mapping tied to subordination status rather than racial category per se).
 *   Each reading is authored as its own constraint with its own epsilon; this
 *   file's epsilon (0.42) reflects the remedial/diversity reading's own
 *   increasingly strained empirical defense of narrow tailoring, not the
 *   colorblind reading's near-zero extraction or the antisubordination
 *   reading's differently-scoped extraction.
 *
 * KEY AGENTS:
 *   - historically_excluded_applicant_groups: primary intended beneficiary (powerless/trapped) — the constraint exists structurally to redress conditions they cannot individually exit
 *   - universities_seeking_diversity_rationale: agenda_setter and institutional beneficiary (institutional/arbitrage) — designs and administers the narrow-tailoring standard, bears litigation risk, retains legitimacy and enrollment benefits
 *   - marginal_rejected_applicants: primary payer (moderate/constrained) — bears the direct, concentrated cost in a single admissions cycle
 *   - asian_american_applicant_cohort: organized payer (organized/constrained) — statistically disadvantaged group that has mobilized litigation against the reading's operation
 *   - federal_judiciary: analytical observer (institutional/analytical) — adjudicates and has substantially narrowed this reading's doctrinal space
 *   - civil_rights_organizations: excluded voice (organized/constrained) — advances the antisubordination alternative from outside the controlling framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.42).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.38).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause — Remedial/Diversity Reading (Race-Conscious Admissions)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, 'facb32e6-b8a4-404c-9aef-453a75394a98').
narrative_ontology:cs_kernel_codification('facb32e6-b8a4-404c-9aef-453a75394a98', fixed_text).
narrative_ontology:cs_authority_grounding('facb32e6-b8a4-404c-9aef-453a75394a98', lineage).
narrative_ontology:cs_interpretation_layer_present('facb32e6-b8a4-404c-9aef-453a75394a98').
narrative_ontology:cs_reading_relation('facb32e6-b8a4-404c-9aef-453a75394a98', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('facb32e6-b8a4-404c-9aef-453a75394a98', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('facb32e6-b8a4-404c-9aef-453a75394a98', foundational, race_conscious_remedy_permissible_for_compelling_interest).
narrative_ontology:cs_axiom_status(race_conscious_remedy_permissible_for_compelling_interest, overridden).
narrative_ontology:cs_axiom_grounding('facb32e6-b8a4-404c-9aef-453a75394a98', race_conscious_remedy_permissible_for_compelling_interest, conventional).
narrative_ontology:cs_axiom('facb32e6-b8a4-404c-9aef-453a75394a98', secondary, documented_historical_exclusion_grounds_affirmative_state_obligation).
narrative_ontology:cs_axiom_status(documented_historical_exclusion_grounds_affirmative_state_obligation, holdable).
narrative_ontology:cs_axiom_grounding('facb32e6-b8a4-404c-9aef-453a75394a98', documented_historical_exclusion_grounds_affirmative_state_obligation, empirically_contingent).
narrative_ontology:cs_reference_frame('facb32e6-b8a4-404c-9aef-453a75394a98', post_reconstruction_remedial_purpose_framework).
narrative_ontology:cs_drift_state('facb32e6-b8a4-404c-9aef-453a75394a98', post_sffa_2023_ruling, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('facb32e6-b8a4-404c-9aef-453a75394a98', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_applicant_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_seeking_diversity_rationale).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, marginal_rejected_applicants).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, asian_american_applicant_cohort).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, compelling_diversity_interest_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, narrow_tailoring_remedial_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups subject to documented historical exclusion from higher education (segregation, quota systems, redlining-adjacent barriers) receive a plus-factor consideration in admissions intended to offset the durable effects of that exclusion. They have no individual exit from the historical condition being remedied; the constraint operates on their behalf without their direct control over its administration.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_applicant_groups, beneficiary,
    powerless, generational, trapped, national).

% Selective universities design admissions processes that weigh race as one factor among many, justified by an institutional interest in educational diversity and/or remediation of documented exclusion. They administer the documentation, set the narrow-tailoring standard internally, and benefit from the legitimacy and enrollment outcomes the arrangement provides, while bearing litigation risk if the tailoring is found insufficiently narrow.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_seeking_diversity_rationale, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, universities_seeking_diversity_rationale, beneficiary).

% Applicants who, under a strictly race-blind process, would have been admitted, but are displaced by the plus-factor consideration given to other applicants. They bear the direct cost of the remedial policy in the single admissions cycle that matters to them; their only recourse is litigation, alternative institutions, or accepting the displacement.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, marginal_rejected_applicants, payer,
    moderate, biographical, constrained, national).

% As a group, statistically disadvantaged relative to a race-blind baseline at several selective institutions under holistic review practices tied to this reading. They have mobilized litigation and public advocacy against the practice, arguing the remedial rationale functions as a ceiling on their admission rates even though they are not the group the remedy targets.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, asian_american_applicant_cohort, payer,
    organized, generational, constrained, national).

% Adjudicates whether specific admissions programs satisfy strict scrutiny — whether the diversity or remedial interest is compelling and the means narrowly tailored. Their doctrine has narrowed the space this reading occupies over decades, most sharply in recent rulings restricting explicit race-conscious admissions.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% Advocate for the antisubordination framing — that the remedy should track subordination status rather than diversity interest generically — and argue the remedial/diversity reading's individualized, holistic-factor design under-delivers on actual redress. They participate as amici but do not control the doctrinal framing universities and courts have settled on.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_organizations, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows selective institutions to pursue a compelling educational interest in a diverse student body and to remedy documented historical exclusion, coordinating admissions policy with a constitutional commitment to equal protection that is read as permitting, not just forbidding, race-conscious action.
% TRANSFER_FUNCTION: Moves a marginal admissions slot from an applicant who would have been admitted under a strictly race-blind process to an applicant benefiting from the plus factor; also moves institutional legitimacy and diversity-linked reputational and pedagogical benefits toward the university.
% ABSENT_VOICES: Antisubordination advocates argue the diversity-interest framing dilutes the remedial function into a generic pedagogical good detached from actual subordination status; colorblind advocates argue any race consciousness is itself the harm. Both groups participate in litigation and public debate but do not control how universities operationalize the standard.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, selective university admissions would revert to race-blind processes (as recent doctrine has substantially compelled), enrollment composition at elite institutions would shift measurably, and the constitutional basis for institutional diversity rationales would need to be reconstructed under the colorblind reading or abandoned; both beneficiary and payer groups have organized entire advocacy infrastructures around this reading's survival or defeat.
% FOUNDING_PROBLEM: Documented, state-sanctioned historical exclusion of racial minorities from higher education (segregation, exclusionary admissions, discriminatory funding) left durable underrepresentation that race-blind processes alone were argued not to remedy quickly or at all.
% FOUNDING_PROBLEM_CORROBORATION: Universities and civil rights litigators attest the underlying exclusion's effects remain measurable in enrollment and wealth data. The Supreme Court majority in recent rulings attests the specific remedial/diversity doctrinal vehicle has become unmoored from measurable end points and functions as indefinite racial balancing; this corroboration comes from outside the beneficiary set and is precisely why this reading's operative space has narrowed.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) reflects a genuine but partial coordination function: the remedial/diversity rationale does redress documented historical exclusion and does produce pedagogical diversity value, but the concentration of cost on a narrow, identifiable set of marginal rejected applicants — rising as institutions leaned more heavily on holistic race-conscious review over the measured interval — pushes this above a pure-rope reading. Suppression (0.38) is moderate: the reading does not foreclose alternative admissions criteria, but it does require universities and courts to actively police the boundary of 'narrow tailoring,' and institutions face real legal exposure for departing from the accepted formula. Theater ratio (0.28) captures the increasing gap identified by courts between stated narrow-tailoring rhetoric (individualized holistic review, no quotas) and functional racial balancing outcomes documented in litigation discovery — a gap that widened over the interval and directly motivated the 2023 doctrinal shift. Resistance (0.72) is high because this reading has been continuously and organizedly contested from multiple directions (colorblind litigants, antisubordination advocates, and the judiciary itself) for its entire operative life.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded applicant groups are the intended structural beneficiaries but hold essentially no power over the arrangement's administration (powerless/trapped) — the remedy is done on their behalf, not by them, which affects how their benefit is realized versus how they experience the constraint's operation. Universities hold agenda-setting power and arbitrage-grade exit (they can adjust or abandon the rationale as doctrine shifts) and capture institutional legitimacy benefits — the classic asymmetric-extraction-with-real-coordination-function tangled rope signature. Marginal rejected applicants and the organized Asian American applicant cohort are targets: their exit options are constrained (litigate, apply elsewhere, or accept displacement), and the cost lands on them in a single consequential cycle even though the policy's stated purpose is generational and structural.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is load-bearing here: this reading's coordination function is not cover — there is documented historical exclusion with measurable lingering effects, and the diversity interest has genuine pedagogical grounding recognized repeatedly by courts over four decades. But the classification also refuses to launder the concentrated cost imposed on marginal rejected applicants as costless or purely diffuse. The founding_problem_status is authored as 'contested' rather than 'dead' precisely because the corroboration is genuinely split between parties with no shared framework — this is the honest state of a constraint whose remedial premise is defended by its administrators and increasingly rejected by its adjudicating authority as untethered from a measurable endpoint. Treating this as a Mountain (as a naive read of 'compelling government interest' language might invite) would launder the real, identifiable victim set; treating it as a pure Snare would erase the genuine historical-exclusion coordination function that motivated Bakke, Grutter, and Fisher.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_rationale_vs_remedial_rationale_drift,
    'Did the remedial_reading''s operative doctrine drift from a genuine historical-exclusion remedy (Bakke''s initial framing rejected pure remediation without judicial/legislative findings, but subsequent cases increasingly relied on generic ''educational diversity'' benefits rather than documented exclusion) into a diversity rationale doing different, more diffuse work than the remedial rationale it descended from?',
    'Doctrinal history analysis comparing the specificity of exclusion findings required in early remedial cases (Bakke, Wygant) against the diversity-interest standard applied in later cases (Grutter, Fisher) — does the later standard still require documented, particularized historical exclusion, or has ''compelling interest'' become detached from any specific remedial finding?',
    'If the diversity rationale substantially replaced the remedial rationale without equivalent evidentiary rigor, the coordination function this reading claims (documented exclusion) is weaker than authored, and the extraction component is proportionally larger than the 0.42 epsilon reflects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_rationale_vs_remedial_rationale_drift, conceptual, 'Whether the diversity-interest branch of this reading retained the evidentiary discipline of its remedial-exclusion origin.').

omega_variable(
    kernel_reading_dominance_transition,
    'This reading was the operative controlling doctrine from 1978-2023 but has been substantially foreclosed by the colorblind_reading in Students for Fair Admissions v. Harvard/UNC (2023). Is the remedial_reading now a historical constraint (dead reading) or does it persist in narrower operative form (e.g., military academies, some state contexts, non-admissions remedial contexts)?',
    'Track post-2023 litigation and legislative activity for surviving applications of race-conscious remediation outside university admissions (military service academy admissions were explicitly carved out in dicta; state remedial contracting programs continue under different scrutiny standards).',
    'If the reading survives only in narrow carve-outs, this story''s interval should be understood as bounded (1978-2023) rather than ongoing, and beneficiary/victim relationships in surviving contexts may need separate constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_dominance_transition, empirical, 'Whether this reading is now historical (foreclosed) or persists in narrowed operative domains.').

omega_variable(
    framing_choice_diversity_vs_remedy_omega,
    'This story treats ''remedy documented historical exclusion'' and ''achieve compelling diversity interest'' as a single reading, per the manifest''s framing. An alternative framing would split these into two distinct constraints with potentially different epsilon values: pure historical remedy (narrower beneficiary class, stronger evidentiary requirement, arguably lower extraction because more tightly bounded) versus generic diversity interest (broader beneficiary class, weaker evidentiary requirement, arguably higher extraction because less bounded and more susceptible to the theater-ratio drift documented above).',
    'Compare epsilon under a decomposed framing: author separate remedial_exclusion_reading and diversity_interest_reading constraints and check whether their epsilon values diverge enough to trigger the epsilon-invariance decomposition rule.',
    'If the two rationales have substantially different epsilon values when isolated, this story''s single epsilon (0.42) is an average that may mask a mountain-adjacent remedial-exclusion core and a more extractive diversity-interest periphery — the manifest''s framing choice to bundle them was followed here, but a future revision might decompose further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_diversity_vs_remedy_omega, conceptual, 'Whether the remedial-exclusion and diversity-interest rationales should be split into separate ε-invariant constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__remedial_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__remedial_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__remedial_reading, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(equa_tr_t2020, equal_protection_kernel__remedial_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.33).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__remedial_reading, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement(equa_be_t2020, equal_protection_kernel__remedial_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.24).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.29).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__remedial_reading, suppression_requirement, 2013, 0.33).
narrative_ontology:measurement(equa_su_t2020, equal_protection_kernel__remedial_reading, suppression_requirement, 2020, 0.36).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__remedial_reading, 0.15).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equal_protection_kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. The colorblind_reading finds near-zero coordination function and near-zero extraction from race-conscious action itself (it locates the harm in any classification, full stop) but recognizes suppression in the form it now imposes on remedial policy design. The antisubordination_reading shares this reading's beneficiary orientation toward historically excluded groups but ties the compelling interest to dismantling caste hierarchy specifically rather than diversity generically, producing a narrower beneficiary/victim mapping and likely a different (probably lower, more tightly bounded) epsilon. All three readings are linked here; contamination or doctrinal shifts affecting one (e.g., the 2023 ruling substantially foreclosing this reading in admissions contexts) propagate pressure onto how the siblings are read going forward.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
