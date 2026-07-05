% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial (Group-Subordination) Reading
 *   domain: constitutional_law/education_policy/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the remedial reading of the Equal Protection
 *   Clause: the claim that the clause is not satisfied by formal neutrality
 *   alone but affirmatively requires race-conscious action to remediate the
 *   durable effects of historical state-sanctioned group subordination. This
 *   is distinct from the diversity reading (which justifies
 *   race-consciousness by present-day pedagogical benefit to all students,
 *   not historical restitution) and from the colorblind reading (which
 *   forbids racial classification regardless of its remedial purpose). Under
 *   this reading, designated historically subordinated groups enter the
 *   beneficiary set and individual members of non-designated groups enter the
 *   victim set — the classification runs by group history, not by present
 *   disadvantage or present classification neutrality. The constraint is
 *   authored as a scaffold: its own internal logic requires a sunset when the
 *   remediated gap closes, distinguishing it from an open-ended entitlement.
 *   In practice this sunset condition has proven difficult to operationalize,
 *   which is itself part of what the 1978-2023 measurement series traces.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_minorities: beneficiary group whose historical exclusion grounds the remedial claim
 *   - non_preferred_group_individual_applicants: bears the cost of group-based remediation regardless of personal history
 *   - asian_american_applicant_cohorts_at_selective_institutions: a group with its own history of exclusion that the remedial ledger does not credit, illustrating the reading's selection problem
 *   - selective_admissions_institutions: administers the classification and retains discretion over its design and duration
 *   - federal_judiciary: adjudicates whether the remedial rationale meets strict scrutiny and has a cognizable endpoint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.61).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.52).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Clause — Remedial (Group-Subordination) Reading").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/education_policy/political_philosophy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, 'f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0').
narrative_ontology:cs_kernel_codification('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', fixed_text).
narrative_ontology:cs_authority_grounding('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', lineage).
narrative_ontology:cs_interpretation_layer_present('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0').
narrative_ontology:cs_reading_relation('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', foundational, equal_protection_requires_group_conscious_restitution).
narrative_ontology:cs_axiom_status(equal_protection_requires_group_conscious_restitution, holdable).
narrative_ontology:cs_axiom_grounding('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', equal_protection_requires_group_conscious_restitution, deontological).
narrative_ontology:cs_axiom('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', foundational, historical_subordination_generates_present_remedial_entitlement).
narrative_ontology:cs_axiom_status(historical_subordination_generates_present_remedial_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', historical_subordination_generates_present_remedial_entitlement, empirically_contingent).
narrative_ontology:cs_reference_frame('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', reconstruction_era_remedial_legislation).
narrative_ontology:cs_drift_state('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', post_sffa_2023, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('f1cf37d9-f6b7-4e90-9875-8c89ca1d0aa0', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, black_applicant_cohorts_at_selective_institutions).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_group_individual_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, asian_american_applicant_cohorts_at_selective_institutions).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, anti_subordination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose ancestors were subjected to state-sanctioned exclusion (slavery, Jim Crow, redlining, exclusionary immigration law) and who continue to show measurable disparities in wealth, admissions outcomes, and institutional representation traceable to those policies. The remedial reading holds that closing these gaps requires explicit group-conscious action by admissions offices, employers, and legislatures, not merely formal neutrality going forward.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_minorities, beneficiary,
    organized, generational, constrained, national).

% Individual applicants who receive an admissions or hiring boost under race-conscious remediation policies justified by group subordination history rather than diversity-of-viewpoint rationale. Their exit option is limited — they cannot opt out of the historical classification that both burdens and, under this reading, benefits them.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, black_applicant_cohorts_at_selective_institutions, beneficiary,
    moderate, biographical, constrained, national).

% Individual applicants who are not members of a group designated as historically subordinated and who are denied admission or a position in favor of a designated-group applicant with a comparable or lower individual qualification profile. They bear the constraint's cost regardless of their own personal history, wealth, or family circumstance — the remedial reading assigns cost by group membership, not by individual desert or advantage.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_group_individual_applicants, payer,
    moderate, biographical, constrained, national).

% A group empirically shown in litigation (e.g., SFFA v. Harvard) to face a statistically lower admission probability at equivalent academic metrics under race-conscious policies, despite not being the group whose historical subordination the policy is designed to remedy and despite themselves having faced discrete historical exclusion (Chinese Exclusion Act, internment). Their situation illustrates that the remedial reading's group ledger does not map cleanly onto all historically subordinated groups — it selects which histories count.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, asian_american_applicant_cohorts_at_selective_institutions, payer,
    organized, generational, constrained, national).

% Universities and employers design and administer the race-conscious remedial policy, deciding which groups qualify as historically subordinated, what weight remediation receives in the selection process, and how long the policy will run. They face litigation risk and reputational cost either way, and retain discretion the other stakeholders lack over how the classification is drawn and applied.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, selective_admissions_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Litigate and lobby to establish and defend the remedial reading as the correct constitutional interpretation, framing equal protection as fundamentally about dismantling caste-like group hierarchies rather than protecting individual rights-bearers from classification. They shape which histories are legible as 'subordination' for remedial purposes.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, civil_rights_legal_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, civil_rights_legal_advocacy_organizations, observer).

% Adjudicates whether remedial race-consciousness satisfies strict scrutiny, whether the remedial rationale is distinct from and less durable than the diversity rationale, and whether the policy has a workable end-point. The Supreme Court's 2023 SFFA decision effectively foreclosed the remedial rationale as a standalone justification for admissions, leaving this reading live mainly in employment discrimination remedies and narrowly-tailored consent decrees.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% Not yet born or not yet affected; would inherit either a society where group gaps have closed and the remedial mandate is retired, or one where the classification has become permanent bureaucratic practice untethered from an actual closing gap. They have no voice in whether the sunset condition is ever actually triggered.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, future_generations_post_remediation, excluded,
    powerless, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective response to documented, government-caused group subordination by directing institutions to weigh group history in allocation decisions, on the theory that formal neutrality alone reproduces rather than remedies inherited disadvantage.
% TRANSFER_FUNCTION: Moves admission slots, jobs, and contracts from higher-qualified-on-formal-metrics applicants outside the designated groups to applicants within designated groups, justified as restitution for state-caused historical harm rather than as a forward-looking diversity benefit.
% ABSENT_VOICES: Individual non-preferred applicants rarely get to contest the historical-harm calculus itself in the admissions process — they experience only the outcome. Groups with genuine historical exclusion who are not on the designated list (e.g., certain Asian-American subgroups, some religious minorities) are also structurally absent from the ledger the remedial reading constructs.
% DISAPPEARANCE_RATIONALE: Beneficiary institutions and civil rights organizations would say gains built over decades erode without the remedial mandate; payer-group litigants and colorblind-reading proponents would say the world simply returns to formally neutral, individual-regarding treatment with no rearrangement beyond ending an unconstitutional practice. The Court's 2023 ruling means this is not hypothetical — the remedial rationale specifically has already been substantially withdrawn from admissions, and the two camps dispute whether anything of value was lost.
% FOUNDING_PROBLEM: Centuries of state-enforced racial subordination (slavery, Jim Crow, redlining, discriminatory immigration and naturalization law) produced durable, measurable group disparities that formal legal equality alone did not close, prompting the argument that the Equal Protection Clause itself compels affirmative group-conscious remedy, not just a ban on future classification.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians and social scientists outside the litigating parties document persistent, measurable racial wealth and opportunity gaps traceable to state action, corroborating that some founding problem remains empirically live. However, the Supreme Court majority in SFFA v. Harvard (2023), writing from outside the beneficiary coalition, held that the remedial rationale specifically is not a judicially cognizable, sufficiently measurable interest capable of principled termination — a corroborating-but-adverse outside voice concluding the remedial framing itself, as a legal doctrine, lacks a workable endpoint even if the underlying disparities are real.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, contested).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.61 — substantial but not maximal — because the remedial reading has a genuine coordination logic (closing state-caused disparities) layered under real cost transfer to non-designated individuals; it is not pure extraction because a documented historical harm underlies the group ledger. Suppression is authored at 0.52, rising over the measurement window, tracking the increasing institutional and legal infrastructure (diversity offices, compliance regimes, litigation defense apparatus) required to sustain race-conscious remediation against mounting legal challenge. Theater ratio is modest (0.22) — the remedial function is largely substantively pursued, though the absence of a triggered sunset by 2023 (SFFA notwithstanding) suggests growing performative persistence relative to the originally invoked historical-harm rationale.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups and their individual co-members sit toward the beneficiary end: the constraint transfers opportunity to them on the theory of restitution for state-caused harm, and their exit from the classification is not fully voluntary (you cannot opt out of your own historical group membership, but you can benefit from the remedy attached to it). Non-preferred group individuals and, distinctly, Asian-American applicant cohorts sit toward the target end: they bear the cost of the remedy regardless of individual circumstance, and their exit options are constrained by the fact that elite institutional access has few substitutes. The judiciary and legal advocacy organizations occupy an observer/agenda-setter axis — they do not personally collect or pay but shape whether and how the classification is drawn and defended.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is doing real work here: the remedial reading's own internal justification requires that it terminate once the historical gap it targets closes, which distinguishes genuine remediation from permanent group-based entitlement. The founding_problem_status is authored as contested rather than dead specifically to avoid two symmetric mislabeling errors — treating the constraint as a mountain (permanent natural feature of equal protection law, which the colorblind reading explicitly denies) or treating it as a pure snare (assuming no real historical harm underlies it, which the corroborating historical-disparity evidence contradicts). The SFFA ruling is read here as an outside-the-beneficiary-coalition judgment that the remedial rationale specifically lacks an operationalizable endpoint in the admissions context — evidence bearing on mandatrophy (has the mandate outlived its administrable form?) without resolving whether the underlying disparities the framework targets are themselves gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_diversity_rationale_collapse,
    'Is the remedial rationale (restitution for historical group subordination) structurally distinct from the diversity rationale (present pedagogical benefit), or do institutions in practice use diversity language as a legally survivable proxy for remedial intent?',
    'Discovery of internal admissions-committee deliberations and historical policy memoranda distinguishing stated diversity rationale from underlying remedial motivation; comparison of policy design before and after diversity rationale became the legally required framing (post-Bakke, post-Grutter).',
    'If institutions substantively pursue remediation under diversity cover, this story''s high ε and scaffold classification are the operative reality even where the diversity_reading story shows lower ε — the two readings would not be independently observed but nested, undermining ε-invariance unless the substitution itself is treated as evidence of which reading is actually governing practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_diversity_rationale_collapse, conceptual, 'Whether remedial and diversity rationales are structurally separable in actual institutional practice or merely in doctrinal language.').

omega_variable(
    sunset_condition_operationalizability,
    'Is there any empirically measurable condition under which proponents of the remedial reading would agree the remediation mandate should terminate, or is the sunset clause aspirational without an operational trigger?',
    'Examine whether any jurisdiction or institution has ever actually sunset a race-conscious remedial policy on the stated ground that the targeted historical gap closed, versus policies that persisted indefinitely or were terminated only by external legal compulsion (e.g., SFFA).',
    'If no institution has ever self-terminated on remediation-achieved grounds, the scaffold classification is contestable — the constraint may function as a piton in practice (declared temporary, never actually retired) despite its formally-declared sunset logic, which would matter for future stories tracking this constraint''s drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_condition_operationalizability, empirical, 'Whether the remedial reading''s sunset clause has ever been operationally triggered or is purely declarative.').

omega_variable(
    group_ledger_selection_problem,
    'What principle determines which historically excluded groups (e.g., Black Americans, Native Americans) are credited as beneficiaries of remediation and which historically excluded groups (e.g., certain Asian-American national-origin subgroups, some religious minorities) are not, despite each having a documented history of state-sanctioned exclusion?',
    'Comparative historical-legal analysis of which groups'' exclusion histories are treated as grounding remedial entitlement in case law and policy versus which are acknowledged as historical fact but not translated into remedial standing.',
    'If the selection is not principled but political/contingent, the remedial reading''s own internal logic (equal protection requires remedy for state-caused subordination) is in tension with its practical application, which would be relevant to whether the constraint''s beneficiary/victim structure is coherent or itself a site of unremediated inequality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(group_ledger_selection_problem, conceptual, 'Whether the list of remediation-eligible groups follows a principled criterion or contingent political selection.').

omega_variable(
    natural_kernel_vs_constructed_reading_ambiguity,
    'Is the remedial reading a genuine unpacking of what the Fourteenth Amendment''s framers and Reconstruction-era practice intended (an originalist-compatible reading, given the Freedmen''s Bureau and similar race-conscious remedial legislation passed by the same Congress), or a later constructed gloss serving present-day institutional interests in maintaining diversity-office infrastructure and litigation-defense capacity?',
    'Historical analysis of Reconstruction Congress''s own race-conscious remedial legislation as evidence of original public meaning, weighed against institutional-interest analysis of who currently administers and defends the remedial framework.',
    'If genuinely original, the remedial reading has a stronger claim to be a mountain-adjacent constitutional requirement rather than a constructed, revisable policy choice; if constructed, the scaffold/tangled-rope character is more clearly correct and the sunset logic more clearly overdue.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_kernel_vs_constructed_reading_ambiguity, conceptual, 'Whether the remedial reading reflects original constitutional meaning or a later constructed institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1954, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_clause__remedial_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement_basis(equa_tr_t1954, observed).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_clause__remedial_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_clause__remedial_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement_basis(equa_tr_t1995, observed).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_clause__remedial_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement_basis(equa_tr_t2003, observed).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_clause__remedial_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement_basis(equa_tr_t2013, observed).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_clause__remedial_reading, theater_ratio, 2023, 0.22).
narrative_ontology:measurement_basis(equa_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_clause__remedial_reading, base_extractiveness, 1954, 0.2).
narrative_ontology:measurement_basis(equa_be_t1954, observed).
narrative_ontology:measurement(equa_be_t1978, equal_protection_clause__remedial_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t1995, equal_protection_clause__remedial_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement_basis(equa_be_t1995, observed).
narrative_ontology:measurement(equa_be_t2003, equal_protection_clause__remedial_reading, base_extractiveness, 2003, 0.5).
narrative_ontology:measurement_basis(equa_be_t2003, observed).
narrative_ontology:measurement(equa_be_t2013, equal_protection_clause__remedial_reading, base_extractiveness, 2013, 0.56).
narrative_ontology:measurement_basis(equa_be_t2013, observed).
narrative_ontology:measurement(equa_be_t2023, equal_protection_clause__remedial_reading, base_extractiveness, 2023, 0.61).
narrative_ontology:measurement_basis(equa_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_clause__remedial_reading, suppression_requirement, 1954, 0.15).
narrative_ontology:measurement_basis(equa_su_t1954, observed).
narrative_ontology:measurement(equa_su_t1978, equal_protection_clause__remedial_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t1995, equal_protection_clause__remedial_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement_basis(equa_su_t1995, observed).
narrative_ontology:measurement(equa_su_t2003, equal_protection_clause__remedial_reading, suppression_requirement, 2003, 0.44).
narrative_ontology:measurement_basis(equa_su_t2003, observed).
narrative_ontology:measurement(equa_su_t2013, equal_protection_clause__remedial_reading, suppression_requirement, 2013, 0.48).
narrative_ontology:measurement_basis(equa_su_t2013, observed).
narrative_ontology:measurement(equa_su_t2023, equal_protection_clause__remedial_reading, suppression_requirement, 2023, 0.52).
narrative_ontology:measurement_basis(equa_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% This story, colorblind_reading, and diversity_reading are three constraint stories reading the same equal_protection_clause kernel. Each instantiates a structurally distinct constraint with its own ε, beneficiary/victim set, and classification: colorblind_reading is expected to compute closer to mountain/rope (no racial beneficiary group, minimal group-based extraction); diversity_reading is expected to compute as tangled_rope (present-benefit rationale for all students layered over real individual cost to non-preferred applicants); this remedial_reading is authored as scaffold given its declared-but-contested sunset logic and high ε from the historical-restitution mandate. Per the ε-invariance principle, these are NOT one constraint measured three ways — they are three constraints sharing a contested text, linked here via network edges rather than folded into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
