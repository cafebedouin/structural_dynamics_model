% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause — Antisubordination Reading (Caste-Focused)
 *   domain: Constitutional Law / Education Policy / Civil Rights
 *
 * SUMMARY:
 *   This constraint instantiates the antisubordination reading of the Equal
 *   Protection Clause kernel: the clause is read as targeting caste-like
 *   hierarchy imposed on historically oppressed racial groups, not
 *   classification by race as such. Under this reading, state action that
 *   entrenches subordination (segregation, exclusionary districting) is
 *   forbidden, while state action that dismantles subordination
 *   (race-conscious admissions and remedial programs aimed at historically
 *   subordinated groups) is constitutionally permitted — and dominant groups
 *   displaced by such measures cannot invoke equal protection to block them,
 *   because the clause's protective core was never theirs to claim. This
 *   reading has waxed and waned in doctrinal dominance: strongly present in
 *   Brown v. Board's anti-caste logic, contested through the
 *   diversity-rationale era, and substantially displaced (though not
 *   eliminated as an academic and dissenting position) by the colorblind
 *   reading's ascendance culminating in SFFA v. Harvard (2023). The
 *   extractiveness trajectory reflects rising political and legal
 *   contestation over WHO counts as a subordinated caste beneficiary versus a
 *   payer as the reading's application to Asian American applicants and to
 *   non-Black, non-Indigenous claimants became a central litigation
 *   battleground.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.58).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.42).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause — Antisubordination Reading (Caste-Focused)").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "Constitutional Law / Education Policy / Civil Rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, 'f8645a9a-52d8-4c95-8715-9cadb31d6e04').
narrative_ontology:cs_kernel_codification('f8645a9a-52d8-4c95-8715-9cadb31d6e04', fixed_text).
narrative_ontology:cs_authority_grounding('f8645a9a-52d8-4c95-8715-9cadb31d6e04', lineage).
narrative_ontology:cs_interpretation_layer_present('f8645a9a-52d8-4c95-8715-9cadb31d6e04').
narrative_ontology:cs_reading_relation('f8645a9a-52d8-4c95-8715-9cadb31d6e04', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('f8645a9a-52d8-4c95-8715-9cadb31d6e04', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('f8645a9a-52d8-4c95-8715-9cadb31d6e04', foundational, clause_targets_caste_not_classification).
narrative_ontology:cs_axiom_status(clause_targets_caste_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('f8645a9a-52d8-4c95-8715-9cadb31d6e04', clause_targets_caste_not_classification, empirically_contingent).
narrative_ontology:cs_axiom('f8645a9a-52d8-4c95-8715-9cadb31d6e04', foundational, dominant_groups_lack_standing_against_remedial_measures).
narrative_ontology:cs_axiom_status(dominant_groups_lack_standing_against_remedial_measures, holdable).
narrative_ontology:cs_axiom_grounding('f8645a9a-52d8-4c95-8715-9cadb31d6e04', dominant_groups_lack_standing_against_remedial_measures, deontological).
narrative_ontology:cs_reference_frame('f8645a9a-52d8-4c95-8715-9cadb31d6e04', reconstruction_anticaste_framework).
narrative_ontology:cs_drift_state('f8645a9a-52d8-4c95-8715-9cadb31d6e04', post_sffa_harvard_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f8645a9a-52d8-4c95-8715-9cadb31d6e04', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, black_college_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, indigenous_and_native_communities).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_group_applicants_in_zero_sum_admissions).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, asian_american_applicants_in_holistic_review_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose subordination the clause, under this reading, is understood to target — beneficiaries of race-conscious admissions, hiring, and redistricting measures justified as dismantling caste-like hierarchy rather than as mere classification. Their access to elite institutions and political representation depends on courts sustaining this reading against colorblind challenges.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_racial_groups, beneficiary,
    moderate, generational, constrained, national).

% Apply to selective universities where race-conscious admissions, under this reading, may weigh their applications favorably as a remedy for caste subordination rather than treating race as a forbidden classification. Their admission odds shift materially depending on which kernel reading a reviewing court adopts.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, black_college_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Communities whose land, sovereignty, and political status claims are sometimes framed as remedying subordination rather than as racial classification; this reading's caste focus can support or complicate their claims depending on whether tribal status is read as political (survives strict scrutiny debates) or racial (triggers the antisubordination analysis).
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, indigenous_and_native_communities, beneficiary,
    powerless, generational, trapped, national).

% Applicants from groups not read as subordinated castes who are displaced in seat-limited admissions processes by race-conscious remedial weighting. Under this reading, they cannot invoke equal protection against such measures because the clause is read as not protecting dominant groups from remedial dismantling of hierarchy — their exit is to apply elsewhere, but the seats they lose are specific and irreversible.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_group_applicants_in_zero_sum_admissions, payer,
    moderate, biographical, constrained, national).

% A group that is itself a historically subject-to-discrimination population but is treated, under many antisubordination-reading admissions schemes, as more proximate to the 'dominant' side of the ledger in zero-sum competitive admissions — bearing statistically documented penalties in holistic review. Their situation is the reading's most contested edge case: are they a subordinated caste or a non-beneficiary group whose equal-protection claims yield to the remedial project?
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, asian_american_applicants_in_holistic_review_systems, payer,
    moderate, biographical, constrained, national).

% Design and administer race-conscious admissions, hiring, and contracting programs, justifying them under the antisubordination reading as dismantling caste hierarchy rather than practicing forbidden classification. They must build evidentiary records connecting their measures to actual subordination-dismantling rather than diversity-for-its-own-sake, since courts have increasingly demanded documented subordination logic in this reading's absence.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, universities_and_state_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicate which kernel reading governs a given equal protection claim, deciding whether a challenged state action entrenches or dismantles hierarchy. Their choice of reading — antisubordination versus colorblind versus remedial — is frequently outcome-determinative and is itself contested doctrine, not settled law.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_courts, observer,
    institutional, civilizational, analytical, national).

% Litigate and lobby to entrench the antisubordination reading in doctrine, framing challenged measures as caste-dismantling. Increasingly excluded from a Supreme Court majority that has moved toward the colorblind reading, leaving this reading's institutional foothold narrower than its doctrinal ambition.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework distinguishing state action that perpetuates racial caste (forbidden) from state action that dismantles it (permitted), allowing race-conscious remedial and admissions programs to survive equal protection challenges when the state can show they target subordination rather than merely classify by race.
% TRANSFER_FUNCTION: Moves institutional seats, contracts, and political representation opportunities toward groups the state or reviewing court identifies as historically subordinated castes, and away from applicants and competitors in dominant or ambiguously-positioned groups who would otherwise have received them under a strictly individualized, non-race-conscious process.
% ABSENT_VOICES: Applicants displaced by race-conscious remedial measures rarely have standing to challenge the underlying subordination diagnosis itself — they can only challenge whether a given program is 'narrowly tailored,' not whether the antisubordination premise correctly maps their own group's status. Asian American plaintiff groups and dominant-group applicants argue the caste framework silently reclassifies them without a hearing on whether they belong in the 'dominant' category at all.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading were abandoned in favor of a strict colorblind reading, virtually all race-conscious admissions, contracting set-asides, and districting remedies justified on subordination grounds would become constitutionally infirm overnight — as substantially occurred after Students for Fair Admissions v. Harvard (2023) moved doctrine toward the colorblind reading. Universities and agencies would have to redesign programs around facially race-neutral proxies, and beneficiary groups would lose a doctrinal basis for direct racial remedies.
% FOUNDING_PROBLEM: The Reconstruction-era Equal Protection Clause was drafted principally to dismantle the legal architecture of racial caste following slavery — Black codes, exclusion from citizenship incidents, and systematic subordination — not to impose a general rule against any use of racial classification by government.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Reconstruction and scholars of the Fourteenth Amendment's drafting history (outside current litigating parties) document that the framing Congress explicitly authorized race-conscious relief measures (e.g., Freedmen's Bureau legislation) contemporaneously with ratification, supporting the antisubordination genealogy. However, a competing originalist scholarly tradition — also outside benefiting parties — reads the same history as supporting a general nondiscrimination principle rather than a caste-specific one; the historical record itself is contested, not merely the modern application.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58, moderate-high: substantial and rising because the group of payers (dominant-group and, contestedly, Asian American applicants) bears a concrete, individualized cost (lost admission or contract seats) whose scale and legitimacy is disputed even among those sympathetic to remedial aims. Suppression is authored at 0.42 (moderate): dominant-group applicants retain full ability to litigate and to apply to other institutions — the constraint does not trap them, but it does foreclose their capacity to win an equal-protection claim against subordination-remedying measures under this reading, which is a real, non-trivial doctrinal foreclosure. Theater ratio is low-moderate (0.2): the caste-subordination diagnosis in most authored programs is not purely performative — universities and agencies that survive strict scrutiny under this reading generally must show documented evidentiary linkage to subordination, though critics argue some programs invoke the framework more rhetorically than rigorously. Accessibility collapse is moderate (0.35): colorblind and remedial readings remain live doctrinal alternatives, so the antisubordination reading has not foreclosed all alternative equal-protection frameworks — it competes with them in ongoing litigation. Resistance is high (0.72): the colorblind reading's rise, especially post-SFFA, represents sustained, organized, and increasingly successful resistance to this reading from litigants, a shifting judicial majority, and state legislatures banning race-conscious admissions.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated racial groups sit at the beneficiary end: the reading's entire justificatory structure exists to authorize measures that flow resources, seats, and political power toward them, and it explicitly bars dominant-group equal-protection claims against those flows. Dominant-group applicants and Asian American applicants in zero-sum admissions sit toward the target end: they bear the concrete transfer cost and, under this reading specifically, cannot claim equal protection's core guarantee against it — their doctrinal shield is weaker here than under the colorblind reading, which is precisely the structural delta this reading produces relative to its siblings. Universities function as agenda-setters administering the transfer, and civil rights organizations are the reading's institutional champions whose exit options have narrowed as the reading's doctrinal support has eroded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview is genuinely contested rather than settled: this reading holds that the clause's founding problem (dismantling racial caste) remains live wherever documented subordination persists, so the arrangement has not gone mandatrophic by its own lights. But the colorblind reading's corroborating historical scholars argue the founding problem, properly read, was general nondiscrimination — meaning what antisubordination proponents call live remedial necessity, colorblind proponents call an arrangement that has outlived (or never matched) its textual mandate. The classification prevents mislabeling this dispute as resolved: it is not that one side has been vindicated by history, but that the kernel itself supports multiple genealogies, and this story's founding_problem_status is authored 'contested' precisely to avoid asserting a false resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asian_american_caste_status_ambiguity,
    'Do Asian American applicants belong to a group the antisubordination reading recognizes as historically subordinated (making remedial measures burdening them internally inconsistent with the reading''s own logic), or are they properly classified as non-beneficiaries whose equal-protection claims yield to the remedial project for other groups?',
    'Doctrinal development addressing whether historical anti-Asian discrimination (Chinese Exclusion Act, internment, quotas) qualifies as caste subordination under this reading''s own test, and whether current statistical admissions penalties against Asian American applicants constitute the kind of hierarchy-entrenchment the reading forbids.',
    'If Asian Americans are recognized as a subordinated group under this reading''s own framework, then admissions penalties against them become internally contradictory rather than externally contested — reclassifying them as victims of caste-entrenchment rather than payers in a remedial scheme, which would sharply reduce this constraint''s coherence as currently practiced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asian_american_caste_status_ambiguity, conceptual, 'Whether Asian American applicants are beneficiaries or victims under the antisubordination reading''s own internal logic.').

omega_variable(
    reading_selection_authority,
    'Which body has final authority to select among the antisubordination, remedial, and colorblind readings of the Equal Protection Clause, and is that selection itself principled or a function of judicial composition?',
    'Track Supreme Court composition changes against equal-protection doctrine shifts (Brown-era anti-caste reasoning, Bakke/Grutter remedial compromise, SFFA colorblind consolidation) to assess whether reading selection tracks legal reasoning or appointment politics.',
    'If reading selection tracks appointments rather than principled doctrinal development, this constraint''s persistence and reversal are exogenous to legal reasoning proper, undermining claims that any single reading represents the clause''s ''true'' meaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_authority, conceptual, 'Whether kernel-reading selection is doctrinally principled or politically contingent.').

omega_variable(
    tribal_sovereignty_racial_political_boundary,
    'Are indigenous/tribal classifications under this reading properly treated as political (surviving rational basis under Morton v. Mancari) or racial (triggering the antisubordination/strict scrutiny analysis), and does this reading''s caste framework resolve or merely relocate that ambiguity?',
    'Track post-SFFA litigation testing whether the political-classification doctrine for tribal status survives the broader retreat from race-conscious frameworks.',
    'If tribal political-status doctrine collapses into the general racial-classification analysis, indigenous communities'' beneficiary status under this reading becomes far more precarious than currently authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tribal_sovereignty_racial_political_boundary, empirical, 'Whether tribal classification is genuinely exempt from this reading''s racial-classification analysis or merely provisionally so.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equal_protection_kernel__antisubordination_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__antisubordination_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__antisubordination_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_kernel__antisubordination_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__antisubordination_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1868, 0.25).
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement(equa_be_t2016, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2016, 0.52).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2023, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1868, 0.15).
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.28).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement(equa_su_t2016, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2016, 0.42).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2023, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
