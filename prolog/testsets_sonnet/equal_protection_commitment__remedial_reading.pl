% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection as Anti-Subordination Mandate (Remedial Reading)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   Since Brown v. Board (1954) through the post-Civil Rights Act era, one
 *   reading of the Equal Protection Clause has held that its core commitment
 *   is anti-subordination, not mere anti-classification: government may not
 *   perpetuate a racial caste system, and correspondingly may use
 *   race-conscious remedies where necessary to dismantle documented, durable
 *   structural subordination. This reading underwrote school desegregation
 *   remedies, affirmative action in employment and contracting, and
 *   race-conscious admissions justified on remedial (not merely diversity)
 *   grounds. It has been in doctrinal retreat since Croson (1989) and Adarand
 *   (1995) imposed strict scrutiny on benign racial classifications, and was
 *   further narrowed by Students for Fair Admissions v. Harvard (2023), which
 *   rejected the diversity-interest rationale for admissions while leaving
 *   remedial rationales for documented discrimination in narrower doctrinal
 *   space.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection as Anti-Subordination Mandate (Remedial Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, 'ca629771-1083-4571-9084-2808e05a0afb').
narrative_ontology:cs_kernel_codification('ca629771-1083-4571-9084-2808e05a0afb', fixed_text).
narrative_ontology:cs_authority_grounding('ca629771-1083-4571-9084-2808e05a0afb', lineage).
narrative_ontology:cs_interpretation_layer_present('ca629771-1083-4571-9084-2808e05a0afb').
narrative_ontology:cs_reading_relation('ca629771-1083-4571-9084-2808e05a0afb', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('ca629771-1083-4571-9084-2808e05a0afb', equal_protection_commitment__diversity_reading, influences).
narrative_ontology:cs_axiom('ca629771-1083-4571-9084-2808e05a0afb', foundational, anti_subordination_is_the_core_commitment).
narrative_ontology:cs_axiom_status(anti_subordination_is_the_core_commitment, holdable).
narrative_ontology:cs_axiom_grounding('ca629771-1083-4571-9084-2808e05a0afb', anti_subordination_is_the_core_commitment, deontological).
narrative_ontology:cs_axiom('ca629771-1083-4571-9084-2808e05a0afb', foundational, group_level_remedy_permissible_for_group_level_historical_harm).
narrative_ontology:cs_axiom_status(group_level_remedy_permissible_for_group_level_historical_harm, holdable).
narrative_ontology:cs_axiom_grounding('ca629771-1083-4571-9084-2808e05a0afb', group_level_remedy_permissible_for_group_level_historical_harm, instrumental).
narrative_ontology:cs_reference_frame('ca629771-1083-4571-9084-2808e05a0afb', reconstruction_anti_caste_original_understanding).
narrative_ontology:cs_drift_state('ca629771-1083-4571-9084-2808e05a0afb', post_sffa_harvard_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('ca629771-1083-4571-9084-2808e05a0afb', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, remedial_program_administering_agencies).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, displaced_nonpreferred_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_racial_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anti_caste_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, substantive_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups subject to historic and ongoing structural subordination (segregation, exclusion from credit and education, discriminatory policing) receive preferential access under remedial programs — set-asides, targeted admissions, hiring preferences — justified as counteracting compounding disadvantage. They cannot exit the racial classification that both subordinated and now benefits them; the remedy is administered by institutions they do not control.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    moderate, generational, constrained, national).

% Universities, agencies, and municipalities design and defend race-conscious remedial programs, framing them as constitutionally compelled responses to documented subordination. They set eligibility criteria, defend the programs in litigation, and bear reputational and legal risk if challenged, but also gain legitimacy and mission fulfillment from administering them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, remedial_program_administering_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Individual applicants for a seat, contract, or position who are displaced by a preference extended to a member of a remedied group bear a concrete, individualized cost for a program targeting group-level historical harm they personally may not have caused. Their exit options are limited to litigation or seeking opportunity elsewhere in a competitive system.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, displaced_nonpreferred_applicants, payer,
    moderate, biographical, constrained, national).

% As a class, members of groups that benefited from historical exclusionary regimes now bear the diffuse and sometimes concentrated costs of remedial reallocation. They argue the remedy imposes present costs on individuals disconnected from the specific historical wrong, converting a group-level moral claim into an individual-level burden.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_racial_groups, payer,
    organized, generational, constrained, national).

% Courts adjudicate whether specific remedial programs are narrowly tailored to a compelling interest in dismantling identified subordination, or whether they exceed constitutional bounds. Doctrine here (strict scrutiny applied to benign classifications) determines which programs survive.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, federal_judiciary, observer,
    institutional, civilizational, analytical, national).

% Litigate to defend and expand remedial programs, framing equal protection doctrine itself as requiring anti-subordination measures. They gain legal and political capital from favorable rulings and shape the doctrinal architecture that administering agencies rely on.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, civil_rights_advocacy_organizations, agenda_setter).

% Advocates for the rival colorblind reading are not absent from public discourse but are structurally excluded from this reading's internal logic — the remedial reading treats their objection (that any racial classification perpetuates caste-thinking) as itself the mechanism of subordination's continuation, foreclosing their premise rather than debating it on the merits within this framework.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_constitutionalists, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to identify and dismantle durable structural subordination (a caste-like stratification along racial lines) that ordinary formally-neutral rules reproduce rather than correct, by permitting temporary race-conscious remedies tied to documented histories of exclusion.
% TRANSFER_FUNCTION: Moves educational seats, contracts, jobs, and other scarce allocations from individuals who would have received them under a race-neutral baseline to individuals from groups documented as subject to historical and ongoing subordination, justified as correcting a prior and continuing transfer running the other direction.
% ABSENT_VOICES: Individual displaced applicants often lack organized representation in the doctrinal debate, which is conducted between institutional litigants (civil rights organizations, state agencies, and organized opposition groups); the individualized cost borne by a specific displaced applicant is frequently invisible in appellate argument focused on group-level statistics and historical narrative.
% DISAPPEARANCE_RATIONALE: If the remedial reading were repudiated and race-conscious remedial programs categorically forbidden, universities, agencies, and employers would immediately have to redesign admissions, hiring, and contracting criteria; documented racial gaps in outcomes that the programs were built to narrow would likely widen without the classification-based lever, and civil rights litigation strategy would shift entirely toward disparate-impact and facially-neutral proxies.
% FOUNDING_PROBLEM: Formally equal treatment under law, applied to a society freshly emerged from centuries of state-enforced racial subordination (slavery, Black Codes, Jim Crow, redlining, exclusion from the New Deal and GI Bill), reproduced rather than dismantled the prior caste structure because neutral rules operating on unequal starting positions preserve the inequality; the remedial reading holds equal protection requires affirmative correction of that structural legacy, not mere prospective neutrality.
% FOUNDING_PROBLEM_CORROBORATION: Sociological and economic research on the racial wealth gap, educational segregation persistence, and intergenerational mobility (much of it from academic economists and sociologists outside the civil rights litigation apparatus) corroborates that formal legal equality since 1964-65 has not closed measured gaps traceable to the historical subordination regime. Opposing corroboration comes from scholars and jurists (including some outside historically privileged groups) who argue the specific causal link between any given individual's current disadvantage and the historical regime has weakened to the point that race-conscious remedy misidentifies the mechanism; no source fully outside all interested parties has resolved which diagnosis is correct.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction climbs from near-zero at Brown (0.15, when the reading operated mainly as desegregation mandate against a state-run caste system with a clear historical wrongdoer) to moderate-high by the present (0.52), as remedial programs increasingly operate as ongoing group-level reallocation mechanisms rather than time-limited corrections tied to specific, identifiable state wrongs. Suppression tracks the same arc: early desegregation orders met open, organized resistance requiring federal enforcement (troops, court orders); today's suppression is legal-doctrinal — narrow tailoring requirements, strict scrutiny, and shrinking judicial tolerance function as an active constraint on how far remedial programs can go, while advocacy organizations actively defend the doctrine's remaining scope. Theater ratio rises modestly as some 'remedial' programs persist with declining connection to any documented specific discriminatory act, functioning more as a general diversity or legitimacy signal than a targeted remedy — this is the drift the diversity_reading formally captured and split off as its own constraint after Bakke.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated racial groups and the agencies administering remedial programs sit toward the beneficiary end: the constraint's operation transfers scarce goods (seats, contracts, jobs) toward them and legitimizes the administering institutions' mission. Displaced nonpreferred applicants and, at the group level, historically privileged racial groups sit toward the target end: they bear a concrete, individualized cost tied to a group-level historical claim. This is exactly the inversion the source material flags — under the sibling colorblind_reading, this same beneficiary/victim assignment reverses, because that reading treats the racial classification itself (regardless of remedial purpose) as the harm, making the administering agency and the preferred applicant the ones imposing a constitutional injury on the displaced applicant. The two readings cannot be merged into one ε without violating the ε-invariance principle — hence two separate constraint files.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state-enforced caste subordination via slavery, Black Codes, and Jim Crow) is substantially dead in its original form but its structural legacies (wealth gaps, segregation persistence, unequal starting positions) are empirically documented as live, which is why founding_problem_status is authored as contested rather than flatly dead. Where a remedial program has drifted to operate as a permanent reallocation mechanism disconnected from any specific identifiable discriminatory act, the mismatch between founding_problem_status and disappearance_verdict (world_rearranges) is exactly the zombie-mandate signature the R5 interview is designed to surface — and is why courts increasingly demand a fresh, specific factual predicate before tolerating the remedy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caste_persistence_vs_individual_desert,
    'Is the relevant unit of moral analysis the historically subordinated GROUP (whose durable structural disadvantage justifies group-based remedy) or the INDIVIDUAL applicant (who may bear no personal connection to, or personal benefit from, the historical wrong)?',
    'No empirical resolution exists; this is a foundational normative disagreement about the unit of constitutional analysis that the remedial and colorblind readings resolve oppositely by design. Could be narrowed but not resolved by longitudinal data on intergenerational transmission of specific harms.',
    'If the group is the correct unit, remedial reading''s beneficiary/victim structure is coherent as designed. If the individual is the correct unit, the same structure looks like the constraint is manufacturing new individual-level injuries to correct a group-level historical wrong, supporting the colorblind reading''s foreclosure of race-conscious remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_persistence_vs_individual_desert, conceptual, 'Whether group or individual is the correct unit for equal protection analysis under the remedial reading.').

omega_variable(
    remedial_scope_creep,
    'How many currently operating ''remedial'' race-conscious programs retain a genuine, specific evidentiary link to documented past discrimination by the administering entity, versus having drifted into general diversity or legitimacy rationales (the diversity_reading''s territory)?',
    'Program-by-program review of the factual predicates courts require under strict scrutiny (Croson-line narrow tailoring analysis); a systematic audit of surviving programs post-SFFA v. Harvard would show what share still meets the remedial reading''s own internal standard.',
    'A high share of programs lacking a specific evidentiary predicate would indicate the remedial reading is increasingly cover for what is structurally the diversity_reading (or worse, pure preference), inflating this story''s ε artificially; a low share would indicate the reading is being applied with fidelity to its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_scope_creep, empirical, 'Whether currently operating remedial programs retain genuine evidentiary links to specific historical discrimination.').

omega_variable(
    who_decides_subordination_is_dismantled,
    'Who has the authority to declare that the caste-like subordination a given remedial program targets has been sufficiently dismantled that the remedy''s constitutional justification has lapsed?',
    'This is currently contested between courts (via strict scrutiny sunset requirements), legislatures (via statutory sunset provisions, rare), and administering agencies (who have structural incentive to declare the problem ongoing). No neutral arbiter exists outside the judiciary, which is itself split on the underlying kernel reading.',
    'If courts are the sole arbiter and increasingly apply the colorblind reading''s skepticism, the remedial reading''s practical operating space will continue to shrink regardless of the underlying sociological facts about whether subordination persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_decides_subordination_is_dismantled, preference, 'Institutional authority to determine when a remedial program''s justification has lapsed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__remedial_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(equa_tr_t1965, equal_protection_commitment__remedial_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__remedial_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_commitment__remedial_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__remedial_reading, theater_ratio, 2003, 0.24).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__remedial_reading, theater_ratio, 2013, 0.26).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_commitment__remedial_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__remedial_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(equa_be_t1965, equal_protection_commitment__remedial_reading, base_extractiveness, 1965, 0.22).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__remedial_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement(equa_be_t1995, equal_protection_commitment__remedial_reading, base_extractiveness, 1995, 0.44).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__remedial_reading, base_extractiveness, 2003, 0.46).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__remedial_reading, base_extractiveness, 2013, 0.48).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__remedial_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__remedial_reading, suppression_requirement, 1954, 0.1).
narrative_ontology:measurement(equa_su_t1965, equal_protection_commitment__remedial_reading, suppression_requirement, 1965, 0.18).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__remedial_reading, suppression_requirement, 1978, 0.32).
narrative_ontology:measurement(equa_su_t1995, equal_protection_commitment__remedial_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__remedial_reading, suppression_requirement, 2003, 0.44).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__remedial_reading, suppression_requirement, 2013, 0.46).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__remedial_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'equal protection clause / affirmative action doctrine' per the ε-invariance principle. colorblind_reading treats any state racial classification as constitutionally suspect regardless of purpose (near-zero ε from its own internal premises, mountain-adjacent). diversity_reading treats race as one admissible factor toward educational diversity as a compelling interest distinct from remedying specific past discrimination (moderate ε, largely foreclosed for admissions post-SFFA v. Harvard 2023 but still live for other diversity rationales). This remedial_reading treats equal protection as an anti-subordination mandate permitting race-conscious correction of documented caste-like structural disadvantage (highest ε of the three, 0.45-0.60, because it affirmatively authorizes ongoing group-based reallocation). The three are linked bidirectionally in commentary; only this file's outbound edges are declared here per authoring convention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
