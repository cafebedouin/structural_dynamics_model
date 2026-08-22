% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Equal Protection Colorblind Reading — Formal Prohibition on Racial Classification
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint story captures the colorblind reading of the Equal
 *   Protection Clause as a distinct constitutional constraint. The reading
 *   holds that the Fourteenth Amendment forbids all governmental use of
 *   racial classifications, treating each person as an individual
 *   rights-bearer whose equal protection claim does not depend on group
 *   membership. The constraint is claimed as a mountain — a formal rule
 *   derived from the constitutional text and structure that would persist
 *   regardless of enforcement because it reflects a constitutive commitment
 *   of the constitutional order. The metrics reflect the reading's own
 *   assessment: very low extractiveness (the rule constrains the state, not
 *   individuals), moderate suppression (the state is prevented from acting,
 *   but alternatives are not suppressed for citizens), low theater (the rule
 *   is genuinely enforced by courts), high accessibility collapse (once the
 *   principle is accepted, race-conscious alternatives are conceptually
 *   foreclosed), and low resistance (the constraint is widely accepted as the
 *   correct reading by its proponents). The historical measurements show the
 *   Plessy era as a period of high extractiveness and theater — the
 *   constraint was formally honored but substantively inverted — and the
 *   post-Brown era as a return to low extractiveness from this reading's
 *   perspective.
 *
 * KEY AGENTS:
 *   - individual_rights_bearers: Primary beneficiary (moderate/mobile) — receives formal equality guarantee
 *   - all_citizens_as_formal_equals: Secondary beneficiary (organized/arbitrage) — benefits from constitutional order without racial classification
 *   - governmental_actors: Agenda setter (institutional/constrained) — bound by absolute prohibition on racial classification
 *   - historically_subordinated_groups: Excluded (organized/identity_locked) — claims for race-conscious remediation foreclosed
 *   - educational_institutions: Payer (organized/constrained) — loses race-conscious policy tools
 *   - originalist_judges: Observer (institutional/analytical) — enforces and interprets the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__colorblind_reading, 0.12).
domain_priors:suppression_score(equal_protection_clause__colorblind_reading, 0.35).
domain_priors:theater_ratio(equal_protection_clause__colorblind_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_clause__colorblind_reading, "Equal Protection Colorblind Reading — Formal Prohibition on Racial Classification").
narrative_ontology:topic_domain(equal_protection_clause__colorblind_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__colorblind_reading, 'dbcd2298-979c-494a-a916-8266827e35a9').
narrative_ontology:cs_kernel_codification('dbcd2298-979c-494a-a916-8266827e35a9', formalized).
narrative_ontology:cs_authority_grounding('dbcd2298-979c-494a-a916-8266827e35a9', lineage).
narrative_ontology:cs_interpretation_layer_present('dbcd2298-979c-494a-a916-8266827e35a9').
narrative_ontology:cs_reading_relation('dbcd2298-979c-494a-a916-8266827e35a9', equal_protection_clause__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('dbcd2298-979c-494a-a916-8266827e35a9', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('dbcd2298-979c-494a-a916-8266827e35a9', foundational, race_never_constitutionally_relevant).
narrative_ontology:cs_axiom_status(race_never_constitutionally_relevant, holdable).
narrative_ontology:cs_axiom_grounding('dbcd2298-979c-494a-a916-8266827e35a9', race_never_constitutionally_relevant, deontological).
narrative_ontology:cs_axiom('dbcd2298-979c-494a-a916-8266827e35a9', foundational, equal_protection_as_individual_right_only).
narrative_ontology:cs_axiom_status(equal_protection_as_individual_right_only, holdable).
narrative_ontology:cs_axiom_grounding('dbcd2298-979c-494a-a916-8266827e35a9', equal_protection_as_individual_right_only, deontological).
narrative_ontology:cs_reference_frame('dbcd2298-979c-494a-a916-8266827e35a9', reconstruction_anti_caste_principle).
narrative_ontology:cs_drift_state('dbcd2298-979c-494a-a916-8266827e35a9', contemporary_colorblind_doctrine, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dbcd2298-979c-494a-a916-8266827e35a9', '2026-08-04T14:32:17Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__colorblind_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, individual_rights_bearers).
narrative_ontology:constraint_beneficiary(equal_protection_clause__colorblind_reading, all_citizens_as_formal_equals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__colorblind_reading, educational_institutions).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, colorblind_constitutionalism).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__colorblind_reading, anti_classification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every person regardless of race receives the same formal protection against governmental racial classification. No individual can be treated differently by the state on the basis of race. Exit means invoking judicial review when the state classifies by race; the courts are accessible and the rule is clear.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, individual_rights_bearers, beneficiary,
    moderate, biographical, mobile, national).

% The citizenry as a whole benefits from a constitutional order in which race is never a legitimate basis for state action. The constraint stabilizes the public meaning of equal citizenship. Exit is not a meaningful concept — this is the constitutive framework of their political membership.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, all_citizens_as_formal_equals, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__colorblind_reading, all_citizens_as_formal_equals, observer).

% Legislatures, executives, and administrative agencies are bound by the absolute prohibition on racial classification. They cannot design race-conscious policies for any purpose — remedial, diversity, or otherwise. Their exit is constitutional amendment or judicial appointment strategy, both institutionally constrained.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, governmental_actors, agenda_setter,
    institutional, biographical, constrained, national).

% Groups that have suffered historical racial subordination find their claims for race-conscious remediation foreclosed by this reading. They would argue that formal neutrality perpetuates substantive inequality, but the constraint treats their group-based injury as irrelevant to equal protection analysis. Exit is identity-locked: their political identity is constituted through the very historical injustice the reading refuses to recognize as a basis for remedy.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, historically_subordinated_groups, excluded,
    organized, generational, identity_locked, national).

% Universities and schools cannot use race as a factor in admissions or program design, even for diversity goals they judge educationally essential. They bear the cost of foregone policy tools and must rely on race-neutral alternatives that may not achieve their aims. Exit is constrained by the judicial supremacy of the constitutional rule.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, educational_institutions, payer,
    organized, biographical, constrained, national).

% Judicial actors who endorse this reading as the correct originalist interpretation of the Fourteenth Amendment. They observe the constraint's operation from the bench and enforce it through strict scrutiny that is fatal in fact. Their role is analytical — they do not bear the constraint's costs or collect its benefits directly.
narrative_ontology:constraint_stakeholder(equal_protection_clause__colorblind_reading, originalist_judges, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable rule that eliminates racial classification from governmental decision-making entirely, coordinating expectations of citizens and officials around a single principle: the state does not sort by race.
% TRANSFER_FUNCTION: Transfers policy discretion over race-conscious measures from governmental actors to the constitutional rule itself — the state loses the tool, individuals gain the guarantee. No material transfer between social groups; the transfer is from state power to individual right.
% ABSENT_VOICES: Historically subordinated racial groups who would argue that formal colorblindness locks in the effects of past discrimination and that substantive equality requires race-conscious remediation. They are excluded because the reading defines equal protection as an individual right against classification, not a group right to remediation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, governmental actors could immediately adopt race-conscious policies for diversity, remediation, or other purposes. The entire architecture of strict scrutiny for racial classifications would collapse. Universities, employers, and legislatures would redesign policies around race-conscious tools. The political and legal landscape would reorganize substantially.
% FOUNDING_PROBLEM: The post-Civil War constitutional settlement needed to establish that the state could not classify citizens by race — the Black Codes and the legacy of slavery demonstrated that racial classification was the primary instrument of subordination. The colorblind reading was built to solve the problem of state-sponsored racial caste by making race legally invisible to government.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and colorblind constitutionalists attest the founding problem remains live — race consciousness in any form risks reviving the classification logic of subordination. Critics (including historians of Reconstruction, critical race theorists, and the remedial/diversity reading proponents) attest the founding problem was specifically the subordination of Black Americans, not racial classification per se, and that the colorblind reading was a later doctrinal construction that displaced the Amendment's remedial purpose. The historical record of the Fourteenth Amendment's framing and ratification is cited by both sides.
narrative_ontology:disappearance_verdict(equal_protection_clause__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(equal_protection_clause__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__colorblind_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__colorblind_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_clause__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_clause__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_clause__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_clause__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is mountain because the colorblind reading presents itself as a formal constitutional principle that emerges from the text and structure of the Fourteenth Amendment — a rule that would be true of the constitutional order regardless of who enforces it. The metrics are authored from the reading's own lights: extractiveness is near-zero because the constraint extracts nothing from individuals (it only binds the state); suppression is modest because the constraint operates on governmental actors, not citizens; accessibility collapse is high because accepting the principle conceptually forecloses race-conscious alternatives; resistance is low from the reading's perspective because the constraint is seen as the Constitution's own command. The 1896 Plessy spike in extractiveness and theater reflects the period when the constraint was formally invoked (separate but equal) but operated as its opposite — a theatrical performance of colorblindness masking a racial caste system. From this reading's perspective, that was a departure from the true constraint, not the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (individual_rights_bearers, all_citizens_as_formal_equals) experience this as a mountain — a stable, non-extractive constitutional guarantee. The excluded seat (historically_subordinated_groups) experiences it as a snare — a constraint that extracts their capacity for remediation while presenting itself as neutral. The payer seat (educational_institutions) experiences it as a rope with costs — they lose a policy tool but gain administrable clarity. The engine computes these per-seat classifications from the structural data; this commentary documents the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are all individuals as formal equals — the constraint's protection is universal and race-neutral by design. Governmental actors are the constrained party (agenda_setters who bear the restriction). Historically subordinated groups are excluded — their structural position is that the constraint denies them a remedial tool, but they are not 'victims' in the extraction sense because the constraint does not take from them; it withholds a tool the remedial reading would give them. Educational institutions are payers — they bear compliance costs and foregone policy discretion. The directionality derivation from these declarations yields low d for beneficiaries (subsidy), high d for governmental actors and institutions (constraint binds them), and an ambiguous d for excluded groups (identity_locked exit modulates toward target).
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading claims the founding problem (state-sponsored racial caste) remains live — race consciousness in any form risks reactivating the classification logic. Critics claim the founding problem was solved or transformed, and the reading now serves to block remediation. The mandatrophy question is whether the constraint's absolute form still serves its original anti-caste function or has become a barrier to addressing the caste's persistent effects. This reading resolves it by declaring the problem live; the sibling readings resolve it differently. The engine's mandatrophy detection will turn on the mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Does the equal_protection_clause kernel contain a single determinate meaning that the colorblind reading correctly identifies, or is the kernel irreducibly ambiguous such that multiple readings are structurally inevitable?',
    'Historical analysis of the Fourteenth Amendment''s framing and ratification record, combined with doctrinal analysis of whether the text ''equal protection of the laws'' can bear only the anti-classification meaning. The disagreement is located in the semantic content of the kernel itself — not in its application.',
    'If the kernel is determinate and colorblind, the sibling readings are errors. If the kernel is ambiguous, the sibling readings are alternative instantiations of the same kernel, and the colorblind reading''s claim to be the sole legitimate instantiation is a power move, not a textual necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the contested kernel admits of a single correct reading or structurally generates multiple readings.').

omega_variable(
    historical_subordination_vs_formal_neutrality,
    'Does the colorblind reading''s refusal to recognize group-based historical injury as constitutionally relevant constitute a structural exclusion that functions as extraction, or is it a necessary consequence of the individual-rights framework?',
    'Empirical study of whether race-neutral alternatives can achieve substantive equality in domains where historical subordination created durable group-level disparities. If race-neutral tools systematically fail to close gaps, the constraint''s formal neutrality operates as substantive extraction from the subordinated group.',
    'If race-neutral tools cannot achieve substantive equality, the colorblind reading''s mountain claim is falsified — the constraint extracts the possibility of remediation from historically subordinated groups while presenting itself as neutral. This would support FSM reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_subordination_vs_formal_neutrality, empirical, 'Whether formal neutrality perpetuates substantive inequality in a way that constitutes extraction.').

omega_variable(
    strict_scrutiny_as_coordination_or_exclusion,
    'Is the strict scrutiny framework (fatal in fact) a genuine coordination mechanism that makes the colorblind rule administrable, or is it an exclusion mechanism that forecloses the remedial and diversity readings by design?',
    'Doctrinal analysis of whether strict scrutiny has ever been satisfied in racial classification cases, and whether the standard''s structure is calibrated to permit any race-conscious policy that meets its stated criteria (compelling interest, narrow tailoring).',
    'If strict scrutiny is structurally impossible to satisfy, the constraint''s enforcement mechanism is an exclusion machine, not a coordination tool. This would raise suppression and theater metrics and challenge the mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_scrutiny_as_coordination_or_exclusion, conceptual, 'Whether the constraint''s enforcement doctrine coordinates or excludes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__colorblind_reading, 1868, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eppcr_tr_t1868, equal_protection_clause__colorblind_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(eppcr_tr_t1896, equal_protection_clause__colorblind_reading, theater_ratio, 1896, 0.78).
narrative_ontology:measurement(eppcr_tr_t1954, equal_protection_clause__colorblind_reading, theater_ratio, 1954, 0.22).
narrative_ontology:measurement(eppcr_tr_t1978, equal_protection_clause__colorblind_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(eppcr_tr_t2003, equal_protection_clause__colorblind_reading, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(eppcr_tr_t2023, equal_protection_clause__colorblind_reading, theater_ratio, 2023, 0.18).

% Extraction over time
narrative_ontology:measurement(eppcr_be_t1868, equal_protection_clause__colorblind_reading, base_extractiveness, 1868, 0.08).
narrative_ontology:measurement(eppcr_be_t1896, equal_protection_clause__colorblind_reading, base_extractiveness, 1896, 0.65).
narrative_ontology:measurement(eppcr_be_t1954, equal_protection_clause__colorblind_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(eppcr_be_t1978, equal_protection_clause__colorblind_reading, base_extractiveness, 1978, 0.12).
narrative_ontology:measurement(eppcr_be_t2003, equal_protection_clause__colorblind_reading, base_extractiveness, 2003, 0.11).
narrative_ontology:measurement(eppcr_be_t2023, equal_protection_clause__colorblind_reading, base_extractiveness, 2023, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(eppcr_su_t1868, equal_protection_clause__colorblind_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(eppcr_su_t1896, equal_protection_clause__colorblind_reading, suppression_requirement, 1896, 0.85).
narrative_ontology:measurement(eppcr_su_t1954, equal_protection_clause__colorblind_reading, suppression_requirement, 1954, 0.4).
narrative_ontology:measurement(eppcr_su_t1978, equal_protection_clause__colorblind_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(eppcr_su_t2003, equal_protection_clause__colorblind_reading, suppression_requirement, 2003, 0.3).
narrative_ontology:measurement(eppcr_su_t2023, equal_protection_clause__colorblind_reading, suppression_requirement, 2023, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__colorblind_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, equal_protection_clause__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, strict_scrutiny_doctrine).
narrative_ontology:affects_constraint(equal_protection_clause__colorblind_reading, affirmative_action_jurisprudence).

% DUAL FORMULATION NOTE:
% This constraint is one member of the equal_protection_clause constraint family (kernel_id: equal_protection_clause). The three readings — colorblind_reading, remedial_reading, diversity_reading — instantiate three distinct constraints from the same kernel. Their ε values differ substantially: colorblind_reading ε ≈ 0.12 (formal rule binding the state), remedial_reading ε ≈ 0.45 (requires state action that extracts from non-beneficiaries), diversity_reading ε ≈ 0.35 (permits state action with diffuse benefits and concentrated costs). The colorblind reading forecloses the remedial reading logically; it coexists with and influences the diversity reading through doctrinal structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_clause__colorblind_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
