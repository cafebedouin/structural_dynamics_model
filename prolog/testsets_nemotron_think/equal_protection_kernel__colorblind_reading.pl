% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: equal_protection_kernel__colorblind_reading
 *   human_readable: Colorblind Equal Protection Clause — Categorical Ban on Racial Classifications
 *   domain: constitutional_law/civil_rights/education_policy
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause asserts that the
 *   Constitution categorically forbids the state from using racial
 *   classifications for any purpose — benign or invidious, remedial or
 *   oppressive. This reading consolidates in the late 20th century (Bakke,
 *   Croson, Adarand, Parents Involved, SFFA) as the controlling Supreme Court
 *   doctrine, displacing the remedial reading (which permitted narrowly
 *   tailored race-conscious remedies) and marginalizing the antisubordination
 *   reading (which permits race-conscious action to dismantle hierarchy). The
 *   constraint operates through judicial enforcement: any state actor that
 *   classifies by race triggers strict scrutiny, which is 'fatal in fact.'
 *   The reading claims Mountain status — a fixed constitutional principle
 *   derived from the text and original understanding — but declares
 *   identifiable beneficiaries (advocates, institutions avoiding
 *   classification, white applicants) and victims (historically excluded
 *   groups, remedial policy beneficiaries), triggering False Summit Mountain
 *   evaluation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, 0.78).
domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, 0.85).
domain_priors:theater_ratio(equal_protection_kernel__colorblind_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_kernel__colorblind_reading, "Colorblind Equal Protection Clause — Categorical Ban on Racial Classifications").
narrative_ontology:topic_domain(equal_protection_kernel__colorblind_reading, "constitutional_law/civil_rights/education_policy").

domain_priors:requires_active_enforcement(equal_protection_kernel__colorblind_reading).
domain_priors:emerges_naturally(equal_protection_kernel__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__colorblind_reading, '3c84756b-9c1d-4a24-9b8f-c60e4b610b3d').
narrative_ontology:cs_kernel_codification('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', fixed_text).
narrative_ontology:cs_authority_grounding('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', lineage).
narrative_ontology:cs_interpretation_layer_present('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d').
narrative_ontology:cs_reading_relation('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', equal_protection_kernel__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', equal_protection_kernel__antisubordination_reading, forecloses).
narrative_ontology:cs_axiom('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', foundational, racial_classifications_per_se_unconstitutional).
narrative_ontology:cs_axiom_status(racial_classifications_per_se_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', racial_classifications_per_se_unconstitutional, deontological).
narrative_ontology:cs_axiom('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', foundational, no_state_obligation_remedy_past_discrimination).
narrative_ontology:cs_axiom_status(no_state_obligation_remedy_past_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', no_state_obligation_remedy_past_discrimination, deontological).
narrative_ontology:cs_axiom('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', secondary, strict_scrutiny_fatal_in_fact).
narrative_ontology:cs_axiom_status(strict_scrutiny_fatal_in_fact, holdable).
narrative_ontology:cs_axiom_grounding('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', strict_scrutiny_fatal_in_fact, conventional).
narrative_ontology:cs_reference_frame('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', colorblind_constitutional_originalism).
narrative_ontology:cs_drift_state('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', post_grutter_sffa_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c84756b-9c1d-4a24-9b8f-c60e4b610b3d', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(equal_protection_kernel__colorblind_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, colorblind_constitutional_advocates).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, formal_equality_proponents).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, institutions_avoiding_racial_classification).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__colorblind_reading, white_applicants_in_competitive_admissions).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, beneficiaries_of_remedial_admissions_policies).
narrative_ontology:constraint_victim(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity_through_race_conscious_means).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, colorblind_constitution_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, formal_equality_principle).
narrative_ontology:constraint_vindicates(equal_protection_kernel__colorblind_reading, anti_classification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and litigate to enforce a categorical ban on all state racial classifications. Their professional identity, organizational mission, and fundraising depend on the colorblind reading prevailing. Exit would mean abandoning the core constitutional commitment that defines their movement.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, colorblind_constitutional_advocates, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__colorblind_reading, colorblind_constitutional_advocates, beneficiary).

% Scholars, lawyers, and citizens who believe formal colorblindness is the only legitimate constitutional principle. They benefit from a predictable, classification-free legal regime but face social and professional costs for advocating this position in many institutional settings.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, formal_equality_proponents, beneficiary,
    moderate, biographical, constrained, national).

% Universities, employers, and government agencies that prefer not to engage in racial classification due to legal risk, administrative burden, or ideological commitment. They benefit from a clear rule that relieves them of the obligation to design and defend race-conscious policies.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_avoiding_racial_classification, beneficiary,
    institutional, generational, arbitrage, national).

% Applicants to selective universities and programs who face no racial preferences under a colorblind regime. They benefit directly from the elimination of race-conscious admissions but their position is contingent on the constraint's enforcement — if the constraint vanished, they would face renewed competition from race-conscious policies.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, white_applicants_in_competitive_admissions, beneficiary,
    moderate, immediate, constrained, national).

% Black, Latino, Native American, and other groups historically excluded by state action who lose the constitutional basis for remedial policies. Their identity and political mobilization are fused to the struggle against subordination; exit from this struggle is experienced as betrayal of collective memory and ongoing injury.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, historically_excluded_racial_groups, payer,
    organized, generational, identity_locked, national).

% Individual students and applicants who would gain admission through race-conscious holistic review but are categorically barred by the colorblind rule. They have no alternative pathway to the same institutions — the constraint removes the only mechanism that made their admission probable.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, beneficiaries_of_remedial_admissions_policies, payer,
    moderate, biographical, trapped, national).

% Universities and employers that believe racial diversity is essential to their educational or operational mission and seek to use race-conscious tools to achieve it. They bear compliance costs, litigation risk, and the loss of their preferred policy instruments. Exit means abandoning their diversity rationale or relocating to jurisdictions with different constitutional regimes.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, institutions_seeking_diversity_through_race_conscious_means, payer,
    institutional, generational, constrained, national).

% The judicial branch that interprets and applies the colorblind reading, striking down race-conscious policies. They administer the constraint's enforcement and their legitimacy in this reading depends on appearing to apply a neutral principle rather than a political preference.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, federal_courts_enforcing_colorblindness, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for the antisubordination reading who argue the Equal Protection Clause targets caste-like hierarchy, not classification per se. They are structurally excluded from the colorblind framework — their core premise (that some racial classifications are constitutionally required) is ruled out of bounds by the constraint's categorical logic.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, civil_rights_advocates_antisubordination, excluded,
    organized, generational, identity_locked, national).

% Advocates for the remedial reading who argue narrowly tailored race-conscious remedies for documented discrimination are permitted. They are excluded because the colorblind reading treats their position as constitutionally impermissible on its face — no balancing test, no narrow tailoring, no exception.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, remedial_policy_defenders, excluded,
    organized, generational, identity_locked, national).

% Scholars who analyze the competing readings without advocating for any single one. They observe the structural dynamics, historical evolution, and doctrinal coherence of each reading from outside the contest.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__colorblind_reading, constitutional_scholars_analytical, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, bright-line rule for state action: no racial classifications, ever. Eliminates the need for courts to adjudicate which classifications are 'benign' or 'compelling,' which interests are legitimate, and how narrowly tailored a policy must be. Creates predictability and prevents the state from sorting citizens by race.
% TRANSFER_FUNCTION: Moves the constitutional authority to use race-conscious remedial tools from historically excluded groups and the institutions that serve them, to the advocates and courts enforcing formal colorblindness. The transfer is the remedial pathway itself — the ability to address past and present discrimination through race-conscious means.
% ABSENT_VOICES: The communities most directly subjected to the historical subordination the Fourteenth Amendment was enacted to address — freedmen and their descendants, Indigenous nations, colonized peoples — are not the authors of the colorblind reading. Their constitutional vision (antidiscrimination as anti-subordination) was displaced by a formal equality frame constructed decades after ratification. They are absent from the room where the colorblind reading was consolidated in doctrine.
% DISAPPEARANCE_RATIONALE: If the categorical ban vanished overnight, universities would immediately reinstate race-conscious holistic review, employers would revive affirmative action programs, voting rights enforcement would return to disparate impact standards, and the entire architecture of colorblind constitutional doctrine would collapse. The world would rearrange around the remedial and antisubordination readings that the constraint currently forecloses.
% FOUNDING_PROBLEM: Post-Civil War Reconstruction required a constitutional guarantee that states would not re-enslave or subjugate freedmen through racially discriminatory laws. The colorblind reading traces its founding to the Reconstruction Congress's rejection of racial caste and the principle that the law must not 'know' race.
% FOUNDING_PROBLEM_CORROBORATION: The colorblind reading's founding narrative is corroborated by originalist scholars (e.g., Raoul Berger, Michael McConnell) who argue the Fourteenth Amendment was understood as colorblind. However, historians of Reconstruction (e.g., Eric Foner, Michael Klarman) and critical race theorists (e.g., Derrick Bell, Cheryl Harris) corroborate the competing antisubordination founding — that the Amendment was understood to authorize race-conscious remedies for the specific subordination of freedmen. No neutral arbiter outside the constitutional debate corroborates one founding over the other.
narrative_ontology:disappearance_verdict(equal_protection_kernel__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__colorblind_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_kernel__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_kernel__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_kernel__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_kernel__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint removes the only constitutional mechanism for race-conscious remediation of historical subordination, transferring that remedial capacity to the enforcers of formal colorblindness. Suppression is very high (0.85) because the constraint's persistence depends entirely on active judicial enforcement — courts must continuously strike down democratically enacted race-conscious policies. Theater ratio is low (0.15) because the enforcement is genuine and consequential, not performative. Accessibility collapse is very high (0.92) because the categorical rule leaves no room for alternatives — no balancing, no narrow tailoring, no 'compelling interest' escape hatch. Resistance is high (0.72) because the constraint faces sustained opposition from civil rights organizations, affected communities, dissenting justices, and academic critics who argue it betrays the Amendment's antisubordination purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (colorblind advocates, courts), the constraint appears as a Mountain — a fixed, neutral principle that prevents the state from sorting citizens by race. From the payer seats (historically excluded groups, remedial beneficiaries), the same constraint operates as a Snare — the coordination story (formal equality) is cover for the extraction of remedial pathways that the Fourteenth Amendment was enacted to secure. The engine computes this divergence from the structural data: the categorical ban's beneficiaries are concentrated and organized; its victims are identity-locked and lack exit. The claimed_type (mountain) and the metrics (high extraction, high suppression) diverge deliberately — the engine measures that divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Colorblind advocates and formal equality proponents are structural beneficiaries (d near 0.0) — they collect the constitutional authority to block race-conscious policies. Institutions avoiding classification are beneficiaries with arbitrage-grade exit (they can comply easily and prefer the clarity). White applicants in competitive admissions are immediate beneficiaries with constrained exit (they benefit from the rule but cannot control it). Historically excluded groups are identity-locked payers (d near 1.0) — their constitutional vision is foreclosed and their remedial tools removed, with exit experienced as betrayal. Remedial policy beneficiaries are trapped payers — they have no alternative pathway. Institutions seeking diversity are constrained payers — they bear compliance costs and lose policy instruments but retain institutional agency. Courts are agenda-setters with analytical exit (they administer the constraint but could reinterpret). Civil rights advocates and remedial defenders are excluded — their premises are ruled out of bounds.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading was built to solve the founding problem of racial caste (Reconstruction's anti-subordination mandate). By the late 20th century, that founding problem was declared 'solved' by the enforcers of colorblindness — formal legal equality achieved, racial classifications now presumptively suspect. But the remedial and antisubordination readings contend the founding problem persists in evolved form (structural inequality, disparate impact, unconscious bias). The mandate (categorical colorblindness) has outlived its founding function (dismantling de jure caste) and now serves a different function (blocking race-conscious remediation of de facto inequality). This is mandatrophy: the constraint persists by redefining its purpose away from the problem it was built to solve. The founding_problem_status 'contested' and disappearance_verdict 'world_rearranges' capture this mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblind_reading_of_equal_protection_kernel,
    'Is the colorblind reading a genuine recovery of the Fourteenth Amendment''s original meaning, or a late-20th-century construction that displaces the Amendment''s antisubordination purpose?',
    'Historical analysis of Reconstruction Congress debates, ratification-era understanding, and early enforcement practice (Freedmen''s Bureau Acts, Civil Rights Act of 1875) versus the doctrinal trajectory from Plessy through Brown to Bakke/SFFA.',
    'If the colorblind reading is historically constructed rather than original, its Mountain claim (emerges_naturally) collapses — it becomes a Tangled Rope or Snare: a coordination mechanism (formal equality) that extracts remedial capacity from historically subordinated groups. If it is original, the Mountain claim holds but the FSM beneficiary structure still requires explanation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colorblind_reading_of_equal_protection_kernel, conceptual, 'Originalist vs. constructionist account of the colorblind reading''s legitimacy').

omega_variable(
    beneficiary_structure_of_formal_colorblindness,
    'Do the declared beneficiaries (colorblind advocates, institutions avoiding classification, white applicants) genuinely benefit from the constraint, or is the beneficiary structure itself contested — e.g., do white applicants actually benefit in absolute terms, or only relative to a race-conscious baseline?',
    'Empirical analysis of admissions outcomes under colorblind vs. race-conscious regimes (e.g., California Prop 209, Michigan Proposal 2, SFFA aftermath) measuring absolute admission rates, institutional diversity, and long-term socioeconomic mobility for all groups.',
    'If white applicants'' absolute admission rates do not improve under colorblindness (only relative positioning changes), the beneficiary declaration for that group is mis-specified — the constraint extracts from historically excluded groups without transferring gains to the declared beneficiaries, shifting classification toward pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_of_formal_colorblindness, empirical, 'Whether the constraint''s extraction transfers to declared beneficiaries or is purely destructive').

omega_variable(
    suppression_mechanism_judicial_vs_legislative,
    'Is the constraint''s high suppression (0.85) driven primarily by judicial enforcement (courts striking down policies) or by legislative/executive compliance (actors self-censoring race-conscious policies)?',
    'Track the proportion of race-conscious policies invalidated by courts versus those never proposed due to anticipated invalidation. Survey institutional legal counsel on chilling effects.',
    'If suppression is primarily judicial, the constraint''s enforcement is centralized and its persistence depends on judicial composition. If primarily legislative/executive self-censorship, the constraint has internalized suppression — actors carry the constraint with them (identity_locked dynamics), making it more resilient to judicial turnover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_vs_legislative, empirical, 'Source of the constraint''s suppressive force: external judicial enforcement vs. internalized compliance').

omega_variable(
    remedial_reading_survival_in_subnational_jurisdictions,
    'Does the remedial reading survive in state constitutions, statutes, or local policies that the colorblind federal reading does not reach, creating a federalism pressure valve?',
    'Survey state constitutional equal protection clauses, state affirmative action bans vs. permissions, and local diversity policies post-SFFA. Measure whether remedial pathways persist outside federal doctrine.',
    'If remedial pathways survive subnationally, the colorblind reading''s accessibility_collapse (0.92) is overstated — alternatives exist but are jurisdictionally bounded. This would reduce extraction for mobile institutions and applicants, and create a network.affects_constraints edge to subnational remedial constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_reading_survival_in_subnational_jurisdictions, empirical, 'Whether federal colorblind doctrine fully collapses remedial alternatives or leaves subnational escape hatches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__colorblind_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_tr_t1868, equal_protection_kernel__colorblind_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_tr_t1896, equal_protection_kernel__colorblind_reading, theater_ratio, 1896, 0.25).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_tr_t1954, equal_protection_kernel__colorblind_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_tr_t1978, equal_protection_kernel__colorblind_reading, theater_ratio, 1978, 0.12).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_tr_t2003, equal_protection_kernel__colorblind_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_tr_t2023, equal_protection_kernel__colorblind_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_be_t1868, equal_protection_kernel__colorblind_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_be_t1896, equal_protection_kernel__colorblind_reading, base_extractiveness, 1896, 0.35).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_be_t1954, equal_protection_kernel__colorblind_reading, base_extractiveness, 1954, 0.45).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_be_t1978, equal_protection_kernel__colorblind_reading, base_extractiveness, 1978, 0.62).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_be_t2003, equal_protection_kernel__colorblind_reading, base_extractiveness, 2003, 0.71).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_be_t2023, equal_protection_kernel__colorblind_reading, base_extractiveness, 2023, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_su_t1868, equal_protection_kernel__colorblind_reading, suppression_requirement, 1868, 0.2).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_su_t1896, equal_protection_kernel__colorblind_reading, suppression_requirement, 1896, 0.8).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_su_t1954, equal_protection_kernel__colorblind_reading, suppression_requirement, 1954, 0.7).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_su_t1978, equal_protection_kernel__colorblind_reading, suppression_requirement, 1978, 0.82).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_su_t2003, equal_protection_kernel__colorblind_reading, suppression_requirement, 2003, 0.84).
narrative_ontology:measurement(equal_protection_kernel__colorblind_reading_su_t2023, equal_protection_kernel__colorblind_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__colorblind_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, equal_protection_kernel__antisubordination_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, affirmative_action_ban_state_constitutional_amendments).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, voting_rights_act_section2_disparate_impact).
narrative_ontology:affects_constraint(equal_protection_kernel__colorblind_reading, disparate_impact_doctrine_employment).

% DUAL FORMULATION NOTE:
% This constraint is one member of the equal_protection_kernel constraint family. The kernel (Fourteenth Amendment Equal Protection Clause) decomposes into three structurally distinct constraints with different ε values and beneficiary/victim structures: colorblind_reading (this file, ε=0.78, claims Mountain, has beneficiaries and victims), remedial_reading (ε≈0.35, Tangled Rope — coordination via diversity rationale with asymmetric extraction via narrow tailoring burden), antisubordination_reading (ε≈0.25, Rope — coordinates anti-hierarchy enforcement with minimal extraction). The colorblind reading forecloses both siblings structurally; the remedial and antisubordination readings coexist_with each other (both permit some race-conscious action, differing only in the triggering condition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, organized, 0.1).
constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, moderate, 0.85).
constraint_indexing:directionality_override(equal_protection_kernel__colorblind_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
