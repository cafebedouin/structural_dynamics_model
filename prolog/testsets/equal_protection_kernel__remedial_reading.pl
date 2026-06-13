% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Race-Conscious State Action for Remedial/Diversity Purposes (Remedial Reading)
 *   domain: constitutional/civil_rights/education
 *
 * SUMMARY:
 *   The remedial reading of the Equal Protection Clause permits universities
 *   and states to consider race in admissions decisions when narrowly
 *   tailored to remedy documented historical exclusion or achieve compelling
 *   diversity interests. This reading instantiates ONE interpretation of a
 *   contested constitutional kernel. The colorblind reading rejects any
 *   race-conscious classification; the antisubordination reading permits
 *   broader remedial action targeting ongoing hierarchy. The remedial reading
 *   sits between: it permits race-consciousness but only for documented
 *   remedial purposes, requiring institutional actors (universities, courts,
 *   legislatures) to articulate and defend the basis for race-conscious
 *   action. This constraint operates as tangled_rope: there is genuine
 *   coordination benefit (addressing historical exclusion through
 *   institutional mechanisms), but also asymmetric extraction (rejected
 *   applicants outside the remedial class bear cost, universities gain
 *   legitimacy and meet diversity targets, historically excluded groups gain
 *   access). The tension between coordination and extraction is managed
 *   through the "narrow tailoring" and "compelling interest" doctrines, which
 *   attempt to cabin extraction to purposes judged remedial.
 *
 * KEY AGENTS:
 *   - universities_pursuing_diversity: institutional agenda-setter; administers admissions policy within the remedial framework; collects legitimacy for race-consciousness tied to remedial purpose
 *   - historically_excluded_racial_groups: primary beneficiary class; gains access to elite institutions at higher rates than race-blind admissions would produce
 *   - rejected_applicants_outside_remedial_class: primary victim class; denied admission when race is used as plus factor; would have been admitted under race-blind process
 *   - judicial_interpreters: secondary agenda-setter; interpret what counts as compelling diversity interest and how narrow the tailoring must be; define the boundary between permissible and impermissible race-consciousness
 *   - colorblind_reading_adherents: excluded seat; would categorically forbid racial classifications; present in courts and legislatures but excluded from university admissions policy
 *   - antisubordination_reading_adherents: excluded seat; would expand remedial justifications beyond documented historical exclusion to ongoing systemic hierarchy; present in critical theory but excluded from mainstream institutional policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.38).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.52).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Race-Conscious State Action for Remedial/Diversity Purposes (Remedial Reading)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional/civil_rights/education").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '7a911b20-45f7-4564-923e-e57aa196289e').
narrative_ontology:cs_kernel_codification('7a911b20-45f7-4564-923e-e57aa196289e', fixed_text).
narrative_ontology:cs_authority_grounding('7a911b20-45f7-4564-923e-e57aa196289e', lineage).
narrative_ontology:cs_interpretation_layer_present('7a911b20-45f7-4564-923e-e57aa196289e').
narrative_ontology:cs_reading_relation('7a911b20-45f7-4564-923e-e57aa196289e', equal_protection_kernel__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a911b20-45f7-4564-923e-e57aa196289e', equal_protection_kernel__antisubordination_reading, influences).
narrative_ontology:cs_axiom('7a911b20-45f7-4564-923e-e57aa196289e', foundational, remedial_race_consciousness_constitutionally_permissible).
narrative_ontology:cs_axiom_status(remedial_race_consciousness_constitutionally_permissible, holdable).
narrative_ontology:cs_axiom_grounding('7a911b20-45f7-4564-923e-e57aa196289e', remedial_race_consciousness_constitutionally_permissible, deontological).
narrative_ontology:cs_axiom('7a911b20-45f7-4564-923e-e57aa196289e', secondary, narrow_tailoring_doctrine_constrains_remedial_scope).
narrative_ontology:cs_axiom_status(narrow_tailoring_doctrine_constrains_remedial_scope, overridden).
narrative_ontology:cs_axiom_grounding('7a911b20-45f7-4564-923e-e57aa196289e', narrow_tailoring_doctrine_constrains_remedial_scope, instrumental).
narrative_ontology:cs_reference_frame('7a911b20-45f7-4564-923e-e57aa196289e', remedial_equal_protection_authority).
narrative_ontology:cs_drift_state('7a911b20-45f7-4564-923e-e57aa196289e', contemporary_post_2023_supreme_court, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7a911b20-45f7-4564-923e-e57aa196289e', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_pursuing_diversity).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants_outside_remedial_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, state_legislatures_and_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and implement the remedial reading of Equal Protection to permit race-conscious admissions. Develop and defend diversity rationales for race-conscious decision-making. Conduct and document evidence of historical exclusion or compelling diversity interests. Face litigation challenging their admissions policies from rejected applicants and colorblind advocates. By the 2023 Supreme Court decision, this institutional seat loses its primary legal justification for race-consciousness and must either cease race-conscious admissions or find alternative bases (e.g., socioeconomic diversity, first-generation status).
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_pursuing_diversity, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from universities' race-conscious admissions under the remedial reading. Gain access to elite institutions at higher rates than race-blind admissions would provide. Benefit from institutional commitment to diversity and the social and economic returns from elite education. Lose this benefit when the remedial reading is superseded by the colorblind reading (post-2023), returning to lower representation at elite institutions absent alternative remedial bases.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Denied admission to universities that practice race-conscious admissions under the remedial reading. Would have been admitted if admissions were race-blind. Litigate against the constraint (Students for Fair Admissions v. Harvard/UNC is their seat's victory). Bear the cost of attending alternative institutions, which may have lower prestige and reduced lifetime earnings. Increasingly win relief post-2023 as courts adopt the colorblind reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants_outside_remedial_class, payer,
    moderate, biographical, mobile, national).

% Legislatures and agencies document historical exclusion and articulate compelling state interests in diversity, which the remedial reading requires before race-conscious state action is permissible. Gain legitimacy for race-conscious policy when tied to documented remedial purposes. Bear political cost from colorblind and antisubordination advocates who dispute the remedial justification.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_legislatures_and_agencies, beneficiary,
    institutional, generational, constrained, national).

% Interpret the Equal Protection Clause to define what counts as compelling diversity interest and how narrow tailoring must be. Adjudicate disputes between universities and rejected applicants. The Supreme Court pivots from the remedial reading (dominant 1978–2016) to the colorblind reading (2023 decision), foreclosing the remedial reading's institutional use. Lower courts and state courts vary in their adherence to the 2023 pivot.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, federal_courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, federal_courts_and_judges, observer).

% Argue the Constitution forbids racial classifications regardless of remedial purpose. Excluded from universities' admissions policy deliberations but present in courts and legislatures. Fund and organize litigation against race-conscious admissions (Students for Fair Admissions case). Win a major victory in the 2023 Supreme Court decision, which adopts the colorblind reading and forecloses the remedial reading. Advance legislation to prohibit race-conscious state action.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, colorblind_reading_advocates, excluded,
    powerful, generational, constrained, national).

% Critique the remedial reading as insufficiently protective against subordination. Argue the Constitution should prohibit all state action that entrenches racial hierarchy, regardless of intent, and should permit all action that dismantles hierarchy. Present in critical race theory discourse and some institutional policy proposals but excluded from mainstream admissions policy. The antisubordination reading remains marginalized even after the 2023 colorblind victory, as neither colorblind nor remedial readings adopt an antisubordination framework.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, antisubordination_theorists_and_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__remedial_reading, universities_pursuing_diversity).
narrative_ontology:fixing_cost_class(equal_protection_kernel__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the institutional coordination problem of how to remedy historical exclusion of racial minorities from elite educational institutions through educational access. Individual universities cannot remedy system-wide exclusion through independent action; coordinated state action with documented remedial purpose creates a mechanism for increasing representation while maintaining institutional legitimacy and educational quality. The remedial reading enables this coordination by permitting race-consciousness when tied to remedial purposes, solving both the collective-action problem (how to scale remedy) and the legitimacy problem (how to justify race-consciousness under colorblind constitutional doctrine).
% TRANSFER_FUNCTION: Transfers admission slots from non-beneficiary applicants (those outside the historically excluded racial groups) to beneficiary applicants (from historically excluded groups), in proportion to the documented remedial purpose or diversity interest. The transfer of slots is accompanied by transfer of the social and economic benefits of elite education (credential, network, earnings premium). The constraint justifies this transfer as remedying historical exclusion rather than as pure redistribution.
% ABSENT_VOICES: Colorblind reading adherents, who argue racial classifications are categorically unconstitutional, are systematically excluded from university admissions policy deliberations even when they hold institutional power (legislatures, courts). Antisubordination theorists, who would expand remedial justifications beyond documented historical exclusion to all action that dismantles systemic hierarchy, are excluded from mainstream institutional policy deliberations. Both absent voices are present in courts and legislatures but marginalized in university admissions policy-making.
% DISAPPEARANCE_RATIONALE: If the remedial reading and its enforcement vanished overnight, universities would shift to race-blind admissions, racial composition of student bodies would change substantially, the pipeline of credentials and networks flowing to historically excluded groups would narrow, litigation over remedial obligations would cease, and institutional commitment to diversity would shift to alternative bases (socioeconomic, geographic, etc.). State agencies would lose a constitutional justification for race-conscious policy. The 2023 Supreme Court decision effectively instantiates this disappearance for most institutions.
% FOUNDING_PROBLEM: Historical and systematic exclusion of racial minorities from elite educational institutions, producing durable disadvantages in educational attainment, professional networks, and economic outcomes. The exclusion was achieved through explicit racial discrimination (pre-1960s) and has persisted through facially neutral mechanisms (testing, legacy preferences, neighborhood segregation) that perpetuate the effects of prior discrimination. The remedial reading was built to permit state and institutional action to remedy this exclusion and to achieve diverse educational environments for their educational and social benefits.
% FOUNDING_PROBLEM_CORROBORATION: The 2023 Supreme Court decision (Students for Fair Admissions v. Harvard/UNC) attests the founding problem is no longer live and no longer justifies race-conscious admissions. The Court found that universities failed to show how race-consciousness was narrowly tailored to remedy documented exclusion, implying the exclusion is judged sufficiently remedied by race-blind alternatives. However, educational institutions, civil rights scholars, and some state legislatures continue to attest the founding problem is live: demographic data shows persistent underrepresentation of some racial groups at elite institutions, and research documents educational and economic disparities that correlate with race. The contest is no longer about whether historical exclusion occurred (all parties acknowledge it) but about whether its effects persist and whether the remedial reading remains justified. The Supreme Court's shift to the colorblind reading marks an institutional judgment that the founding problem is dead; civil rights advocates and critical scholars dispute this judgment.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38): the remedial reading permits race-conscious action, which transfers admission slots and benefits from non-beneficiary to beneficiary applicants. This transfer is not as high-extraction as pure rent-seeking because it is justified by documented remedial purpose and constrained by narrow tailoring doctrine. However, the gap between remedial justification and actual admission effects creates extractive slack—universities can use diversity interests that expand beyond strict historical redress, and the measuring of remedial purpose is contestable (high theater ratio at 0.41). Suppression is moderate-high (0.52): the reading's persistence depends on defending against colorblind and antisubordination challenges, on excluding those reading's advocates from admissions policy deliberations, and on maintaining doctrinal boundaries (compelling interest, narrow tailoring) that prevent the remedial reading from collapsing into pure redistribution or expanding into antisubordination territory. The measurement series traces the remedial reading's institutional adoption and legal validation: extractiveness rises from 1964 (early, tentative) to 2003 (peak institutional adoption), then holds flat as court challenges intensify. Theater ratio rises throughout (documentation burden becomes more elaborate and performative), suggesting increasing efforts to legitimize race-conscious action as remedial rather than mere redistribution. Suppression requirement rises through 2013 then falls slightly at 2024, reflecting the Supreme Court's 2023 pivot away from the remedial reading. All metrics share the 1964–2024 time grid.
 *
 * PERSPECTIVAL GAP:
 *   The universities-and-state seat should experience this constraint as genuine coordination it administers (solving a real collective-action problem: how to remedy exclusion without fragmented individual institutional action). The rejected-applicants seat experiences it as enforced extraction (denied access to an institution they would have attended under race-blind rules, bearing the cost of remedial reallocation). Judicial interpreters sit between: they coordinate on the meaning of permissible remedial purpose but also suppress alternative readings (colorblind, antisubordination) that would foreclose race-consciousness. The engine should compute divergent classifications across these seats from the structural data: the seat that administers and benefits should compute as rope or tangled-rope-cooperative; the target seat (rejected applicants) should compute as substantially more extractive; the judicial seat should compute as administrative-coordination with suppression costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: historically excluded racial groups (d near 0.0—the constraint subsidizes their institutional access); universities pursuing diversity (d near 0.1—they gain institutional legitimacy and achieve diversity targets while bearing modest legal defense costs). Victims: rejected applicants outside the remedial class (d near 0.85—the constraint extracts the specific benefit of admission slots, with only mobile exit options: attend alternative institutions, litigate, or advocate political change). State actors documenting remedial purpose are secondary beneficiaries (d near 0.15—they legitimize the action and gain normative authority for defining remedial purposes). Colorblind and antisubordination adherents are identity_locked into their reading (d near 1.0 if they become institutional policy-makers forced to implement remedial admissions; d near 0.5 if they remain excluded advocates). Judicial interpreters sit analytically at d near 0.5 unless they are captured by one reading (the 2023 shift suggests a pivot toward colorblind-sympathetic interpretation).
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading avoids pure snare classification because the coordination function is genuine: historical exclusion is a documented collective problem, and race-conscious admissions with documented remedial purpose do solve a real institutional coordination problem that race-blind admissions cannot. However, the constraint verges on mandatrophy because the founding problem (historical exclusion from elite institutions) has been substantially addressed through legal remedies, institutional norms, and demographic shifts, yet the remedial reading persists and even expands (theater ratio rising, doctrinal elaboration of compelling interests expanding to general diversity benefits beyond specific historical remedies). The 2023 Supreme Court reversal signals that the founding problem is now judged dead by the authoritative interpreters, making the remedial reading a zombie doctrine: it solved a live problem in 1978 (Regents v. Bakke), but by 2024, courts have ruled the problem no longer justifies the remedy. The constraint should compute as tangled_rope (genuine coordination + asymmetric extraction + active enforcement) until the 2023 pivot, then show evidence of transition toward piton (maintenance of doctrine despite ruling against its justification) or dormancy. The measurement series should not reflect this transition perfectly because the remedial reading remains institutionally embedded even after judicial rejection—universities continue to use proxy measures, and the doctrine remains live in some state systems. Mandatrophy is contested: beneficiaries and institutional adherents deny the founding problem is dead; colorblind and antisubordination readings affirm it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_purpose_scope_ambiguity,
    'How narrow must the connection between documented historical exclusion and race-conscious admissions action be? Does remedial justification extend only to groups that experienced specific institutional discrimination, or to all members of historically subordinated racial groups, or to all action that increases diversity?',
    'Examine court decisions defining compelling interest across jurisdictions and time: compare strict-remedial-harm language (e.g., specific institutional discrimination by the defendant institution) against broader diversity-interest language (e.g., benefits of racial diversity to education regardless of specific historical harm). Analyze how universities justify race-consciousness in practice versus how courts evaluate justification in litigation.',
    'Narrow scope reading (remedying specific, documented institutional discrimination) would move extractiveness downward and constrain beneficiaries to directly affected groups. Broad scope reading (diversity as compelling interest) moves extractiveness upward (more applicants become beneficiaries, more rejected applicants become victims) and expands the constraint toward snare-like characteristics. The 2023 Supreme Court decision effectively narrows the scope to near-zero, foreclosing the remedial reading in most institutional contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_purpose_scope_ambiguity, conceptual, 'Whether remedial justification is narrowly tied to specific documented harms or broadly includes diversity benefits.').

omega_variable(
    colorblind_foreclosure_question,
    'Does the remedial reading logically foreclose the colorblind reading, or can both coexist as live doctrinal alternatives?',
    'Examine whether the remedial reading''s core premise (remedial race-consciousness is constitutionally permissible under Equal Protection) logically contradicts colorblind reading''s core premise (no racial classifications are permissible regardless of purpose). The answer hinges on whether Equal Protection is read as permitting context-dependent exception-making (remedial readings hold this; colorblind reading denies it) or as categorical prohibition-setting (colorblind reading holds this; remedial reading denies it).',
    'If foreclosed: the remedial reading''s adoption in judicial and institutional practice should have displaced the colorblind reading by logical necessity. The fact that colorblind reading persists and gains ground (2023 Supreme Court decision) suggests they do NOT foreclose each other—rather, they coexist as competing readings available to different institutional interpreters. This affects the nature of the contest: if foreclosure were operative, the colorblind reading would be incoherent; instead, it is politically ascendant, suggesting the contest is not primarily logical but institutional (which reading will courts and institutions adopt).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_foreclosure_question, conceptual, 'Whether remedial and colorblind readings logically foreclose each other or coexist as live alternatives.').

omega_variable(
    antisubordination_influences_question,
    'Does the remedial reading influence or influence away the antisubordination reading? Does acceptance of remedial race-consciousness as constitutional create structural conditions that pressure courts and institutions toward antisubordination reasoning, or does the narrow tailoring doctrine resist that expansion?',
    'Trace doctrinal evolution: examine whether courts and institutions that adopt the remedial reading subsequently face pressure to expand it to cover antisubordination concerns (race-consciousness to address ongoing systemic hierarchy), or whether narrow tailoring doctrine successfully cabins the remedial reading against expansion. Compare jurisdictions with strong remedial-reading adoption against those with strong colorblind adoption and examine which face greater antisubordination pressures.',
    'If remedial influences toward antisubordination: the remedial reading may be unstable—accepting remedial race-consciousness opens the logical door to broader antisubordination reasoning. If narrow tailoring successfully resists influence: the remedial reading is more stable and the antisubordination reading remains effectively excluded from mainstream institutional policy. The answer affects the constraint''s long-term classification: an unstable remedial reading that influences toward antisubordination suggests the remedial reading is transitional (scaffold-like), not stable coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(antisubordination_influences_question, conceptual, 'Whether the remedial reading creates downstream pressure toward antisubordination reasoning or successfully resists it.').

omega_variable(
    institutional_fidelity_to_remedial_documentation,
    'When universities claim to act on remedial purposes or compelling diversity interests, do their actual admissions practices track the documented purposes, or do race-conscious considerations operate orthogonally to the stated justification?',
    'Audit universities'' internal admissions materials and decision patterns: compare stated remedial justifications and diversity interests against the observed correlation between race and admissions outcomes, controlling for other institutional criteria. Examine whether racial considerations predict admissions outcomes at the magnitude justified by documented remedial purpose, or whether they diverge (suggesting theater: the documentation provides cover for broader discretion).',
    'High fidelity would support tangled_rope classification (genuine coordination function + extraction justified by stated purpose). Low fidelity would elevate theater_ratio and suggest snare-like characteristics (race-consciousness operates as cover for less defensible allocation mechanisms). The 2023 Supreme Court decision implied low fidelity by treating diversity interests as insufficiently tethered to remedial purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_fidelity_to_remedial_documentation, empirical, 'Whether universities'' race-conscious practices track their stated remedial or diversity justifications.').

omega_variable(
    founding_problem_measurement_ambiguity,
    'Is the founding problem (historical exclusion of racial minorities from elite educational institutions) still live, dead, or contested at 2024? Has institutional exclusion been substantially remedied, or does substantial disparity persist?',
    'Examine demographic composition of elite universities over 1964–2024: track the proportion of students from historically excluded racial groups, comparing trends in admissions rates, enrollment rates, and graduation rates. Compare absolute numbers and rates against counterfactual race-blind admissions distributions. Examine wages, employment outcomes, and social mobility for graduates to assess whether exclusion effects persist.',
    'Mounting evidence that the founding problem is dead (historic underrepresentation has been substantially remedied, contemporary disparities reflect other factors than institutional exclusion, educational benefits of diversity have been achieved) would support mandatrophy classification and the 2023 Supreme Court finding that the remedial justification no longer applies. Persistent evidence of exclusion would support the founding problem being live and justify the remedial reading''s continued operation. Current state: data shows both (i) substantial improvement in minority representation at elite institutions since 1964, (ii) persistent racial disparities in admissions and outcomes. The contest is whether remaining disparities constitute exclusion justifying remedial action or are explained by factors outside the constraint''s purview.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_measurement_ambiguity, empirical, 'Whether the historical exclusion problem the remedial reading was built to solve is still live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1964, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1964, equal_protection_kernel__remedial_reading, theater_ratio, 1964, 0.18).
narrative_ontology:measurement_basis(equa_tr_t1964, observed).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement_basis(equa_tr_t1978, observed).
narrative_ontology:measurement(equa_tr_t1992, equal_protection_kernel__remedial_reading, theater_ratio, 1992, 0.35).
narrative_ontology:measurement_basis(equa_tr_t1992, observed).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_kernel__remedial_reading, theater_ratio, 2003, 0.39).
narrative_ontology:measurement_basis(equa_tr_t2003, observed).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_kernel__remedial_reading, theater_ratio, 2013, 0.42).
narrative_ontology:measurement_basis(equa_tr_t2013, observed).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_kernel__remedial_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(equa_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1964, equal_protection_kernel__remedial_reading, base_extractiveness, 1964, 0.15).
narrative_ontology:measurement_basis(equa_be_t1964, observed).
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.28).
narrative_ontology:measurement_basis(equa_be_t1978, observed).
narrative_ontology:measurement(equa_be_t1992, equal_protection_kernel__remedial_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement_basis(equa_be_t1992, observed).
narrative_ontology:measurement(equa_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement_basis(equa_be_t2003, observed).
narrative_ontology:measurement(equa_be_t2013, equal_protection_kernel__remedial_reading, base_extractiveness, 2013, 0.39).
narrative_ontology:measurement_basis(equa_be_t2013, observed).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__remedial_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(equa_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1964, equal_protection_kernel__remedial_reading, suppression_requirement, 1964, 0.32).
narrative_ontology:measurement_basis(equa_su_t1964, observed).
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement_basis(equa_su_t1978, observed).
narrative_ontology:measurement(equa_su_t1992, equal_protection_kernel__remedial_reading, suppression_requirement, 1992, 0.48).
narrative_ontology:measurement_basis(equa_su_t1992, observed).
narrative_ontology:measurement(equa_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement_basis(equa_su_t2003, observed).
narrative_ontology:measurement(equa_su_t2013, equal_protection_kernel__remedial_reading, suppression_requirement, 2013, 0.54).
narrative_ontology:measurement_basis(equa_su_t2013, observed).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__remedial_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(equa_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% The remedial reading is one interpretation of the contested equal_protection_kernel. The three readings (remedial, colorblind, antisubordination) are structurally distinct constraints with different ε values, beneficiary/victim structures, and institutional effects. The remedial reading permits race-conscious action when narrowly tailored to remedy documented historical exclusion or achieve compelling diversity interests. The colorblind reading forbids racial classifications categorically. The antisubordination reading permits broader remedial action targeting ongoing systemic hierarchy. These are not variations in measurement basis—they are genuinely different constraints instantiating different readings of the same constitutional text. The remedial reading influences and is influenced by both siblings: the colorblind reading competes for judicial adoption (2023 Supreme Court decision represents a shift toward colorblind); the antisubordination reading creates pressure to expand remedial justifications beyond documented harms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__remedial_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
