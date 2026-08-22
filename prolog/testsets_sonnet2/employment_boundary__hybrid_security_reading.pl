% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Third-Category Platform Worker Status (Hybrid Security Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story authors the hybrid_security_reading of the employment_boundary
 *   kernel: platform workers occupy a legislatively created third category,
 *   distinct from both employee and independent-contractor status, that
 *   mandates partial protections (medical and injury coverage) while
 *   excluding the fuller protections (retirement security, unemployment
 *   insurance, collective bargaining, career development) that attach to
 *   employment. The reading treats this as a real but incomplete coordination
 *   structure — it solves the coverage-gap problem the two-tier system left
 *   open, but the terms of the solution were substantially shaped by the
 *   party with the most to lose from full reclassification. The referent for
 *   extractiveness is the hybrid statute as currently operating, not either
 *   sibling reading's preferred alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.52).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.48).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Third-Category Platform Worker Status (Hybrid Security Reading)").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'ec280ce9-b432-44ac-8145-a8e8f71e3ead').
narrative_ontology:cs_kernel_codification('ec280ce9-b432-44ac-8145-a8e8f71e3ead', distributed).
narrative_ontology:cs_authority_grounding('ec280ce9-b432-44ac-8145-a8e8f71e3ead', distributed).
narrative_ontology:cs_reading_relation('ec280ce9-b432-44ac-8145-a8e8f71e3ead', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec280ce9-b432-44ac-8145-a8e8f71e3ead', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_axiom('ec280ce9-b432-44ac-8145-a8e8f71e3ead', foundational, employment_status_admits_a_genuine_third_category).
narrative_ontology:cs_axiom_status(employment_status_admits_a_genuine_third_category, holdable).
narrative_ontology:cs_axiom_grounding('ec280ce9-b432-44ac-8145-a8e8f71e3ead', employment_status_admits_a_genuine_third_category, conventional).
narrative_ontology:cs_axiom('ec280ce9-b432-44ac-8145-a8e8f71e3ead', secondary, tailored_partial_protection_is_superior_to_binary_classification).
narrative_ontology:cs_axiom_status(tailored_partial_protection_is_superior_to_binary_classification, holdable).
narrative_ontology:cs_axiom_grounding('ec280ce9-b432-44ac-8145-a8e8f71e3ead', tailored_partial_protection_is_superior_to_binary_classification, instrumental).
narrative_ontology:cs_reference_frame('ec280ce9-b432-44ac-8145-a8e8f71e3ead', two_tier_employment_contractor_dichotomy).
narrative_ontology:cs_drift_state('ec280ce9-b432-44ac-8145-a8e8f71e3ead', post_gig_economy_statutory_reform, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec280ce9-b432-44ac-8145-a8e8f71e3ead', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, policy_architects_of_third_category).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_gig_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work under algorithmic dispatch and rating systems that function like direct supervision, but are classified into a new statutory tier instead of employee status. They receive medical coverage (91.5% enrollment) and injury insurance (86.2% coverage) funded by mandated platform contributions, but have no employer-sponsored retirement plan, no seniority-based advancement, no unemployment insurance, and no collective bargaining rights equivalent to employees. Exit means leaving the platform entirely, forfeiting accrued app-specific reputation and any benefit continuity.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_gig_workers, payer,
    powerless, biographical, constrained, national).

% Lobbied for and helped design the third-category statute. Pay into mandated medical and injury funds — a real, bounded cost — in exchange for statutory certainty that workers are not employees. This forecloses exposure to minimum wage floors, overtime, unemployment insurance contributions, retirement matching, and collective bargaining obligations that would apply under full employment status. The net cost of the hybrid tier is calibrated to be materially lower than employment costs while being politically defensible as 'worker protection.'
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_operators, beneficiary).

% Legislators and regulators who authored the hybrid statute can point to concrete coverage percentages (medical, injury) as evidence of a successful compromise, avoiding the harder political fight of either the formalist or substantive readings. Their institutional credit is tied to the third category's continued existence as a visible 'solution,' regardless of whether it institutionalizes a lower floor than full employment would have delivered.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, policy_architects_of_third_category, beneficiary,
    institutional, generational, analytical, national).

% Compete for labor and market share against platform operators who bear a lighter statutory cost structure. They are not party to the hybrid-category negotiations but bear competitive pressure from platforms whose labor costs are institutionally discounted relative to employment law.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers_in_adjacent_sectors, excluded,
    organized, biographical, constrained, national).

% Argued for the substantive employment reading (economic dependence, algorithmic control equals employment) during the legislative process but were structurally outvoted or sidelined in favor of the compromise. They continue to litigate and organize against the hybrid category, viewing the coverage percentages as a ceiling dressed up as a floor.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_unions_and_worker_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate disputes over whether specific platform relationships fit the third category, employment, or contracting. They review enrollment and coverage data, hear worker and platform testimony, and can refer statutory language back to the legislature for revision.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, courts_and_labor_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__hybrid_security_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__hybrid_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legislatively defined middle tier so that platform work — which does not cleanly fit either the traditional employee or independent-contractor mold — receives some baseline protections (medical and injury coverage) without forcing a costly and disruptive full reclassification fight.
% TRANSFER_FUNCTION: Moves a bounded, statutorily capped contribution from platforms into medical and injury funds for platform workers, while workers forgo retirement security, seniority-based advancement, unemployment insurance, and collective bargaining rights that would attach under full employee status.
% ABSENT_VOICES: Labor unions and worker advocates who favored the substantive employment reading were present in the legislative process but structurally outvoted; traditional employers bearing comparative cost disadvantage were not consulted at all. Both would object that the third category locks in a permanently discounted labor cost structure rather than serving as a genuine transitional compromise.
% DISAPPEARANCE_RATIONALE: If the hybrid statute vanished overnight, courts would default to applying either the formalist or substantive employment tests to platform workers directly — likely triggering a wave of reclassification litigation, changing platform cost structures substantially, and either eliminating the medical/injury coverage (if formalist wins) or extending full employment protections (if substantive wins). The current arrangement is a designed equilibrium, not a background fact.
% FOUNDING_PROBLEM: Existing employment law offered only two categories — employee or independent contractor — neither of which fit workers who are economically dependent on and algorithmically directed by a platform but lack a fixed schedule, single employer, or traditional supervisory relationship. Workers were falling through the coverage gap of the contractor category entirely.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and policy architects attest the third category solved the coverage gap, citing the medical and injury enrollment figures. Labor unions, several academic labor economists, and dissenting regulators attest from outside the benefiting coalition that the founding problem — precarious workers lacking employment-grade security — remains substantially live, and that the hybrid category was substantially shaped by platform lobbying to cap platform liability rather than to maximize worker security.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than high: workers do receive substantial, quantifiable coverage (91.5% medical, 86.2% injury enrollment) that a pure snare reading would not concede. But the theater ratio (0.42) and its upward drift reflect that the political credit claimed for 'protecting platform workers' has grown faster than the actual scope of protection — the category is increasingly invoked as a settled solution even as retirement and bargaining gaps persist unaddressed. Suppression (0.48) is moderate: workers are not physically coerced, but exit from the platform economy back into traditional employment is constrained by sector-wide adoption of similar hybrid terms, reducing the practical alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform gig workers sit closer to the target end of directionality: they are named in victims for the protections they lack, even while receiving the protections they do get — this is a partial-victim structure, which is exactly what a hybrid category should look like if it is a genuine hybrid rather than a full snare or full rope. Platform operators are the clearest beneficiaries: they trade a bounded, actuarially predictable contribution for statutory insulation from a much larger set of potential employment obligations. Policy architects are secondary beneficiaries via institutional credit, not direct financial gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) matters here: it prevents mislabeling a structure with a real, measurable coordination function (91.5%/86.2% coverage rates are not nothing) as pure extraction, while also refusing to let the coordination function launder the asymmetry — platforms get a much better deal on the exchange than workers do, and the enforcement apparatus (statutory definitions, dispute-resolution processes) actively maintains that asymmetric split rather than merely administering a neutral protection scheme.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_category_capture_degree,
    'Was the specific content of the third-category statute (which protections are mandated, which are excluded) designed by a neutral process weighing worker and platform interests equally, or was it substantially shaped by platform lobbying to minimize cost relative to what either full employment or a worker-designed hybrid would have required?',
    'Legislative history analysis: compare draft protections proposed by worker advocacy coalitions against the enacted statute''s protections, and trace which provisions were added or removed during platform-operator lobbying periods.',
    'High capture would push this reading''s ε upward toward snare-adjacent territory; low capture would support a closer-to-rope reading of the same hybrid structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_category_capture_degree, empirical, 'Degree to which platform lobbying shaped the specific content of the hybrid protections.').

omega_variable(
    third_category_kernel_reading_selection,
    'Is the employment_boundary kernel genuinely under-determined such that a third category is the structurally correct reading, or does treating it as under-determined itself serve platform interests by avoiding the substantive_employment_reading''s conclusion?',
    'Compare labor-law outcomes and worker welfare metrics in jurisdictions that adopted the hybrid reading versus jurisdictions that adopted the substantive_employment_reading for functionally identical platform work arrangements.',
    'If the hybrid reading produces materially worse worker outcomes than the substantive reading for equivalent work, that is evidence the ''third category is needed'' framing is itself doing extractive work — a conceptual ambiguity this story does not resolve, per Rule 1''s instruction to author this reading cleanly rather than adjudicate between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_category_kernel_reading_selection, conceptual, 'Whether kernel under-determination is genuine or a reading artifact serving platform interests.').

omega_variable(
    retirement_bargaining_gap_permanence,
    'Is the exclusion of retirement security and collective bargaining rights from the third category a temporary omission awaiting future legislative extension, or a structurally permanent feature of the hybrid design?',
    'Track legislative amendment activity over a multi-year window: if amendments progressively add employment-adjacent protections, the gap is transitional; if the statute remains static or protections are rolled back, the gap is structural.',
    'A transitional gap would support a scaffold-adjacent reading of at least part of this constraint''s trajectory; a permanent gap confirms the tangled_rope classification as the durable steady state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retirement_bargaining_gap_permanence, empirical, 'Whether the excluded protections are a transitional gap or a permanent structural feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__hybrid_security_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__hybrid_security_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__hybrid_security_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__hybrid_security_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(empl_tr_t24, employment_boundary__hybrid_security_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(empl_be_t4, employment_boundary__hybrid_security_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(empl_be_t8, employment_boundary__hybrid_security_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(empl_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(empl_be_t16, employment_boundary__hybrid_security_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(empl_be_t20, employment_boundary__hybrid_security_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(empl_be_t24, employment_boundary__hybrid_security_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(empl_su_t4, employment_boundary__hybrid_security_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(empl_su_t8, employment_boundary__hybrid_security_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(empl_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(empl_su_t16, employment_boundary__hybrid_security_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(empl_su_t20, employment_boundary__hybrid_security_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(empl_su_t24, employment_boundary__hybrid_security_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__substantive_employment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the employment_boundary kernel. The formalist_employment_reading treats platform workers as contractors outside employment law entirely (lowest ε for platforms, highest for workers). The substantive_employment_reading treats them as employees regardless of contract form (highest obligation on platforms, closest to zero exclusion for workers). This hybrid_security_reading occupies the middle: moderate ε reflecting partial, quantifiable worker protections coexisting with permanently excluded protections. All three share the same underlying labor-economic facts about platform work; they differ in which legal category is read onto those facts, which is exactly the kernel-reading structure the framework is designed to keep separate rather than average together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
