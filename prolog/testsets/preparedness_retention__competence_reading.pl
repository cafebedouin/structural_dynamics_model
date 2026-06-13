% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Competence Retention Through Drills and Inspections
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   This is the competence_reading of the preparedness_retention kernel. It
 *   asserts that preparedness is fundamentally live exercised knowledge—that
 *   drills, inspections, and competence-assessment regimens are not
 *   ceremonial performances but essential coordination mechanisms that
 *   prevent atrophy of operational knowledge across personnel turnover and
 *   time gaps between real events. The constraint names the commitment:
 *   maintain response capacity through continuous practice and evaluation.
 *   This reading coexists with two sibling readings: the husk_reading
 *   (preparedness is memorial theater disconnected from actual competence)
 *   and the hybrid_reading (preparedness is stratified—technical competence
 *   retained in specialized institutions while broader societal memory
 *   becomes ceremonial). The three readings share the same institutional
 *   apparatus (drills, inspections) but disagree fundamentally on whether
 *   that apparatus functions as described.
 *
 * KEY AGENTS:
 *   - Emergency response institutions: set and enforce drill regimens, manage competence assessment. They argue drills are essential; they bear the cost of personnel release and exercise administration.
 *   - Operational personnel: participate in drills, invest time and cognitive effort. They benefit from competence development and professional advancement; they pay the cost of time away from routine work.
 *   - Population at risk: passive beneficiaries who depend on preparedness when disasters occur. They have no voice in drill design or standards.
 *   - Fiscal administrators: fund drills and can redirect resources. They face the cost-benefit tradeoff between present spending and future (probabilistic) disaster reduction.
 *   - Institutional memory keepers: maintain procedures, competence standards, lesson-retention systems. They benefit from drills as data sources for continuous improvement.
 *   - Disaster historians and analysts: provide empirical evidence on whether drills preserve actual competence or degenerate into theater.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.28).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Competence Retention Through Drills and Inspections").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/institutional_memory/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'c795ab4d-47f9-4a50-a908-eee005b9f31e').
narrative_ontology:cs_kernel_codification('c795ab4d-47f9-4a50-a908-eee005b9f31e', implicit).
narrative_ontology:cs_authority_grounding('c795ab4d-47f9-4a50-a908-eee005b9f31e', practice).
narrative_ontology:cs_interpretation_layer_present('c795ab4d-47f9-4a50-a908-eee005b9f31e').
narrative_ontology:cs_reading_relation('c795ab4d-47f9-4a50-a908-eee005b9f31e', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('c795ab4d-47f9-4a50-a908-eee005b9f31e', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('c795ab4d-47f9-4a50-a908-eee005b9f31e', foundational, live_knowledge_requires_continuous_practice).
narrative_ontology:cs_axiom_status(live_knowledge_requires_continuous_practice, holdable).
narrative_ontology:cs_axiom_grounding('c795ab4d-47f9-4a50-a908-eee005b9f31e', live_knowledge_requires_continuous_practice, empirically_contingent).
narrative_ontology:cs_axiom('c795ab4d-47f9-4a50-a908-eee005b9f31e', foundational, competence_preservable_through_coordinated_practice).
narrative_ontology:cs_axiom_status(competence_preservable_through_coordinated_practice, holdable).
narrative_ontology:cs_axiom_grounding('c795ab4d-47f9-4a50-a908-eee005b9f31e', competence_preservable_through_coordinated_practice, instrumental).
narrative_ontology:cs_reference_frame('c795ab4d-47f9-4a50-a908-eee005b9f31e', operational_competence_live_and_maintained).
narrative_ontology:cs_drift_state('c795ab4d-47f9-4a50-a908-eee005b9f31e', contemporary_post_covid_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c795ab4d-47f9-4a50-a908-eee005b9f31e', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_safety).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, operational_institutions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the constraint solves a genuine coordination problem with minimal coercive framing. Drills are voluntary participation in most democratic systems; institutions can (and do) choose to scale them back. Personnel have biographical/career incentives aligned with competence—skill development improves career prospects. Suppression is very low (0.15) because the constraint does not require active suppression of alternatives; instead, it offers a solution (competence through practice) that actors adopt because the problem is real. Theater ratio is modest (0.22) because some ceremonialism is inevitable in institutional practice, but the core function (skill development, knowledge transfer) remains substantive. Accessibility collapse is moderate-high (0.72) because, once you understand that competence atrophies without practice, the need for drills becomes nearly unavoidable—the constraint emerges from structural necessity, not from suppressed alternatives. Resistance is low (0.18) because the constraint does not demand coercive compliance; it asks for something personnel often want (skill development, professional advancement). The measurement series shows slow, modest increase in extractiveness and theater ratio over 40 time units, reflecting a slight drift toward increased administrative burden and bureaucratic documentation as drill programs mature. This is normal institutional aging, not evidence of degradation toward pure extraction. The stability at time_point 40 (compared to time_point 30) suggests the drift plateaus.
 *
 * PERSPECTIVAL GAP:
 *   The institutional agenda-setters (emergency response organizations) should compute this constraint differently from operational personnel and fiscal administrators. From the agenda-setter perspective, drills are a core function they control and believe in—low extraction, high necessity. From the operational personnel seat, drills are a coordinated activity that benefits their career but costs time—symmetric, with alignment on shared competence goals. From the fiscal administrator seat, drills are a recurrent cost with delayed, probabilistic benefit—higher apparent extraction unless they believe the competence argument strongly. The engine computes d per seat from the structural data (beneficiary/victim declarations, exit options, power): institutions get low d (beneficiary), personnel get symmetric d (coordinated), administrators get moderate d (payer but mobile, can redirect). This divergence is the analytical signal—the competence reading depends on actors having alignment on the value of live knowledge.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: population_safety (passive, benefits from competence when disaster occurs) and operational_institutions (direct stake in maintaining response capacity). Neither is extractive; both benefit from drills. Payers: operational_personnel (time and effort, but also career benefit—symmetric) and fiscal_administrators (budget cost, mobile exit—higher d but not trapped). The constraint is characterized as pure coordination because the transfer (time, budget) flows to a genuine functional output (competence preservation), not to rents or asymmetric advantage. No agent is identity_locked to participation; drills can be scaled or abandoned. Exit is constrained for personnel (their career depends on competence demonstration) but not coercively constrained—the constraint creates conditions they find beneficial.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy detection: The founding problem (knowledge atrophy across personnel turnover and time gaps between events) remains LIVE. Drills and inspections directly address that problem. There is no evidence that the founding mandate has outlived its function. The constraint does not fit the mandatrophy profile. However, the hybrid_reading and husk_reading suggest that in some jurisdictions, the constraint DOES show mandatrophy: the drill apparatus persists ceremonially while actual competence maintenance has moved elsewhere (to specialized agencies like water authorities) or has degraded entirely (to theater). This reading explicitly rejects that diagnosis—it asserts that where drills are done well, they preserve actual competence. The engine will measure this disagreement through the (founding_problem_status x disappearance_verdict) mismatch: this reading has status=live + verdict=world_rearranges (drills matter, disaster response degrades without them). If measurements show theater_ratio rising sharply or resistance collapsing, the mismatch consumer will flag the reading as contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ceremony_distinction,
    'How do we distinguish whether a drill regiment maintains actual operational competence versus performing competence as theater?',
    'Measure post-disaster response performance against predictions derived from drill performance; compare response capacity in jurisdictions with continuous drill regimens versus lapsed regimens; conduct embedded observation of how personnel use drill learning in actual operations.',
    'If competence is genuine and measurable through disaster outcomes and skill tests, the constraint''s classification as low-extraction coordination holds. If drills consistently fail to predict actual performance or competence atrophies despite regular drills, the classification shifts toward piton (inertial ritual with decoupled function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ceremony_distinction, empirical, 'Whether drill performance translates to actual operational competence.').

omega_variable(
    reading_boundary_husk_vs_competence,
    'This reading claims preparedness IS live exercised knowledge. The sibling husk_reading claims preparedness IS memorial performance disconnected from live knowledge. Are these two readings of the same constraint (competing framings of a single commitment), or two entirely different constraints?',
    'Examine the kernel text—what commitment do drills and inspections instantiate? If the commitment is ''maintain response capacity by exercise'' (kernel premise independent of outcome), then both readings are readings of it, distinguished by whether the exercises actually work. If the kernel is instead ''perform the appearance of preparedness,'' then this reading and husk_reading are incompatible descriptions of different commitments.',
    'If this reading and husk_reading are readings of the same kernel, they coexist_with each other (different parties hold them, neither forecloses the other). If they describe different kernels, this is not a kernel reading at all—it is a standalone constraint story about a competence-preserving practice that happens to share institutional apparatus with a ceremonial reading of a different kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_husk_vs_competence, conceptual, 'Whether this reading shares a kernel with its siblings or describes a distinct constraint.').

omega_variable(
    fiscal_sustainability_omega,
    'Is continuous competence preservation through drills fiscally sustainable as written, or does budget pressure eventually force degradation toward ceremonial compliance (theater)?',
    'Time-series analysis of drill budget as fraction of institutional budget across multi-decade horizon; regression of drill frequency against fiscal stress indicators; case comparison of agencies that sustained vs. abandoned drill regimens under budget cuts.',
    'If competence-preserving drills are fiscally sustainable, the constraint remains a low-extraction coordination mechanism. If budget pressure systematically drives drift toward theater (measured by increasing ratio of drill count to actual personnel engagement depth), the constraint degrades into a hybrid or piton state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_omega, empirical, 'Whether the competence reading remains viable across fiscal cycles.').

omega_variable(
    identity_fusion_preparedness_professionals,
    'For emergency response professionals, has competence-preservation become an identity-constituting practice, such that questioning drill regimens triggers identity threat rather than substantive debate?',
    'Interview and ethnographic study: ask personnel why drills matter and examine whether answers focus on capacity/skill outcomes or on professional identity/community belonging. Test whether competence critiques are met with adaptation or with identity-protective backlash.',
    'If identity fusion is high, competence-preservation language may mask extraction (time, effort, career constraint) that personnel tolerate because the practice constitutes their professional self. This would shift the beneficiary structure from ''population safety + operational personnel'' to ''professional identity maintenance,'' with operational personnel bearing identity-locked costs rather than genuinely coordinated ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_preparedness_professionals, empirical, 'Whether competence maintenance has become fused with professional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__competence_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__competence_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__competence_reading, theater_ratio, 25, 0.23).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__competence_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__competence_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__competence_reading, base_extractiveness, 25, 0.29).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__competence_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__competence_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__competence_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__competence_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__competence_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__competence_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__competence_reading, suppression_requirement, 40, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (competence_reading) of the preparedness_retention kernel. It coexists with husk_reading (preparedness as hollow ceremony) and hybrid_reading (preparedness as stratified technical/ceremonial). The three readings share institutional apparatus (drills, inspections) but disagree fundamentally on whether that apparatus preserves actual operational competence or has decoupled into theater. ε-invariance requires separate stories: the competence reading operationalizes low ε (genuine coordination, minimal extraction); the husk reading operationalizes higher ε (time and effort extracted without functional return); the hybrid reading operationalizes intermediate ε (competence genuine in specialized institutions, hollow in broader society). All three stories link via network.affects_constraints to document the kernel contest. Engine classification divergence across readings is the measurement—committer frame exists to capture what competence-preservation MEANS when different institutional actors hold different interpretations of whether it works.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
