% ============================================================================
% CONSTRAINT STORY: demographic_resource_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_resource_allocation, []).

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
 *   constraint_id: demographic_resource_allocation
 *   human_readable: Demographic Resource Allocation Constraint in Contractionary Population Development
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   China's demographic transition from expansion to contraction creates
 *   wave-like cohort peaks moving through the age distribution: junior high
 *   enrollment peaks in 2026, senior high in 2029, higher education in 2032,
 *   followed by sustained decline. The dependency ratio (non-working to
 *   working population) rises as these cohorts age and subsequent cohorts
 *   shrink. The constraint is the demographic arithmetic itself — the
 *   planning apparatus must reallocate education, healthcare, and pension
 *   resources to match the shifting age structure or face service delivery
 *   failures. The claim is mountain because the cohort waves are the product
 *   of fertility decisions made 15-30 years prior; no current policy can
 *   alter the size of cohorts already born. The modest extraction (0.18)
 *   reflects that the central planning apparatus benefits from administering
 *   the reallocation (institutional capacity, policy authority) while the
 *   demographic necessity itself is non-extractive. KEY AGENTS: Central
 *   planning apparatus (institutional/analytical) administers reallocation;
 *   provincial education bureaus (institutional/constrained) manage school
 *   closures; aging cohort services (organized/mobile) receive expanding
 *   resources; young families in tier-three cities (powerless/constrained)
 *   experience reduced youth infrastructure; migrant worker households
 *   (powerless/trapped) excluded from planning grid; demographic policy
 *   analysts (analytical/analytical) observe narrow degrees of freedom.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_resource_allocation, 0.18).
domain_priors:suppression_score(demographic_resource_allocation, 0.12).
domain_priors:theater_ratio(demographic_resource_allocation, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_resource_allocation, extractiveness, 0.18).
narrative_ontology:constraint_metric(demographic_resource_allocation, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(demographic_resource_allocation, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(demographic_resource_allocation, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(demographic_resource_allocation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_resource_allocation, mountain).
narrative_ontology:human_readable(demographic_resource_allocation, "Demographic Resource Allocation Constraint in Contractionary Population Development").
narrative_ontology:topic_domain(demographic_resource_allocation, "political_economy/development_planning/state_capitalism").

domain_priors:emerges_naturally(demographic_resource_allocation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(demographic_resource_allocation, 'd6aae3e9-a090-4fec-b5db-3c8e36ef9b70').
narrative_ontology:cs_kernel_codification('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', formalized).
narrative_ontology:cs_authority_grounding('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', lineage).
narrative_ontology:cs_interpretation_layer_present('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70').
narrative_ontology:cs_reading_relation('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', demographic_resource_allocation__performance_legitimacy_qualitative_development, coexists_with).
narrative_ontology:cs_reading_relation('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', demographic_resource_allocation__performance_legitimacy_techno_nationalist, coexists_with).
narrative_ontology:cs_reading_relation('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', demographic_resource_allocation__performance_legitimacy_livelihood_security, influences).
narrative_ontology:cs_axiom('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', foundational, demographic_structure_determines_resource_allocation).
narrative_ontology:cs_axiom_status(demographic_structure_determines_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', demographic_structure_determines_resource_allocation, empirically_contingent).
narrative_ontology:cs_axiom('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', secondary, cohort_waves_require_dynamic_reallocation).
narrative_ontology:cs_axiom_status(cohort_waves_require_dynamic_reallocation, holdable).
narrative_ontology:cs_axiom_grounding('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', cohort_waves_require_dynamic_reallocation, empirically_contingent).
narrative_ontology:cs_reference_frame('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', performance_legitimacy_quantitative_growth).
narrative_ontology:cs_drift_state('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', demographic_transition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6aae3e9-a090-4fec-b5db-3c8e36ef9b70', '2026-06-12T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_resource_allocation, central_planning_apparatus).
narrative_ontology:constraint_beneficiary(demographic_resource_allocation, aging_cohort_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(demographic_resource_allocation, provincial_education_bureaus).
narrative_ontology:constraint_victim(demographic_resource_allocation, young_families_in_tier_three_cities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the reallocation of education, healthcare, and pension resources across wave-like age cohort peaks. Faces the structural necessity of closing schools in depopulating regions while expanding elderly care infrastructure in the same timeframe. The constraint is the demographic arithmetic itself — the planning apparatus does not create the cohort waves but must respond to them or face service delivery failures.
narrative_ontology:constraint_stakeholder(demographic_resource_allocation, central_planning_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Manage school closures and teacher redeployment as junior high enrollment peaks in 2026 then declines. They bear the political cost of closing facilities built during expansion while the demographic wave moves through the system. Their fiscal capacity is constrained by the same population decline that necessitates the closures.
narrative_ontology:constraint_stakeholder(demographic_resource_allocation, provincial_education_bureaus, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(demographic_resource_allocation, provincial_education_bureaus, agenda_setter).

% Healthcare providers, pension administrators, and elderly care facilities receive expanding resource allocation as the dependency ratio rises. The demographic constraint channels resources toward them not through lobbying but through arithmetic — the growing elderly population requires proportionally more services regardless of policy preference.
narrative_ontology:constraint_stakeholder(demographic_resource_allocation, aging_cohort_services, beneficiary,
    organized, biographical, mobile, national).

% Experience school closures and reduced educational infrastructure as their children move through the declining enrollment wave. They cannot exit the constraint by moving to tier-one cities due to hukou restrictions and housing costs. The resource reallocation away from youth services is not a policy choice they can contest but a demographic fact they must absorb.
narrative_ontology:constraint_stakeholder(demographic_resource_allocation, young_families_in_tier_three_cities, payer,
    powerless, biographical, constrained, regional).

% Structurally excluded from both the education system in destination cities (due to hukou) and from the pension system (due to informal employment). The demographic reallocation operates on registered populations; migrant households fall outside the planning grid entirely and bear the constraint's costs without accessing its coordination function.
narrative_ontology:constraint_stakeholder(demographic_resource_allocation, migrant_worker_households, excluded,
    powerless, immediate, trapped, local).

% Model the cohort waves and dependency ratio trajectories. They observe that the constraint's force derives from fertility decisions made 15-30 years prior, now manifesting as unavoidable resource reallocation pressures. Their analysis shows the planning apparatus has narrow degrees of freedom — the demographic structure is given, only the allocation response is variable.
narrative_ontology:constraint_stakeholder(demographic_resource_allocation, demographic_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates education, healthcare, and pension resources across age cohorts to match demographic structure. Solves the problem of service delivery to populations whose size and composition change predictably but substantially over generational timescales.
% TRANSFER_FUNCTION: Moves fiscal resources and infrastructure capacity from youth-serving sectors (schools, pediatric care) to elderly-serving sectors (pensions, geriatric care, long-term care facilities) as the population age structure shifts from expansion to contraction.
% ABSENT_VOICES: Migrant worker households are structurally excluded from the planning grid due to hukou restrictions. They would argue for portable benefits and access to urban services, but the demographic allocation operates on registered populations only. Future unborn cohorts have no voice in the resource commitments being made now that will constrain their fiscal space.
% DISAPPEARANCE_RATIONALE: If the demographic constraint disappeared — if cohort sizes stabilized or the dependency ratio froze — the resource reallocation pressure would vanish. Schools would not need to close, pension systems would not need expansion, and the planning apparatus would face a static optimization problem rather than a dynamic reallocation crisis. The constraint's force is the demographic arithmetic itself.
% FOUNDING_PROBLEM: Post-1980 fertility decline and one-child policy created wave-like cohort structure with predictable peaks moving through the age distribution. The founding problem was managing the initial expansion wave (building schools for the large cohorts); the current problem is managing the contraction wave (reallocating resources as those cohorts age and subsequent cohorts shrink).
% FOUNDING_PROBLEM_CORROBORATION: National Bureau of Statistics population data, Ministry of Education enrollment projections, and independent demographic research all corroborate that the cohort waves are ongoing and the dependency ratio is rising. The problem is not contested — the arithmetic is public and the service delivery pressures are observable across provinces.
narrative_ontology:disappearance_verdict(demographic_resource_allocation, world_rearranges).
narrative_ontology:founding_problem_status(demographic_resource_allocation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(demographic_resource_allocation, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2026-06-12',
    'performance_legitimacy_kernel_demographic_constraint', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(demographic_resource_allocation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_resource_allocation_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(demographic_resource_allocation, ExtMetricName, E),
    domain_priors:suppression_score(demographic_resource_allocation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(demographic_resource_allocation),
    narrative_ontology:constraint_metric(demographic_resource_allocation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(demographic_resource_allocation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(demographic_resource_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint's force is demographic arithmetic, not policy choice — the cohort waves exist regardless of who administers the response. The modest extraction reflects that the central planning apparatus gains institutional authority from managing the reallocation, and aging cohort services benefit from resource flows, but these are second-order effects riding on a first-order demographic necessity. Suppression is very low (0.12) because the constraint does not depend on preventing alternatives — the demographic structure is given, and local governments have some flexibility in how they respond (close schools vs. consolidate, expand home care vs. institutional care). Theater ratio is very low (0.08) because the service delivery pressures are real and observable — schools genuinely need to close where enrollment collapses, pension systems genuinely need expansion as retirees increase. Accessibility collapse is very high (0.88) because no policy can alter the size of cohorts already born; the only alternatives are different allocation responses to the same demographic fact. Resistance is low (0.15) because the constraint is widely understood as demographic necessity rather than policy imposition, though provincial governments resist specific closures and young families resist reduced services. The measurement series shows modest upward drift in extraction and suppression as the planning apparatus's role in managing the reallocation expands and as political resistance to school closures requires more active enforcement of consolidation plans.
 *
 * PERSPECTIVAL GAP:
 *   The central planning apparatus experiences the constraint as a coordination problem with narrow degrees of freedom — the demographic structure is given, only the allocation response is variable. Provincial education bureaus experience it as a fiscal and political crisis — they must close schools and redeploy teachers while facing local resistance. Young families experience it as a reduction in services they depend on, with no exit option. Aging cohort services experience it as a resource windfall driven by demographic necessity rather than policy favor. The gap is structural: the same demographic arithmetic appears as coordination necessity from the planning seat, as imposed cost from the provincial and family seats, and as natural benefit from the aging services seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The central planning apparatus is positioned as beneficiary (gains institutional authority from administering the reallocation, d near 0.2) but also bears coordination costs. Provincial education bureaus are positioned as payers (bear political costs of closures, constrained exit, d near 0.6). Aging cohort services are beneficiaries (receive expanding resource allocation, mobile exit, d near 0.1). Young families in tier-three cities are payers (experience reduced youth infrastructure, constrained exit, d near 0.7). Migrant worker households are excluded entirely (trapped, d near 0.9 for the exclusion mechanism itself). Demographic policy analysts are observers (analytical seat, d near 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is claimed as mountain (demographic arithmetic) but declares beneficiaries (central planning apparatus, aging cohort services), which triggers FSM evaluation. The mandatrophy question is whether the demographic necessity is being used to justify extractive institutional expansion beyond what the coordination function requires. The omega variables address this: if the planning apparatus's institutional gains are proportional to the coordination complexity, the mountain claim holds; if the apparatus is using demographic necessity as cover for expanding control over resource allocation beyond what the cohort waves require, it is a false summit. The modest extraction (0.18) and very low theater ratio (0.08) suggest the coordination function is genuine, but the beneficiary declaration flags the risk that demographic necessity is being leveraged for institutional authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_necessity_vs_institutional_expansion,
    'Is the central planning apparatus''s expanding role in resource reallocation proportional to the coordination complexity imposed by the demographic transition, or is demographic necessity being used to justify institutional expansion beyond what the cohort waves require?',
    'Compare the planning apparatus''s institutional growth and resource control to the actual service delivery coordination required by the demographic transition. If institutional expansion exceeds coordination complexity, the demographic necessity is being leveraged for extractive purposes.',
    'If the apparatus''s gains are proportional to coordination needs, the mountain claim holds and the modest extraction is the price of managing the transition. If institutional expansion exceeds coordination needs, the constraint is a false summit — a genuine demographic necessity being used as cover for extractive institutional growth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_necessity_vs_institutional_expansion, empirical, 'Whether demographic necessity justifies observed institutional expansion').

omega_variable(
    allocation_flexibility_vs_demographic_determinism,
    'How much of the resource reallocation pattern is determined by demographic arithmetic (cohort sizes, dependency ratios) versus policy choices about allocation priorities (education quality vs. quantity, institutional vs. home-based elderly care)?',
    'Cross-provincial comparison of allocation responses to similar demographic pressures. If responses vary substantially, the constraint has more policy flexibility than the demographic determinism framing suggests.',
    'If allocation patterns are tightly determined by demographic arithmetic, the mountain claim is strengthened. If substantial variation exists, the constraint is partly constructed — the demographic necessity is real but the allocation response embeds policy choices that benefit some actors over others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allocation_flexibility_vs_demographic_determinism, empirical, 'Degree of policy flexibility within demographic constraint').

omega_variable(
    migrant_exclusion_necessity,
    'Is the exclusion of migrant worker households from the demographic planning grid a necessary feature of the resource allocation constraint, or is it a policy choice that uses demographic necessity as justification?',
    'Examine whether including migrant households in the planning grid would make the resource allocation problem unsolvable (true demographic constraint) or merely more complex (policy choice dressed as necessity).',
    'If migrant exclusion is necessary for the allocation problem to be tractable, it is part of the mountain. If exclusion is a policy choice that simplifies administration at migrants'' expense, the constraint is partly constructed and the extraction on migrant households is higher than the base metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_exclusion_necessity, conceptual, 'Whether migrant exclusion is demographic necessity or policy choice').

omega_variable(
    fertility_policy_feedback,
    'To what extent is the current demographic constraint (cohort waves, dependency ratio) the product of prior policy choices (one-child policy, hukou restrictions) versus autonomous fertility decline?',
    'Counterfactual demographic modeling: what would China''s age structure look like absent the one-child policy? If substantially different, the ''natural'' demographic constraint is partly the product of prior state intervention.',
    'If the demographic structure is largely the product of autonomous fertility decline (as in other East Asian countries), the mountain claim is strengthened. If the one-child policy substantially amplified the cohort waves, the constraint is partly state-constructed and the ''demographic necessity'' framing obscures policy responsibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fertility_policy_feedback, empirical, 'Degree to which demographic constraint is policy-constructed vs. autonomous').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_resource_allocation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demo_tr_t0, demographic_resource_allocation, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(demo_tr_t0, observed).
narrative_ontology:measurement(demo_tr_t6, demographic_resource_allocation, theater_ratio, 6, 0.06).
narrative_ontology:measurement_basis(demo_tr_t6, observed).
narrative_ontology:measurement(demo_tr_t12, demographic_resource_allocation, theater_ratio, 12, 0.07).
narrative_ontology:measurement_basis(demo_tr_t12, observed).
narrative_ontology:measurement(demo_tr_t18, demographic_resource_allocation, theater_ratio, 18, 0.07).
narrative_ontology:measurement_basis(demo_tr_t18, projected).
narrative_ontology:measurement(demo_tr_t24, demographic_resource_allocation, theater_ratio, 24, 0.08).
narrative_ontology:measurement_basis(demo_tr_t24, projected).
narrative_ontology:measurement(demo_tr_t30, demographic_resource_allocation, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(demo_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(demo_be_t0, demographic_resource_allocation, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(demo_be_t0, observed).
narrative_ontology:measurement(demo_be_t6, demographic_resource_allocation, base_extractiveness, 6, 0.14).
narrative_ontology:measurement_basis(demo_be_t6, observed).
narrative_ontology:measurement(demo_be_t12, demographic_resource_allocation, base_extractiveness, 12, 0.16).
narrative_ontology:measurement_basis(demo_be_t12, observed).
narrative_ontology:measurement(demo_be_t18, demographic_resource_allocation, base_extractiveness, 18, 0.17).
narrative_ontology:measurement_basis(demo_be_t18, projected).
narrative_ontology:measurement(demo_be_t24, demographic_resource_allocation, base_extractiveness, 24, 0.18).
narrative_ontology:measurement_basis(demo_be_t24, projected).
narrative_ontology:measurement(demo_be_t30, demographic_resource_allocation, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(demo_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(demo_su_t0, demographic_resource_allocation, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(demo_su_t0, observed).
narrative_ontology:measurement(demo_su_t6, demographic_resource_allocation, suppression_requirement, 6, 0.09).
narrative_ontology:measurement_basis(demo_su_t6, observed).
narrative_ontology:measurement(demo_su_t12, demographic_resource_allocation, suppression_requirement, 12, 0.11).
narrative_ontology:measurement_basis(demo_su_t12, observed).
narrative_ontology:measurement(demo_su_t18, demographic_resource_allocation, suppression_requirement, 18, 0.12).
narrative_ontology:measurement_basis(demo_su_t18, projected).
narrative_ontology:measurement(demo_su_t24, demographic_resource_allocation, suppression_requirement, 24, 0.12).
narrative_ontology:measurement_basis(demo_su_t24, projected).
narrative_ontology:measurement(demo_su_t30, demographic_resource_allocation, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(demo_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_resource_allocation, resource_allocation).
narrative_ontology:affects_constraint(demographic_resource_allocation, performance_legitimacy_quantitative_growth).
narrative_ontology:affects_constraint(demographic_resource_allocation, performance_legitimacy_livelihood_security).
narrative_ontology:affects_constraint(demographic_resource_allocation, hukou_system_mobility_restriction).

% DUAL FORMULATION NOTE:
% This constraint is the demographic arithmetic underlying multiple performance legitimacy readings. The quantitative growth reading treats demographic decline as a threat to GDP growth rates (shrinking workforce, reduced investment demand). The livelihood security reading treats it as a service delivery challenge (elderly care, pension sustainability). The techno-nationalist reading treats it as a labor quality opportunity (smaller cohorts enable higher per-capita investment in education and skills). Each reading instantiates a different constraint from the same demographic fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(demographic_resource_allocation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
