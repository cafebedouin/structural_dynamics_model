% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy: Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint captures the reading of state legitimacy that grounds
 *   political authority in the delivery of tangible, daily-life improvements:
 *   employment stability, accessible healthcare, quality education, and
 *   dignified elderly care. It emerged as the dominant legitimacy frame
 *   during the post-2012 leadership transition, when the 'growth at all
 *   costs' model generated visible welfare gaps — rural healthcare collapse,
 *   urban employment precarity, pension shortfalls. The reading reorients
 *   fiscal policy toward consumption support and service delivery,
 *   strengthening redistribution mechanisms (targeted transfers, public
 *   service expansion, social insurance deepening). This structurally
 *   benefits service-sector workers, urban households, and direct users of
 *   public health/education/eldercare systems. The victims are
 *   capital-intensive industrial expansion (which loses priority in credit
 *   allocation and land quotas) and local government infrastructure spending
 *   (which bears the unfunded mandate of service delivery without
 *   commensurate revenue). The constraint requires active enforcement: fiscal
 *   discipline on local governments, credit guidance away from heavy
 *   industry, and political suppression of labor unrest in declining sectors.
 *   Theater ratio rises over time as 'common prosperity' slogans decorate a
 *   structure that increasingly struggles to fund its promises without
 *   growth.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.42).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.35).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'd43d2716-60f1-4ce5-8b6a-b66e133b7bc4').
narrative_ontology:cs_kernel_codification('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', implicit).
narrative_ontology:cs_authority_grounding('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', extraction).
narrative_ontology:cs_interpretation_layer_present('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4').
narrative_ontology:cs_reading_relation('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', foundational, legitimacy_derives_from_lived_welfare).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_lived_welfare, holdable).
narrative_ontology:cs_axiom_grounding('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', legitimacy_derives_from_lived_welfare, deontological).
narrative_ontology:cs_axiom('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', foundational, social_safety_net_as_primary_state_obligation).
narrative_ontology:cs_axiom_status(social_safety_net_as_primary_state_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', social_safety_net_as_primary_state_obligation, conventional).
narrative_ontology:cs_reference_frame('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', post_2012_welfare_gap_crisis).
narrative_ontology:cs_drift_state('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', common_prosperity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d43d2716-60f1-4ce5-8b6a-b66e133b7bc4', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, elderly_care_recipients).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, public_health_users).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, education_dependents).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, heavy_industry_capital).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_funding).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, export_oriented_manufacturing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, state_legitimacy_derives_from_lived_welfare).
narrative_ontology:constraint_vindicates(performance_legitimacy__livelihood_security_reading, social_safety_net_as_primary_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legitimacy frame, directs credit allocation toward service sectors, mandates local service delivery, and manages the fiscal transfer system. Can adjust the constraint's parameters (transfer levels, mandate scope) but faces hard demographic and fiscal limits. Benefits from regime stability the constraint produces.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain expanded employment in healthcare, education, elderly care, and public services. Wages and job security improve relative to manufacturing. Cannot easily exit the political system; their benefits depend on continued state prioritization of services over industry.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_workers, beneficiary,
    organized, biographical, constrained, national).

% Receive better public services, consumption subsidies, and social insurance. Also bear rising costs (housing, education supplements, private care top-ups) and implicit taxes (financial repression, currency depreciation). Exit is constrained by hukou, career, and family ties.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, urban_households, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, urban_households, payer).

% Direct users of expanded public eldercare. Their identity and survival are fused with the service; they cannot conceive of exit. The constraint's legitimacy rests visibly on their welfare. They are the moral core of the arrangement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, elderly_care_recipients, beneficiary,
    powerless, immediate, identity_locked, local).

% Benefit from expanded insurance coverage, rural clinic upgrades, and drug price controls. Exit options are limited to private insurance (costly) or medical tourism (elite only). The constraint delivers tangible daily value but strains under demographic load.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, public_health_users, beneficiary,
    moderate, biographical, constrained, regional).

% Families with children in public education gain from reduced fees, better rural schools, and tutoring crackdowns that lower competitive pressure. They also lose choice and face intensified exam competition. Exit is constrained by the gaokao system and residential sorting.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, education_dependents, beneficiary,
    moderate, generational, constrained, national).

% State-owned and private capital in steel, chemicals, machinery, construction face credit tightening, demand contraction from infrastructure slowdown, and regulatory pressure to reduce capacity. They lobby, diversify into services, or seek overseas markets — but exit is constrained by asset specificity and political dependence.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, heavy_industry_capital, payer,
    powerful, biographical, constrained, national).

% County and township governments mandated to deliver expanded services (eldercare, healthcare, education) without commensurate revenue. Land finance revenue collapses; central transfers are insufficient and earmarked. They hide debt, delay payments, and cut discretionary spending. Cannot exit their jurisdiction; trapped in a fiscal vice.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_funding, payer,
    institutional, biographical, trapped, local).

% Lose policy priority (land, credit, energy) to service sectors. Face rising labor costs from service-sector competition. Can relocate production to Southeast Asia or Mexico — mobile exit, but at cost of supply chain disruption and market access. Some capture benefits from 'dual circulation' domestic demand push.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, export_oriented_manufacturing, payer,
    powerful, biographical, mobile, global).

% Neither full beneficiaries (excluded from urban hukou services) nor pure payers (they fuel the export sector being deprioritized). They would demand portable social insurance and hukou reform if heard. Their exclusion is structural: the constraint's urban service focus leaves them in a liminal zone.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, rural_migrant_workers, excluded,
    powerless, biographical, constrained, national).

% Sees the full structure: a genuine welfare coordination function fused with asymmetric fiscal extraction, enforced through political control, with rising theater as demographic promises outrun fiscal capacity. No stake in the outcome; tracks the constraint's drift across readings.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a welfare floor — employment in services, healthcare access, education quality, eldercare dignity — that the market alone would not deliver at scale, solving the coordination problem of universal basic welfare in a developing economy with weak private safety nets.
% TRANSFER_FUNCTION: Moves fiscal resources (tax revenue, credit, land quotas) from capital-intensive industrial sectors and local government infrastructure budgets toward service sectors and household consumption subsidies, mediated by central transfers and directed credit.
% ABSENT_VOICES: Rural migrant workers (excluded from urban services, deprioritized in industrial policy), private-sector SMEs in services (crowded out by state expansion), and future taxpayers (bearing the debt service for today's transfers) are not in the room. They would demand portability, market access, and fiscal sustainability.
% DISAPPEARANCE_RATIONALE: If the livelihood-security legitimacy frame vanished overnight, credit would flow back to heavy industry, local governments would slash service mandates to service debt, urban households would lose consumption subsidies, and elderly care would revert to family-only provision. The political coalition sustaining the current regime would fracture; a new legitimacy frame (likely growth or techno-nationalist) would be imposed.
% FOUNDING_PROBLEM: Post-2012: the growth-at-all-costs model produced a welfare crisis — rural healthcare collapse, urban employment precarity, pension shortfalls, environmental degradation — that threatened regime legitimacy. The livelihood-security reading was built to solve this by making daily-life welfare the primary metric of governance success.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the central authority itself (common prosperity speeches, 2021 onward), by international development institutions (World Bank China reports on aging and inequality), and by independent scholars outside the benefiting parties (e.g., Martin Whyte on social stability, Karen Eggleston on healthcare access). No beneficiary group corroborates the problem's resolution — all attest it is worsening.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).
:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint redirects resources from high-multiplier investment to lower-multiplier consumption, but the transfer is real and measurable — not pure rent extraction. Suppression (0.35) is moderate: local governments resist unfunded mandates; industrial interests lobby against credit tightening; both are managed through political control rather than open coercion. Theater ratio (0.28) reflects genuine service delivery alongside performative 'people-centered' rhetoric that masks fiscal strain. Accessibility collapse (0.38) is low: alternatives (private healthcare, supplemental education, family eldercare) persist but are costly. Resistance (0.52) is significant: local governments hide debt, enterprises underreport employment, citizens circumvent hukou restrictions to access better services. The claimed type is tangled_rope because the constraint genuinely coordinates a welfare floor (coordination function) while asymmetrically extracting from industrial and fiscal capacity (extraction function) and requires active enforcement to maintain the redistribution.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (urban household), the constraint appears as a rope: real coordination, tangible benefits, suppression is invisible. From the victim seat (county government financing a new nursing home mandate with land sale revenue that no longer exists), it appears as a snare: extraction without coordination, active suppression of fiscal dissent. The agenda-setter seat (central fiscal authority) experiences it as a scaffold: temporary redistribution to bridge a structural transition, but with no credible sunset because the demographic crisis (aging) makes the welfare floor permanent. The engine computes this divergence from the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Service sector workers and urban households are structural beneficiaries: they receive expanded public services, employment guarantees in services, and consumption subsidies. Their exit options are constrained — they benefit from the arrangement but cannot easily leave the political system that provides it (identity_locked for elderly care recipients, constrained for others). Heavy industry capital and local government infrastructure funding are structural victims: the former faces credit rationing and demand contraction; the latter bears unfunded service mandates. Their exit options are constrained (capital) to trapped (local governments cannot exit their jurisdiction). The analytical observer (this reading's author) sees the full structure: a genuine coordination function (welfare floor) fused with asymmetric extraction (industrial-to-service transfer).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-growth welfare gaps) remains live and worsening (aging population, youth unemployment). The arrangement has not atrophied — it has intensified. Mandatrophy is not resolved; the constraint's function has expanded, not decayed. The risk is mandatrophy in reverse: the mandate grows faster than the fiscal capacity to fulfill it, converting a tangled_rope toward a snare as extraction hardens and coordination frays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a distinct reading of the performance_legitimacy kernel, or does it merely emphasize a subset of the same underlying legitimacy claim?',
    'Compare resource allocation outcomes under sibling readings: if the livelihood_security_reading produces systematically different budget shares, institutional mandates, and policy sequencing than quantitative_growth_reading, the readings instantiate different constraints.',
    'If readings are distinct constraints, each gets its own ε and classification; if they are emphasis variants, they share a constraint and the analysis must model intra-constraint variation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether sibling readings of performance_legitimacy constitute separate constraints or emphasis variants of one constraint.').

omega_variable(
    naturalness_of_welfare_priority,
    'Does prioritizing livelihood security emerge from an intrinsic state-citizen compact (natural law of legitimacy), or is it a contingent policy choice that benefits identifiable agents?',
    'Test against regime transitions: if new regimes that abandon livelihood security lose legitimacy faster than those that abandon growth targets, the priority has natural-law character; if regimes can substitute legitimacy sources, it is contingent.',
    'If natural, the constraint trends toward mountain/rope; if contingent with beneficiaries, it is a false summit candidate (tangled_rope or snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_welfare_priority, empirical, 'Whether the welfare-legitimacy link is structural or constructed.').

omega_variable(
    redistribution_efficiency_vs_extraction,
    'Are the strengthened redistribution mechanisms efficient transfers to vulnerable populations, or do they extract from productive sectors beyond the fiscal capacity of the state?',
    'Measure fiscal incidence: net transfer to service sectors/households vs. deadweight loss from capital-intensive sector contraction and local government debt service.',
    'If efficient, the constraint coordinates genuine welfare gains (rope/tangled_rope); if extractive beyond capacity, it becomes a snare with local government and industrial sectors as structural victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_efficiency_vs_extraction, empirical, 'Whether redistribution strengthens the social contract or extracts beyond sustainability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__livelihood_security_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__livelihood_security_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.24).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__livelihood_security_reading, suppression_requirement, 25, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, local_government_fiscal_strain).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, industrial_credit_allocation).

% DUAL FORMULATION NOTE:
% The performance_legitimacy kernel decomposes into four structurally distinct constraints (readings), each with different ε, beneficiaries, victims, and enforcement requirements. This reading (livelihood_security) and quantitative_growth_reading are in tension over credit allocation; qualitative_development_reading attempts to synthesize but inherits extraction from both; techno_nationalist_reading competes for the same fiscal space. All four are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, institutional, 0.35).
constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, organized, 0.25).
constraint_indexing:directionality_override(performance_legitimacy__livelihood_security_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
