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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy — Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This story instantiates the livelihood-security reading of the
 *   performance-legitimacy kernel: the claim that state legitimacy rests on
 *   delivering tangible, directly-experienced improvements in employment,
 *   healthcare, education, and elderly care, rather than on abstract growth
 *   rates, structural transformation, or technological self-sufficiency.
 *   Under this reading, fiscal and investment priorities shift concretely —
 *   subsidized credit and land quotas move from capital-intensive industrial
 *   expansion and local infrastructure construction toward service-sector
 *   wages, consumption subsidies, and welfare capacity. This is a genuine
 *   coordination response to a real welfare gap, but it operates through an
 *   enforced reallocation that extracts fiscal headroom from specific
 *   institutional payers (provincial fiscal authorities, infrastructure
 *   bureaus, capital-intensive firms) to fund specific beneficiaries (service
 *   workers, households, eldercare recipients). The sibling readings —
 *   quantitative growth, qualitative development, and techno-nationalist —
 *   are separate constraints with their own ε values and stakeholder
 *   structures; they are not represented here, only referenced via network
 *   links and omega variables per the ε-invariance discipline.
 *
 * KEY AGENTS:
 *   - central_planning_authority: institutional agenda-setter reallocating fiscal and investment priority toward welfare delivery
 *   - service_sector_workers and urban_households: primary beneficiaries of the livelihood-security reallocation
 *   - capital_intensive_industrial_firms and local_government_infrastructure_bureaus: bear the reallocated cost through reduced investment quotas and unfunded service mandates
 *   - industrial_policy_technocrats: excluded voice favoring the sibling capital-formation legitimacy basis
 *   - independent_economic_analysts: analytical observer tracking whether the reallocation produces durable welfare gains or defers future costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.58).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy — Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, '057db70e-81c4-4cb7-ada6-101855b76ced').
narrative_ontology:cs_kernel_codification('057db70e-81c4-4cb7-ada6-101855b76ced', distributed).
narrative_ontology:cs_authority_grounding('057db70e-81c4-4cb7-ada6-101855b76ced', practice).
narrative_ontology:cs_interpretation_layer_present('057db70e-81c4-4cb7-ada6-101855b76ced').
narrative_ontology:cs_reading_relation('057db70e-81c4-4cb7-ada6-101855b76ced', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('057db70e-81c4-4cb7-ada6-101855b76ced', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('057db70e-81c4-4cb7-ada6-101855b76ced', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('057db70e-81c4-4cb7-ada6-101855b76ced', foundational, welfare_experience_is_the_legitimacy_metric).
narrative_ontology:cs_axiom_status(welfare_experience_is_the_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('057db70e-81c4-4cb7-ada6-101855b76ced', welfare_experience_is_the_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('057db70e-81c4-4cb7-ada6-101855b76ced', secondary, consumption_support_takes_priority_over_investment_expansion).
narrative_ontology:cs_axiom_status(consumption_support_takes_priority_over_investment_expansion, holdable).
narrative_ontology:cs_axiom_grounding('057db70e-81c4-4cb7-ada6-101855b76ced', consumption_support_takes_priority_over_investment_expansion, instrumental).
narrative_ontology:cs_reference_frame('057db70e-81c4-4cb7-ada6-101855b76ced', growth_first_legitimacy_baseline).
narrative_ontology:cs_drift_state('057db70e-81c4-4cb7-ada6-101855b76ced', post_slowdown_rebalancing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('057db70e-81c4-4cb7-ada6-101855b76ced', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, urban_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, elderly_care_recipients).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, healthcare_and_education_providers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_firms).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, construction_and_heavy_industry_workers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, provincial_fiscal_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, urban_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the political framing that ties regime legitimacy to visible improvements in employment, healthcare, education, and elderly care. Reallocates fiscal transfers, mandates minimum service benchmarks on local governments, and adjusts investment quotas away from capital projects toward consumption support. Answers to the population's felt experience of daily life, not to abstract growth statistics.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, central_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Gain from expanded hiring in healthcare, education, eldercare, and retail as consumption-support policy directs subsidies and wage floors toward these sectors. Their livelihoods improve directly and visibly, which is the currency this reading of legitimacy is built on.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sector_workers, beneficiary,
    moderate, biographical, constrained, national).

% Receive improved access to schools, clinics, and eldercare facilities, and benefit from redistribution mechanisms like transfer payments and subsidized services. Also bear some tax and pricing adjustments that fund these programs, but net experience is one of tangible gain in daily conditions.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, urban_households, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__livelihood_security_reading, urban_households, payer).

% Depend entirely on state-funded eldercare capacity that only exists because this reading of legitimacy prioritizes visible welfare delivery over industrial output. Have no exit and no alternative provider if funding priorities shift back toward investment.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, elderly_care_recipients, beneficiary,
    powerless, immediate, trapped, local).

% See investment quotas, subsidized credit, and land allocation redirected toward the service and consumption sectors, at direct cost to planned capacity expansion. Can lobby through industry associations and sometimes relocate production, but cannot exit the planning system that allocates credit and land.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industrial_firms, payer,
    powerful, biographical, constrained, national).

% Historically funded through land sales and debt-financed construction; under this reading their infrastructure and industrial-park budgets are cut in favor of social spending mandates passed down as unfunded or underfunded obligations. Cannot refuse the mandate, must absorb the fiscal shortfall or cut services elsewhere.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_bureaus, payer,
    organized, biographical, trapped, regional).

% Face reduced project pipelines and layoffs as capital-intensive infrastructure and industrial investment is deprioritized. Some can retrain into service work, but skills, geography, and age often trap them in a shrinking sector.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, construction_and_heavy_industry_workers, payer,
    powerless, biographical, constrained, regional).

% Must fund expanded social welfare mandates from provincial budgets already strained by prior debt-financed investment, without commensurate central transfers in many cases. Bear the fiscal risk of legitimacy politics decided centrally.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, provincial_fiscal_authorities, payer,
    organized, biographical, trapped, regional).

% Favor sustained capital formation and industrial upgrading as the legitimacy basis, and would argue that diverting investment to consumption support undercuts long-run productive capacity and future welfare capacity itself. Their view is subordinated whenever the livelihood-security framing dominates the political center's priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, industrial_policy_technocrats, excluded,
    institutional, generational, analytical, national).

% Track fiscal transfers, service delivery outcomes, and investment ratios to assess whether the livelihood-security framing produces durable welfare gains or defers necessary industrial and infrastructure investment, creating future fiscal or growth liabilities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, independent_economic_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__livelihood_security_reading, diffuse).
narrative_ontology:fixing_cost_class(performance_legitimacy__livelihood_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves an acute legitimacy and welfare problem: without visible improvement in employment, healthcare access, education quality, and eldercare capacity, political consent frays and social instability risk rises. Redirecting resources toward directly-experienced services coordinates state capacity around what citizens can actually feel in daily life.
% TRANSFER_FUNCTION: Moves fiscal transfers, subsidized credit, and land/investment quotas away from capital-intensive industrial expansion and local infrastructure construction toward service-sector wages, household consumption subsidies, healthcare and education budgets, and eldercare capacity — extracting fiscal headroom from provincial infrastructure authorities and industrial firms and delivering it to service providers and households.
% ABSENT_VOICES: Industrial policy technocrats favoring capital formation as the legitimacy basis are structurally sidelined when livelihood-security framing dominates; they would argue this reading trades long-run productive capacity for short-run political stability, but they do not control the political calculus that decides which reading of legitimacy wins in a given period.
% DISAPPEARANCE_RATIONALE: If livelihood-security legitimacy vanished as an organizing constraint, fiscal transfers would likely revert toward capital-intensive investment and infrastructure construction, service and welfare budgets would face renewed competition for resources, and household consumption support would lose its privileged claim on the state's fiscal envelope — a substantial reallocation of resources and political priority.
% FOUNDING_PROBLEM: Rapid capital-intensive growth strategies produced GDP expansion without commensurate improvement in ordinary citizens' lived conditions — inadequate healthcare access, underfunded education, weak eldercare provision, and employment insecurity — creating a legitimacy gap between headline growth statistics and everyday experience.
% FOUNDING_PROBLEM_CORROBORATION: Central planning authorities and service-sector beneficiaries attest the founding problem remains live and central. Independent economic analysts and industrial policy technocrats — outside the immediate beneficiary set — corroborate that a genuine service-delivery gap existed and persists in specific dimensions (eldercare capacity, rural healthcare), but also note that the livelihood-security framing is sometimes invoked opportunistically to justify short-term consumption stimulus during growth slowdowns, which is a distinct claim from the original welfare-gap problem.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real but moderate transfer: resources are reallocated from capital-intensive sectors to welfare delivery through fiscal and credit-allocation mechanisms, not through outright confiscation — this is redistribution with an active winner and loser set, not negligible-cost coordination. Suppression (0.62) reflects that the reallocation is centrally mandated onto local and provincial authorities and capital-intensive firms who cannot opt out of planning-system credit and land allocation; the mandate is enforced administratively. Theater ratio (0.40) reflects a meaningful gap between announced welfare-delivery achievements and underlying capacity-building — visible service openings and subsidy programs sometimes substitute for durable capacity investment, especially where consumption stimulus is used opportunistically during growth slowdowns. Accessibility collapse (0.50) and resistance (0.45) sit at moderate levels: alternative allocations (favoring industrial investment) remain politically available and are actively advocated by technocratic and industrial interests, so this is a contested, actively defended reallocation rather than a foreclosed one.
 *
 * DIRECTIONALITY LOGIC:
 *   Service-sector workers, urban households, and elderly care recipients are structural beneficiaries — the reading's entire legitimacy claim rests on their experienced improvement, so directionality sits near the beneficiary end for them. Capital-intensive industrial firms, local infrastructure bureaus, construction workers, and provincial fiscal authorities are structural targets — they lose planned investment, face unfunded mandates, or see project pipelines shrink, placing them near the target end. The central planning authority sits outside the d-scale as agenda-setter: it does not extract from itself, it allocates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a felt welfare gap under growth-first governance — was real and, per corroboration, remains partly live (eldercare capacity, rural healthcare access). This prevents dismissing the constraint as pure extraction theater: the coordination function is genuine. But the contested status also flags a mandatrophy risk: if the livelihood-security framing is invoked to justify consumption stimulus during growth slowdowns independent of the original welfare gap, the mandate has drifted from its founding problem toward general fiscal-cycle management, which the tangled_rope classification (rather than rope) is designed to capture — genuine coordination function AND asymmetric extraction from specific payer classes, requiring active enforcement to sustain the reallocation against competing claims on the fiscal envelope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    livelihood_reading_dominance_conditions,
    'Under what political and macroeconomic conditions does the livelihood-security reading of performance legitimacy dominate over the quantitative-growth, qualitative-development, or techno-nationalist readings, and how stable is that dominance?',
    'Track fiscal allocation shifts (investment-to-consumption ratio, social spending as share of budget) across multiple growth-cycle phases; identify whether livelihood-security framing rises specifically during growth slowdowns or employment shocks, which would indicate it functions as a cyclical legitimacy fallback rather than a stable governing philosophy.',
    'If livelihood-security legitimacy is primarily a counter-cyclical fallback invoked during growth stress, its ε and stakeholder structure should be read as transitional (closer to scaffold dynamics) rather than as a stable tangled_rope; if it represents a durable structural commitment, the tangled_rope reading with entrenched winner/loser sets is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_reading_dominance_conditions, conceptual, 'Whether this reading is a stable governing commitment or a cyclical fallback triggered by growth or employment stress.').

omega_variable(
    reading_boundary_ambiguity,
    'Where exactly does the livelihood-security reading''s resource claim end and the qualitative-development reading''s claim begin, given both sometimes justify service-sector investment (e.g., healthcare technology, education modernization) using overlapping language?',
    'Examine budget line-item justifications: livelihood-security framing emphasizes direct consumption/access improvement (more clinics, more teachers, higher eldercare capacity), while qualitative-development framing emphasizes efficiency and innovation gains (telemedicine platforms, ed-tech, productivity-enhancing automation in care delivery) even when funding the same nominal sector.',
    'Misattributing a qualitative-development-driven investment to this reading would inflate this constraint''s beneficiary set and understate its true extraction from capital-intensive industry, since some service-sector spending is actually justified on efficiency grounds rather than direct livelihood grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Ambiguity in disentangling livelihood-security justified spending from qualitative-development justified spending within overlapping service sectors.').

omega_variable(
    opportunistic_invocation_risk,
    'Is the livelihood-security legitimacy claim invoked genuinely to address the founding welfare gap, or opportunistically to justify consumption stimulus that primarily serves short-term growth-rate management (which would actually belong to the quantitative-growth reading)?',
    'Compare the timing and targeting of livelihood-security policy announcements against GDP growth trajectory: genuine welfare-gap-driven policy should track underlying service-access metrics (hospital beds per capita, eldercare wait times), while opportunistic invocation should track quarterly growth figures more closely than access metrics.',
    'High correlation with growth-rate timing rather than access-metric need would suggest partial mandatrophy — the livelihood-security mandate is doing genealogically different work than its founding problem, which would push the classification toward a more purely extractive reading of the fiscal reallocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunistic_invocation_risk, empirical, 'Whether livelihood-security policy timing tracks genuine welfare need or growth-cycle management.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t4, performance_legitimacy__livelihood_security_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__livelihood_security_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__livelihood_security_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__livelihood_security_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__livelihood_security_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perf_be_t4, performance_legitimacy__livelihood_security_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__livelihood_security_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__livelihood_security_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__livelihood_security_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__livelihood_security_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perf_su_t4, performance_legitimacy__livelihood_security_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__livelihood_security_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__livelihood_security_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__livelihood_security_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__livelihood_security_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__livelihood_security_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept of 'performance legitimacy' per the ε-invariance principle. Each reading names a different primary beneficiary/victim structure and a different fiscal reallocation pattern: livelihood_security_reading (this story) prioritizes service delivery and consumption support at the expense of capital-intensive investment and local infrastructure; quantitative_growth_reading prioritizes GDP expansion metrics; qualitative_development_reading prioritizes innovation/efficiency/sustainability transformation; techno_nationalist_reading prioritizes strategic-industry self-sufficiency. These are not the same constraint measured differently — they have distinct stakeholder sets, distinct transfer directions, and are expected to have distinct ε values, since each reading licenses a different resource claim against the same finite fiscal envelope. When one reading dominates political discourse, it structurally pressures the others' resource availability (an influences relationship), documented per-story in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
