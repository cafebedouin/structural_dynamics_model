% ============================================================================
% CONSTRAINT STORY: single_cell_mechanistic_research_funding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_single_cell_mechanistic_research_funding, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: single_cell_mechanistic_research_funding
 *   human_readable: Single Cell Mechanistic Research Funding Constraint
 *   domain: biomedical_research/funding_allocation
 *
 * SUMMARY:
 *   Single-cell mechanistic research occupies a structural bottleneck in
 *   biomedical funding allocation. The constraint arises from a systematic
 *   mismatch between scientific discovery value and funding incentive
 *   structure. Mechanistic studies of individual cell behavior require
 *   expensive instrumentation (optical tweezers, atomic force microscopy,
 *   high-speed imaging), long experimental timelines, and yield results that
 *   are difficult to quantify and report in standardized metrics.
 *   High-throughput phenotyping (bulk RNA-seq, flow cytometry, imaging
 *   screens) produces countable outputs (genes expressed, cell populations
 *   identified, phenotype hits) that map cleanly to productivity metrics and
 *   publication culture. Funding agencies, constrained by metrics-driven
 *   allocation systems, favor high-throughput work. This creates asymmetric
 *   extraction: junior researchers pursuing mechanistic questions face
 *   limited funding, graduate students must publish low-yield negative
 *   results or inconclusive mechanistic data, while established groups
 *   capturing resources for phenotyping can hire junior researchers to
 *   execute expensive mechanistic pilots as subsidiary work. The constraint
 *   exhibits genuine coordination function (standardized protocols, shared
 *   mechanistic frameworks, collaborative instrument access) alongside
 *   asymmetric extraction (funding flows toward phenotyping enterprises,
 *   mechanistic branches underfunded). This is a tangled_rope at baseline,
 *   with risk of degradation to snare if review bias tightens, and potential
 *   sunset via open-data repositories and pre-registered mechanism studies.
 *
 * KEY AGENTS:
 *   - Junior Researchers (postdocs, graduate students): Primary victims (powerless/trapped) — career pathway locked into mechanistic work with minimal funding; exit costs are severe (accumulated expertise becomes liabilities in better-funded phenotyping domains)
 *   - Mid-Career Investigators: Constrained victims (moderate/constrained) — face funding bias but retain some agency and benefit from method-sharing; can transition to phenotyping with partial expertise transfer
 *   - Established Cell Biology Groups: Primary beneficiaries (institutional/arbitrage) — capture high-throughput funding, hire junior researchers for mechanistic pilots, benefit from coordination of shared protocols
 *   - Instrumentation Manufacturers: Incidental beneficiaries (institutional/arbitrage) — benefit from demand for expensive single-cell instruments, external to researcher-funding dyad
 *   - Open Mechanism Consortium: Organized agents (organized/constrained) — building shared databases and low-cost protocols that create potential sunset to the constraint
 *   - Grant Review System: Institutional actor (institutional/arbitrage) — maintains performative bias against mechanisms; theater ratio high because stated values (study mechanisms) diverge from actual allocation (fund phenotyping)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(single_cell_mechanistic_research_funding, 0.58).
domain_priors:suppression_score(single_cell_mechanistic_research_funding, 0.62).
domain_priors:theater_ratio(single_cell_mechanistic_research_funding, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(single_cell_mechanistic_research_funding, extractiveness, 0.58).
narrative_ontology:constraint_metric(single_cell_mechanistic_research_funding, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(single_cell_mechanistic_research_funding, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(single_cell_mechanistic_research_funding, tangled_rope).
narrative_ontology:human_readable(single_cell_mechanistic_research_funding, "Single Cell Mechanistic Research Funding Constraint").
narrative_ontology:topic_domain(single_cell_mechanistic_research_funding, "biomedical_research/funding_allocation").

domain_priors:requires_active_enforcement(single_cell_mechanistic_research_funding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(single_cell_mechanistic_research_funding, established_cell_biology_groups).
narrative_ontology:constraint_beneficiary(single_cell_mechanistic_research_funding, instrumentation_manufacturers).
narrative_ontology:constraint_beneficiary(single_cell_mechanistic_research_funding, high_throughput_screening_platforms).
narrative_ontology:constraint_victim(single_cell_mechanistic_research_funding, junior_researchers).
narrative_ontology:constraint_victim(single_cell_mechanistic_research_funding, mechanism_discovery_science).
narrative_ontology:constraint_victim(single_cell_mechanistic_research_funding, mechanistically_orphaned_cell_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR RESEARCHER (SNARE) — Career pathway locked into single-cell mechanistic research despite minimal funding availability. Exit options are trapped: publishing negative or mechanistically inconclusive single-cell work damages career trajectory; abandoning the research area forfeits accumulated expertise; institutional pressure to publish means continuing extraction-prone pathways. Maximum extraction experienced by agents with no alternative.
constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER INVESTIGATOR (TANGLED ROPE) — Constrained by grant review bias against mechanistic single-cell studies, but also benefits from coordination of standardized methods, shared protocols, and collaborative access to expensive instrumentation. Extraction is real (funding favors high-throughput phenotyping over mechanism) but balanced against genuine coordination benefits from method-sharing communities. Significant agency but asymmetric extraction.
constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED CELL BIOLOGY GROUP (ROPE) — Benefits from brand recognition, preliminary data advantage, and ability to frame single-cell mechanistic work as hypothesis-generating screening. Experiences funding constraint as a coordination problem: securing resources for large-scale screening requires partnering with junior researchers and method-sharing. Net beneficiary — extraction flows toward this institutional agent.
constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTRUMENTATION MANUFACTURER (ROPE) — Benefits from funding-driven demand for single-cell imaging, flow cytometry, and sequencing platforms. Experiences the constraint as coordination: standardization of mechanical readout protocols drives bulk hardware sales. No extraction experienced — external to the researcher-funding dyad.
constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN MECHANISM CONSORTIUM (SCAFFOLD) — Organized network (EMBL, CRG, Caltech biological networks) building shared single-cell mechanistic databases and low-cost community protocols. Sees the funding constraint as temporary: open-access mechanism repositories and pre-registered replication studies are creating alternatives to expensive proprietary instrument dependency. Sunset clause: as community datasets mature (5-10 years), the funding bottleneck loses coercive force because mechanistic questions become answerable via data integration rather than de novo expensive experiments.
constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: GRANT REVIEW BIAS SYSTEM (PITON) — Funding agency bias against mechanistic single-cell studies persists through institutional inertia and theater: review panels favor high-throughput phenotyping because outcomes are countable and measurable; mechanistic studies appear less 'productive' despite deeper insight. The review ritual maintains its form (studying molecular mechanisms is still legitimated in abstract) while the actual allocation mechanism degrades it in practice. Piton classification derives from high theater_ratio and mismatch between stated values and allocation pattern.
constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: MECHANISM DISCOVERY SCIENCE FIELD (TANGLED ROPE) — The abstract scientific function of understanding why cells behave mechanistically is both coordinated (through shared mechanistic frameworks and validated protocols) and extracted from (funding favors phenotyping and throughput over mechanistic depth). The field experiences genuine coordination benefits (standardized single-cell mechanical assays, validated readouts) alongside asymmetric extraction (resources flow to phenotyping enterprises while mechanistic branches are underfunded). This is a field-level tangled rope: mechanism science coordinates discovery while being systematically under-resourced relative to its explanatory value.
constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(single_cell_mechanistic_research_funding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(single_cell_mechanistic_research_funding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(single_cell_mechanistic_research_funding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(single_cell_mechanistic_research_funding, TR),
    TR >= 0.70.

:- end_tests(single_cell_mechanistic_research_funding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from junior researchers through career risk and limited funding, but extraction is not maximal because mechanistic work is still recognized as scientifically legitimate and some funding pathways exist (foundation grants, institutional seed funding, international collaborations). The trajectory shows accumulation: extractiveness rose from 0.42 to 0.58 over 14 years as throughput demands increased and mechanistic questions became less 'productive' by metric standards. Suppression (0.62): Moderate-high. Barriers to exit include career pathway lock-in (accumulated mechanistic expertise is less valuable outside mechanistic domains), institutional reward structures (publications per dollar favors phenotyping), and publication bias (negative mechanistic results are harder to publish). But suppression is not total — some researchers successfully transition to applied mechanistic work (bioengineering, medical devices), and international funding provides alternative paths. Theater ratio (0.68): Moderate-high. The grant review system maintains performative commitment to understanding mechanisms while actual allocation metrics favor countable throughput. Review panels include statements about mechanistic value, yet funding success correlates poorly with mechanistic depth and strongly with phenotypic output. Theater has increased over time as measurement standardization favors quantifiable metrics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (single-cell mechanistic research in a metrics-driven funding system) produces radically different classifications. Junior researchers see a snare (trapped, no acceptable exit, maximum extraction). Established groups see a rope (genuine coordination benefits, net advantage). Mid-career investigators see tangled rope (mixed coordination and extraction, some agency). The open consortium sees a scaffold with sunset (temporary problem being solved by open data). The grant review system sees a piton (performative ritual, institutional inertia, degraded function). The field itself (mechanism discovery science) sees tangled rope (coordinates mechanistic frameworks while being underfunded). The perspectival divergence is not confusion — it is diagnosis. When powerless and institutional agents classify the same constraint differently, the gap reveals the extraction mechanism: the constraint genuinely benefits those with power and arbitrage while trapping those without.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from beneficiary/victim status plus exit options. Junior researchers face high directionality (d ≈ 0.92): they are victims with trapped exits, maximizing experienced extractiveness. Mid-career investigators face moderate directionality (d ≈ 0.68): they are victims but with constrained (not trapped) exits, partially buffering extraction. Established groups face low directionality (d ≈ 0.12): they are beneficiaries with arbitrage options, experiencing negative effective extraction (the constraint subsidizes them). The open consortium faces moderate-high directionality (d ≈ 0.55): they are organized victims with mobile exits, giving them agency despite extraction. The grant system itself faces low directionality (d ≈ 0.18): as an institutional beneficiary with arbitrage options (they can reshape review criteria), they experience the constraint as coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The constraint avoids misclassification by distinguishing genuine coordination (shared mechanistic protocols, standardized assays) from asymmetric extraction (funding flows toward phenotyping, mechanistic branches underfunded). The tangled_rope classification requires three structural features: (1) beneficiaries exist (established groups, phenotyping enterprises, manufacturers), (2) victims exist (junior researchers, mechanistic branches), (3) active enforcement is required (grant review bias, metric standardization). All three are present. The constraint is NOT pure rope (there is real extraction, not just coordination), and it is NOT pure snare (coordination benefits are genuine, not cover story). The mandatrophy is resolved by acknowledging the dual structure: the constraint genuinely coordinates mechanistic science while simultaneously extracting from agents without power to redirect resources. The open consortium scaffold perspective shows that the extraction could be reduced (via data repositories and pre-registered studies) without destroying the coordination function — this is the diagnostic edge: a snare cannot have a sunset (extraction would fight dissolution), but a tangled rope can (the coordination component survives resource reallocation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanistic_depth_versus_throughput_tradeoff,
    'Is the funding bias against single-cell mechanistic research driven by genuine scientific priority allocation (mechanisms are inherently less productive than phenotyping), or by measurement theater (mechanisms are harder to quantify)?',
    'Citation impact analysis: do mechanistic single-cell studies (when published) generate higher downstream hypothesis-building than high-throughput phenotyping studies? Predictive value comparison: which class of studies better predicts therapeutic outcomes?',
    'If mechanism studies have higher impact: current funding allocation is misaligned with actual scientific value — constraint is more extractive (higher epsilon) and represents pure rent-seeking by phenotyping enterprises. If phenotyping has higher impact: constraint reflects legitimate prioritization — epsilon drops, classification becomes more rope-like (genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanistic_depth_versus_throughput_tradeoff, empirical, 'Whether mechanistic research has lower or higher intrinsic scientific value than phenotyping').

omega_variable(
    open_data_sufficiency_for_mechanism,
    'Can mechanistic questions about single cells be answered robustly through data integration over existing repositories (mine existing single-cell measurements for mechanical properties), or does each mechanistic hypothesis require de novo expensive experiments?',
    'Proof-of-concept studies: replication of mechanistic conclusions from repository data versus required new experiments; completeness of repositoried mechanical readouts across cell types and perturbations',
    'If repository mining is sufficient: scaffold sunset is real, funding constraint has genuine time horizon (high confidence in sunset_clause). If new experiments required: open data cannot substitute for expensive instruments — sunset is aspirational rather than structural, constraint persists longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_data_sufficiency_for_mechanism, empirical, 'Whether existing single-cell data repositories contain sufficient mechanistic information').

omega_variable(
    junior_researcher_exit_cost_magnitude,
    'What is the actual economic and career cost for a junior researcher exiting single-cell mechanistic research mid-career (i.e., switching to better-funded phenotyping or non-mechanistic biology)?',
    'Career trajectory analysis: publication record recovery time, citation metrics, funding access, employment timeline for researchers who switched research areas post-PhD',
    'If exit cost is moderate (6-12 month recovery): exit_options should be ''constrained'' rather than ''trapped'' — classification shifts from snare to tangled_rope. If exit cost is severe (3+ years of diminished productivity): trapped classification confirmed, snare designation is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(junior_researcher_exit_cost_magnitude, empirical, 'Career cost of exiting single-cell mechanistic research').

omega_variable(
    review_panel_conscious_bias_versus_structural_incentive,
    'Do grant reviewers consciously devalue mechanistic work, or do they follow institutional incentives that de facto penalize mechanism studies?',
    'Analysis of grant review criteria coding: explicit mentions of ''mechanism'' in funded vs rejected abstracts; survey of reviewer self-reported reasoning; investigation of unconscious bias (does mechanism mention trigger lower scores independent of content quality?)',
    'If conscious bias: constraint is maintained by deliberate extraction enforcement — requires active resistance (higher suppression, full snare from institutional perspective). If structural incentive (reviewer panels are metric-driven): constraint is maintained by theater and misalignment of metrics to actual scientific value — more amenable to scaffold reform (shift metrics, not reviewer attitudes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_panel_conscious_bias_versus_structural_incentive, empirical, 'Whether review bias against mechanisms is deliberate or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(single_cell_mechanistic_research_funding, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scmrf_tr_t0, single_cell_mechanistic_research_funding, theater_ratio, 0, 0.55).
narrative_ontology:measurement(scmrf_tr_t7, single_cell_mechanistic_research_funding, theater_ratio, 7, 0.62).
narrative_ontology:measurement(scmrf_tr_t14, single_cell_mechanistic_research_funding, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(scmrf_be_t0, single_cell_mechanistic_research_funding, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(scmrf_be_t7, single_cell_mechanistic_research_funding, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(scmrf_be_t14, single_cell_mechanistic_research_funding, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(single_cell_mechanistic_research_funding, resource_allocation).
narrative_ontology:affects_constraint(single_cell_mechanistic_research_funding, high_throughput_phenotyping_funding_dominance).
narrative_ontology:affects_constraint(single_cell_mechanistic_research_funding, junior_researcher_career_precarity).
narrative_ontology:affects_constraint(single_cell_mechanistic_research_funding, mechanistic_knowledge_bottleneck).

% DUAL FORMULATION NOTE:
% Single-cell mechanistic research funding is upstream of both high-throughput phenotyping dominance (causally enables the metric bias system) and mechanistic knowledge bottleneck (represents the field-level consequence). Decomposed into three stories: (1) this story (funding allocation constraint, ε=0.58, tangled_rope), (2) phenotyping dominance (metrics-driven allocation bias, ε=0.42, rope-piton hybrid), (3) knowledge bottleneck (field-level underfunding of mechanism science, ε=0.52, tangled_rope). Each story has distinct ε values because they measure different observable constraints: this story measures funding asymmetry; phenotyping dominance measures institutional bias; knowledge bottleneck measures scientific consequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(single_cell_mechanistic_research_funding, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
