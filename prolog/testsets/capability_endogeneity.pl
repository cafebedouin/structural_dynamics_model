% ============================================================================
% CONSTRAINT STORY: capability_endogeneity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_endogeneity, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: capability_endogeneity
 *   human_readable: Capability Endogeneity in AI Measurement
 *   domain: epistemology/measurement/artificial_intelligence
 *
 * SUMMARY:
 *   Capability measurement in AI relies on standardized benchmarks that
 *   operationalize 'capability' as performance on specific task
 *   distributions. The constraint is that measured capability is not drawn
 *   from a fixed latent pool but is actively cultivated by research effort
 *   directed toward benchmark-legible competences. When benchmarks shift or
 *   funding moves to new standards, previously optimized competences
 *   atrophy—a phenomenon researchers call the 'capability rust belt.' The
 *   constraint coordinates research effort around shared measurement
 *   standards while simultaneously shaping what capabilities exist to be
 *   measured. This is one reading (partition_choice_reading) of the contested
 *   kernel 'generality_standard'—what makes capability general rather than
 *   narrow. KEY AGENTS (by structural relationship): - Benchmark consortia:
 *   Agenda-setters (institutional/mobile) — design the measurement standards
 *   that operationalize capability - AI labs with benchmark access:
 *   Beneficiaries (institutional/constrained) — optimize toward benchmarks,
 *   benefit from stable standards they have adapted to - Capability
 *   forecasters: Beneficiaries (organized/mobile) — depend on benchmarks
 *   measuring fixed latent capability for forecasting tractability -
 *   Researchers studying capability rust: Observers (moderate/mobile) —
 *   document endogeneity empirically - Philosophy of measurement community:
 *   Observers (moderate/analytical) — provide theoretical framework for
 *   understanding measurement as construction - Systems deployed under prior
 *   standards: Non-agent payers (powerless/trapped) — stranded optimization
 *   cost when standards shift - Downstream users of capability claims: Payers
 *   (moderate/constrained) — bear cost when systems fail outside benchmark
 *   distribution
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_endogeneity, 0.38).
domain_priors:suppression_score(capability_endogeneity, 0.42).
domain_priors:theater_ratio(capability_endogeneity, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_endogeneity, extractiveness, 0.38).
narrative_ontology:constraint_metric(capability_endogeneity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(capability_endogeneity, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(capability_endogeneity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(capability_endogeneity, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_endogeneity, rope).
narrative_ontology:human_readable(capability_endogeneity, "Capability Endogeneity in AI Measurement").
narrative_ontology:topic_domain(capability_endogeneity, "epistemology/measurement/artificial_intelligence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(capability_endogeneity, 'f79d690e-cbaf-47ce-b9e5-9c0856acfb58').
narrative_ontology:cs_kernel_codification('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', distributed).
narrative_ontology:cs_authority_grounding('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', distributed).
narrative_ontology:cs_reading_relation('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', capability_endogeneity__trajectory_extrapolation_reading, forecloses).
narrative_ontology:cs_reading_relation('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', capability_endogeneity__generation_gate_reading, coexists_with).
narrative_ontology:cs_axiom('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', foundational, capability_is_observer_dependent_partition).
narrative_ontology:cs_axiom_status(capability_is_observer_dependent_partition, holdable).
narrative_ontology:cs_axiom_grounding('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', capability_is_observer_dependent_partition, conventional).
narrative_ontology:cs_axiom('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', foundational, measurement_constructs_rather_than_discovers).
narrative_ontology:cs_axiom_status(measurement_constructs_rather_than_discovers, holdable).
narrative_ontology:cs_axiom_grounding('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', measurement_constructs_rather_than_discovers, empirically_contingent).
narrative_ontology:cs_reference_frame('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', capability_as_latent_property).
narrative_ontology:cs_drift_state('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', post_capability_rust_documentation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f79d690e-cbaf-47ce-b9e5-9c0856acfb58', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_endogeneity, benchmark_consortia).
narrative_ontology:constraint_beneficiary(capability_endogeneity, ai_labs_with_benchmark_access).
narrative_ontology:constraint_beneficiary(capability_endogeneity, capability_forecasters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(capability_endogeneity, downstream_users_of_capability_claims).
narrative_ontology:constraint_vindicates(capability_endogeneity, measurement_shapes_reality).
narrative_ontology:constraint_vindicates(capability_endogeneity, capability_as_constructed_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain the standardized benchmarks that operationalize 'capability' as measurable performance. They choose task distributions, scoring functions, and what counts as general versus narrow. Their choices structure what gets optimized and what atrophies. They benefit from being the authoritative measurement infrastructure but face competitive pressure from alternative benchmark suites.
narrative_ontology:constraint_stakeholder(capability_endogeneity, benchmark_consortia, agenda_setter,
    institutional, generational, mobile, global).

% Optimize systems toward benchmark performance because benchmarks determine funding, publication, and market valuation. Early access to benchmark construction details or compute resources to iterate on benchmark-adjacent tasks gives structural advantage. They benefit from stable benchmarks they have optimized for, and from the legitimacy benchmarks confer on their capability claims.
narrative_ontology:constraint_stakeholder(capability_endogeneity, ai_labs_with_benchmark_access, beneficiary,
    institutional, biographical, constrained, global).

% Produce timeline estimates and capability projections that depend on treating benchmarks as measuring fixed latent capability rather than cultivated performance. Their forecasting models assume capability is discovered not constructed. They benefit from the apparent objectivity and continuity of benchmark trajectories, which makes forecasting tractable.
narrative_ontology:constraint_stakeholder(capability_endogeneity, capability_forecasters, beneficiary,
    organized, biographical, mobile, global).

% Document capability atrophy when benchmarks shift: competences that were optimized under one standard decay when funding and attention move to new standards. They measure the endogeneity empirically but lack institutional power to change how capability is operationalized in deployment or policy contexts.
narrative_ontology:constraint_stakeholder(capability_endogeneity, researchers_studying_capability_rust, observer,
    moderate, biographical, mobile, global).

% Analyze the conceptual structure of capability measurement and argue that benchmarks do not neutrally measure pre-existing capability but participate in constructing what capability means. They provide the theoretical framework for understanding endogeneity but are structurally outside the AI development and forecasting communities where capability claims have material consequences.
narrative_ontology:constraint_stakeholder(capability_endogeneity, philosophy_of_measurement_community, observer,
    moderate, generational, analytical, global).

% AI systems optimized for benchmarks that are no longer funded or maintained. Their performance on legacy tasks may remain high but their competences are no longer valued or maintained. They represent sunk optimization cost that becomes stranded when the measurement standard shifts.
narrative_ontology:constraint_stakeholder(capability_endogeneity, systems_deployed_under_prior_standards, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(capability_endogeneity, systems_deployed_under_prior_standards).

% Make decisions based on capability assessments that assume benchmarks measure stable, general competence. They bear the cost when systems fail on tasks outside the benchmark distribution or when competences atrophy after deployment. Their ability to verify capability claims independently is limited by the same measurement infrastructure that produced the claims.
narrative_ontology:constraint_stakeholder(capability_endogeneity, downstream_users_of_capability_claims, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides shared operationalization of 'capability' so AI systems can be compared, progress can be tracked, and resource allocation decisions can be grounded in measurable performance rather than subjective assessment.
% TRANSFER_FUNCTION: Moves research effort, compute resources, and institutional attention toward benchmark-legible competences and away from competences that are not captured in current benchmark suites. Transfers legitimacy from measurement infrastructure (benchmark consortia) to systems that score well.
% ABSENT_VOICES: Alternative measurement frameworks that would operationalize capability differently, and competences that are not benchmark-legible but may be practically important. These are structurally excluded because the coordination function requires standardization, which necessarily privileges some operationalizations over others.
% DISAPPEARANCE_RATIONALE: If benchmark-driven capability measurement vanished, AI development would fragment into incomparable local optimization targets, forecasting would lose its empirical anchor, and resource allocation would revert to subjective assessment or alternative coordination mechanisms. The research community would reorganize around different standards for what counts as progress.
% FOUNDING_PROBLEM: Early AI research lacked shared standards for comparing systems or measuring progress, making it difficult to allocate resources, replicate results, or accumulate knowledge across research groups.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem is attested by researchers across the AI community, including those critical of current benchmarks. The philosophy of measurement community and capability rust researchers corroborate that the founding problem remains live while also documenting that the solution has created new structural effects (endogeneity, capability atrophy) that were not part of the original problem.
narrative_ontology:disappearance_verdict(capability_endogeneity, world_rearranges).
narrative_ontology:founding_problem_status(capability_endogeneity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(capability_endogeneity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-29',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(capability_endogeneity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_endogeneity_tests).
:- end_tests(capability_endogeneity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint channels research effort toward benchmark-legible competences and away from alternatives, creating path dependency and stranded optimization cost when standards shift. However, the coordination function is genuine—shared measurement standards solve a real collective action problem in comparing systems and tracking progress. Suppression is moderate (0.42) because alternative operationalizations of capability are structurally disadvantaged but not actively prohibited; researchers can pursue non-benchmark work but face funding and publication barriers. Theater ratio is low-moderate (0.28) because most benchmark activity serves the real coordination function, though an increasing share involves gaming benchmarks or optimizing for benchmark performance rather than underlying capability. Accessibility collapse is low (0.35) because alternative measurement frameworks remain conceptually available and some research communities pursue them, though they lack the institutional infrastructure of dominant benchmarks. Resistance is moderate-high (0.58) because the philosophy of measurement community and capability rust researchers actively contest the assumption that benchmarks neutrally measure pre-existing capability, and some labs pursue alternative evaluation frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the benchmark consortia and AI labs' position, the constraint is genuine coordination infrastructure that makes progress measurable and comparable. From the capability rust researchers' position, the same structure operates as a cultivation mechanism that shapes what capabilities exist rather than neutrally measuring them. From the forecasters' position, benchmarks provide the empirical anchor that makes timeline estimation tractable. From the philosophy of measurement community's position, treating benchmarks as measuring fixed latent capability commits a category error—capability is constructed through the measurement process, not discovered by it. The engine computes these divergences from the structural data; the claimed type (rope) reflects the coordination function while the metrics capture the moderate extraction from path dependency and capability atrophy.
 *
 * DIRECTIONALITY LOGIC:
 *   Benchmark consortia are agenda-setters with mobile exit—they design the standards but face competitive pressure from alternative benchmarks, placing them near the beneficiary end but not at full beneficiary (d ≈ 0.25). AI labs with benchmark access are beneficiaries with constrained exit—they benefit from stable standards they have optimized for but are locked into benchmark-driven development by funding and publication incentives (d ≈ 0.20). Capability forecasters are beneficiaries with mobile exit—they benefit from treating benchmarks as measuring fixed capability but could adopt alternative forecasting frameworks (d ≈ 0.15). Researchers studying capability rust and philosophy of measurement community are observers with analytical perspective (d ≈ 0.50). Systems deployed under prior standards are non-agent payers bearing stranded optimization cost (not assigned d as non-agents). Downstream users are payers with constrained exit—they bear costs when capability claims fail to generalize but have limited ability to verify claims independently (d ≈ 0.65).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids pure extraction classification because the coordination function is genuine and ongoing—AI research still needs shared standards for comparison and progress tracking. It avoids pure coordination classification because the endogeneity effect creates real costs: competences atrophy when standards shift, research effort is channeled toward benchmark-legible tasks at the expense of alternatives, and downstream users bear costs when benchmark performance fails to generalize. The moderate extractiveness reflects these structural effects while recognizing that the coordination problem the constraint solves remains live. The rising theater ratio over time reflects increasing optimization for benchmark performance rather than underlying capability, but has not yet crossed into piton territory where the coordination function would be mostly vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_latent_vs_constructed,
    'Is measured capability drawn from a fixed latent pool that benchmarks discover, or is capability actively constructed through optimization toward benchmark-legible competences?',
    'Longitudinal studies tracking capability distributions before and after benchmark shifts, measuring whether competences persist when no longer benchmarked or decay predictably (capability rust). If competences track funding and benchmark incentives rather than persisting independently, capability is constructed not discovered.',
    'If capability is constructed, then benchmark-driven forecasting commits a category error by treating measurement as discovery rather than cultivation. Timeline estimates would need to account for the endogeneity of capability to measurement standards. If capability is latent, current forecasting methodology is vindicated and endogeneity effects are second-order corrections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_latent_vs_constructed, empirical, 'Whether capability is latent property or measurement-dependent construction').

omega_variable(
    generality_partition_uniqueness,
    'Is there a unique correct partition that makes capability ''general'' rather than narrow, or are multiple incompatible partitions equally defensible?',
    'Philosophical analysis of whether ''general'' names a natural kind or a normative choice. Empirical test: if competing benchmark suites produce diverging capability orderings that do not converge with more data, partitions are non-unique. If orderings converge, a unique partition may exist.',
    'If partitions are non-unique, then disagreement about AGI timelines may structure into position-correlated camps that more measurement sharpens rather than dissolves. If a unique partition exists, disagreement is noise around a hidden fact and should converge with better measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generality_partition_uniqueness, conceptual, 'Whether generality admits unique operationalization or is irreducibly observer-dependent').

omega_variable(
    benchmark_shift_predictability,
    'Are benchmark shifts driven by scientific progress toward better measurement of fixed capability, or by strategic choices and funding dynamics that are not predictable from capability theory?',
    'Historical analysis of benchmark adoption patterns: do shifts follow theoretical advances in understanding capability, or do they follow funding availability, institutional politics, and competitive dynamics? If the latter, benchmark trajectories are not reliable guides to capability trajectories.',
    'If benchmark shifts are not predictable from capability theory, then extrapolating current benchmark trends to forecast capability timelines assumes stability of a socially constructed measurement standard rather than tracking an objective process. Forecasts would need to model benchmark evolution as endogenous to the AI development ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benchmark_shift_predictability, empirical, 'Whether benchmark evolution tracks scientific progress or institutional dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_endogeneity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capa_tr_t0, capability_endogeneity, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(capa_tr_t0, observed).
narrative_ontology:measurement(capa_tr_t5, capability_endogeneity, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(capa_tr_t5, observed).
narrative_ontology:measurement(capa_tr_t10, capability_endogeneity, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(capa_tr_t10, observed).
narrative_ontology:measurement(capa_tr_t15, capability_endogeneity, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(capa_tr_t15, observed).
narrative_ontology:measurement(capa_tr_t20, capability_endogeneity, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(capa_tr_t20, observed).
narrative_ontology:measurement(capa_tr_t25, capability_endogeneity, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(capa_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(capa_be_t0, capability_endogeneity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(capa_be_t0, observed).
narrative_ontology:measurement(capa_be_t5, capability_endogeneity, base_extractiveness, 5, 0.26).
narrative_ontology:measurement_basis(capa_be_t5, observed).
narrative_ontology:measurement(capa_be_t10, capability_endogeneity, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(capa_be_t10, observed).
narrative_ontology:measurement(capa_be_t15, capability_endogeneity, base_extractiveness, 15, 0.34).
narrative_ontology:measurement_basis(capa_be_t15, observed).
narrative_ontology:measurement(capa_be_t20, capability_endogeneity, base_extractiveness, 20, 0.36).
narrative_ontology:measurement_basis(capa_be_t20, observed).
narrative_ontology:measurement(capa_be_t25, capability_endogeneity, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(capa_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(capa_su_t0, capability_endogeneity, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(capa_su_t0, observed).
narrative_ontology:measurement(capa_su_t5, capability_endogeneity, suppression_requirement, 5, 0.32).
narrative_ontology:measurement_basis(capa_su_t5, observed).
narrative_ontology:measurement(capa_su_t10, capability_endogeneity, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(capa_su_t10, observed).
narrative_ontology:measurement(capa_su_t15, capability_endogeneity, suppression_requirement, 15, 0.39).
narrative_ontology:measurement_basis(capa_su_t15, observed).
narrative_ontology:measurement(capa_su_t20, capability_endogeneity, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(capa_su_t20, observed).
narrative_ontology:measurement(capa_su_t25, capability_endogeneity, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(capa_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_endogeneity, information_standard).
narrative_ontology:affects_constraint(capability_endogeneity, trajectory_extrapolation_reading).
narrative_ontology:affects_constraint(capability_endogeneity, generation_gate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the generality_standard kernel. The partition_choice_reading (this constraint) treats general capability as observer-dependent partition; trajectory_extrapolation_reading treats it as continuous scalar with fixed threshold; generation_gate_reading treats it as qualitative faculty gate. The readings have different ε values because they make different structural claims about what capability measurement does: partition_choice has moderate extraction from path dependency and capability atrophy; trajectory_extrapolation has higher extraction from timeline inflation and regulatory capture; generation_gate has low extraction because it deflates rather than inflates capability claims. All three are linked via network.affects_constraints because they compete for definitional authority over the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
