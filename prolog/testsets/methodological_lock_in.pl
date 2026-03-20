% ============================================================================
% CONSTRAINT STORY: methodological_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_methodological_lock_in, []).

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
 *   constraint_id: methodological_lock_in
 *   human_readable: Methodological Lock-In: Component Screening vs Systems Mechanistic Investigation
 *   domain: philosophy_of_science/systems_biology/epistemology
 *
 * SUMMARY:
 *   The methodological lock-in in systems biology represents a structural
 *   tension between the institutional and technological infrastructure for
 *   high-throughput component-level screening (GWAS, expression profiling,
 *   proteomics) and the epistemic requirements for mechanistic causal
 *   understanding. This constraint exhibits genuine coordination functions —
 *   standardized data formats, shared statistical frameworks, and
 *   collaborative infrastructure solve real collective action problems in
 *   large-scale biology. But it also exhibits asymmetric extraction: funding
 *   flows toward screening platforms and vendor ecosystems while mechanistic
 *   research programs face resource starvation; publication bias favors
 *   simple high-throughput findings over complex mechanistic investigations;
 *   and career structures reward data generation over causal inference. The
 *   constraint is actively enforced through funding priorities (NIH/NSF
 *   emphasis on 'omics' and big data), training pipelines (graduate programs
 *   in bioinformatics and computational biology focus on screening analysis),
 *   infrastructure investment (sequencing centers, biobanks, data
 *   repositories), and publication norms (high-impact journals prioritize
 *   large-scale association studies). The theater_ratio (0.58) reflects that
 *   peer review for component studies focuses on statistical compliance and
 *   sample size rather than biological mechanism or causal validity —
 *   reviewers cannot assess replication probability or mechanistic yield from
 *   summary statistics alone. The constraint is downstream of
 *   perturbation_epistemology (the mountain constraint that
 *   perturbation-based causal inference is epistemically superior to
 *   correlation-based screening for mechanism discovery) but exists as a
 *   separate institutional and economic structure.
 *
 * KEY AGENTS:
 *   - Mechanistic Biology Research Programs: Primary victim (powerless/trapped) — face funding cuts and lab closures as resources flow toward screening infrastructure; cannot exit paradigm without career abandonment
 *   - Systems Modeling Groups: Secondary victim (moderate/constrained) — must incorporate component-level data to remain fundable but gain computational substrate; mixed extraction experience
 *   - Sequencing Technology Vendors: Primary beneficiary (institutional/arbitrage) — capture revenue through equipment sales, consumables, and service contracts; methodological consensus creates stable demand
 *   - High-Throughput Screening Labs: Secondary beneficiary (institutional/mobile) — benefit from infrastructure investment and standardization; can pivot to adjacent methodologies
 *   - GWAS Research Consortia: Mixed actor (organized/constrained) — both benefit from coordination and perpetuate extraction through gatekeeping; constrained by sunk infrastructure investment
 *   - Causal Inference Research Community: Organized agents (organized/mobile) — building alternative pathways through interventional methods; see lock-in as temporary with sunset logic
 *   - Peer Review System: Institutional actor (institutional/constrained) — maintains performative review ritual focused on statistical compliance rather than mechanistic insight
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination and extraction; recognizes lock-in as contingent institutional arrangement rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(methodological_lock_in, 0.48).
domain_priors:suppression_score(methodological_lock_in, 0.62).
domain_priors:theater_ratio(methodological_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(methodological_lock_in, extractiveness, 0.48).
narrative_ontology:constraint_metric(methodological_lock_in, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(methodological_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(methodological_lock_in, tangled_rope).
narrative_ontology:human_readable(methodological_lock_in, "Methodological Lock-In: Component Screening vs Systems Mechanistic Investigation").
narrative_ontology:topic_domain(methodological_lock_in, "philosophy_of_science/systems_biology/epistemology").

domain_priors:requires_active_enforcement(methodological_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(methodological_lock_in, sequencing_technology_vendors).
narrative_ontology:constraint_beneficiary(methodological_lock_in, high_throughput_screening_labs).
narrative_ontology:constraint_beneficiary(methodological_lock_in, gwas_research_consortia).
narrative_ontology:constraint_victim(methodological_lock_in, mechanistic_biology_research_programs).
narrative_ontology:constraint_victim(methodological_lock_in, systems_modeling_groups).
narrative_ontology:constraint_victim(methodological_lock_in, causal_inference_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MECHANISTIC BIOLOGY RESEARCH PROGRAM (SNARE) — Trapped by funding structures that reward high-throughput component screening over labor-intensive mechanistic investigation. Cannot exit the methodological paradigm without abandoning career trajectory. Experiences maximum extraction: institutional resources flow toward screening infrastructure while mechanistic programs face closure or conversion.
constraint_indexing:constraint_classification(methodological_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEMS MODELING GROUP (TANGLED ROPE) — Constrained by need to incorporate GWAS/expression data to remain fundable, but also benefits from the data infrastructure for model parameterization. Experiences mixed extraction: must adopt component-level methods to access resources, yet gains computational substrate. Career costs of pure systems work are high but not insurmountable.
constraint_indexing:constraint_classification(methodological_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SEQUENCING TECHNOLOGY VENDOR (ROPE) — Primary beneficiary experiencing the constraint as coordination. The methodological consensus creates stable demand for sequencing platforms, reagents, and bioinformatics pipelines. Extraction flows toward this agent through equipment sales, consumables, and service contracts. Can arbitrage across research domains and geographies.
constraint_indexing:constraint_classification(methodological_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-THROUGHPUT SCREENING LAB (ROPE) — Institutional beneficiary with mobile exit options. Benefits from infrastructure investment and methodological standardization that enables cross-study comparison. Experiences the constraint as coordination: shared protocols and data formats solve genuine collective action problems in large-scale biology. Can pivot to adjacent screening methodologies if funding shifts.
constraint_indexing:constraint_classification(methodological_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: GWAS RESEARCH CONSORTIUM (TANGLED ROPE) — Organized institutional actor that both benefits from and perpetuates the lock-in. Coordination function is real: consortia solve data-sharing and statistical power problems. But extraction is also real: consortium membership becomes gatekeeping mechanism, and methodological standards suppress alternative approaches. Constrained exit because pivoting away from GWAS would strand sunk infrastructure investment.
constraint_indexing:constraint_classification(methodological_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CAUSAL INFERENCE RESEARCH COMMUNITY (SCAFFOLD) — Organized agents developing interventional and perturbation-based methods see the component-screening paradigm as temporary. New experimental techniques (CRISPR screens, optogenetics, single-cell perturbation) and computational methods (causal discovery algorithms, mechanistic AI) are building alternative pathways that bypass correlation-based screening. Sunset logic: as causal methods mature and costs decline, the screening paradigm's extraction mechanism loses force.
constraint_indexing:constraint_classification(methodological_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: PEER REVIEW SYSTEM (PITON) — The review process for high-throughput component studies has become largely performative. Reviewers assess statistical significance and sample size but cannot evaluate biological mechanism, causal validity, or replication probability from summary statistics alone. The ritual persists through institutional inertia: journals maintain impact factors by publishing high-citation screening studies despite low mechanistic yield. Theater ratio reflects that review focuses on methodological compliance rather than biological insight.
constraint_indexing:constraint_classification(methodological_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the methodological lock-in exhibits both genuine coordination (standardized data formats, shared infrastructure, statistical frameworks) and asymmetric extraction (suppression of mechanistic alternatives, vendor capture, career barriers). The constraint is not a natural law — alternative methodological paradigms are structurally possible and historically precedented. The lock-in is maintained through active enforcement: funding priorities, publication bias, infrastructure investment, and training pipeline design.
constraint_indexing:constraint_classification(methodological_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(methodological_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(methodological_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(methodological_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(methodological_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(methodological_lock_in, TR),
    TR >= 0.70.

:- end_tests(methodological_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from mechanistic research programs through funding reallocation, publication bias, and career barriers, while benefiting sequencing vendors and high-throughput labs. The extraction is substantial but not maximal — some mechanistic work persists, and hybrid approaches exist. The value reflects that the career and resource asymmetry is real but not total. Suppression (0.62): High. Significant barriers to mechanistic investigation include funding concentration in screening infrastructure, publication bias against negative or complex results, training pipeline emphasis on bioinformatics over experimental perturbation, and sunk cost lock-in from institutional investment in sequencing centers. But suppression is not total — causal inference methods are developing, and some funding agencies support mechanistic work. Theater ratio (0.58): Moderate-high. Peer review for component studies is substantially performative: reviewers assess statistical significance, sample size, and methodological compliance but cannot evaluate biological mechanism, causal validity, or replication probability from summary statistics. The review ritual persists because it maintains journal impact factors through high-citation screening studies, not because it ensures biological insight. The theater has increased over the interval as screening studies have grown in scale and complexity, outpacing reviewer capacity for mechanistic evaluation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the tangled rope pattern: different agents experience the same structural phenomenon as coordination, extraction, or both depending on their position. Sequencing vendors and screening labs see coordination (Rope) — the methodological consensus solves genuine problems and creates stable markets. Mechanistic programs see pure extraction (Snare) — resources flow away from them toward screening infrastructure with no exit path. Systems modeling groups see mixed coordination and extraction (Tangled Rope) — they must adopt component methods to survive but gain data infrastructure. GWAS consortia see coordination with gatekeeping (Tangled Rope) — they solve data-sharing problems but suppress alternatives. The causal inference community sees a temporary problem with a sunset (Scaffold) — interventional methods are building alternative pathways. The peer review system sees its own degraded ritual (Piton) — review persists through inertia despite low mechanistic yield. The analytical observer sees the full structure (Tangled Rope) — genuine coordination coexisting with asymmetric extraction, actively enforced through institutional mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect each agent's structural position relative to the extraction flow. Sequencing technology vendors are primary beneficiaries with arbitrage exit options — they experience negative effective extraction (the constraint subsidizes them through stable demand). High-throughput screening labs are secondary beneficiaries with mobile exit — they experience low extraction and can pivot if needed. GWAS consortia are mixed actors with constrained exit — they benefit from coordination but are locked in by sunk infrastructure investment, experiencing moderate extraction. Systems modeling groups are secondary victims with constrained exit — they must adopt component methods to access resources but gain computational substrate, experiencing moderate-high extraction. Mechanistic biology programs are primary victims with trapped exit — they face funding cuts and lab closures with no viable alternative, experiencing maximum extraction. The causal inference community has mobile exit and sees a sunset — they experience low extraction because they have agency and alternative pathways. The peer review system has constrained exit and maintains a degraded ritual — the piton classification derives from the theater gate rather than from high experienced extraction. The analytical observer recognizes the constraint as tangled rope — genuine coordination coexisting with asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that the methodological lock-in has BOTH a genuine coordination function (standardized data formats, shared statistical frameworks, collaborative infrastructure) AND asymmetric extraction (vendor capture, suppression of mechanistic alternatives, career barriers). The coordination is real: component-level screening solves collective action problems in large-scale biology, enables cross-study comparison, and generates hypothesis-generating associations. The extraction is also real: funding flows toward screening platforms while mechanistic programs face closure; publication bias favors simple high-throughput findings; and career structures reward data generation over causal inference. The constraint requires active enforcement through funding priorities, training pipelines, infrastructure investment, and publication norms — it does not emerge naturally from epistemic superiority alone. The tangled rope classification captures this duality: the constraint is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid where both functions coexist and are structurally inseparable at the current institutional configuration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanistic_yield_threshold,
    'What proportion of component-level associations must yield validated mechanisms before the screening paradigm is justified as discovery rather than extraction?',
    'Longitudinal tracking of GWAS hits and expression signatures: what fraction progress to mechanistic understanding within 10 years? Compare to historical rates for hypothesis-driven mechanistic studies.',
    'If yield < 5%: screening is primarily extractive (vendor revenue and publication volume with minimal biological insight). If yield > 20%: screening is legitimate discovery infrastructure with acceptable false positive rate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanistic_yield_threshold, empirical, 'Mechanistic validation rate threshold for screening paradigm justification').

omega_variable(
    infrastructure_sunk_cost,
    'How much of the methodological lock-in is driven by sunk infrastructure investment vs genuine epistemic superiority of component screening?',
    'Counterfactual analysis: if sequencing costs rose 10x or causal inference methods became 10x cheaper, would funding allocation shift? Survey data on researcher methodology preferences absent resource constraints.',
    'If primarily sunk cost: lock-in is extractive path dependency (Snare from more perspectives). If primarily epistemic: lock-in reflects genuine methodological advantage (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_sunk_cost, conceptual, 'Sunk cost vs epistemic advantage in methodology choice').

omega_variable(
    causal_method_maturity_timeline,
    'How long until interventional and perturbation-based causal methods achieve cost-effectiveness and throughput comparable to correlation-based screening?',
    'Technology roadmap analysis: CRISPR screen costs, optogenetic scalability, single-cell perturbation-seq economics. Extrapolate from historical cost curves for sequencing and synthesis.',
    'If < 5 years: scaffold perspective confirmed — sunset is imminent. If > 15 years: scaffold is aspirational — lock-in persists for another generation of researchers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_method_maturity_timeline, empirical, 'Timeline for causal method cost-effectiveness parity').

omega_variable(
    publication_bias_magnitude,
    'What proportion of mechanistic studies with negative or complex results remain unpublished due to bias toward simple high-throughput positive findings?',
    'Registered report comparison: publication rates for pre-registered mechanistic studies vs post-hoc screening studies. Survey of researchers on file-drawer effects by methodology type.',
    'If bias > 50%: suppression is severe and extraction is higher than measured. If bias < 20%: publication system is functioning and measured suppression is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publication_bias_magnitude, empirical, 'Publication bias magnitude for mechanistic vs screening studies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(methodological_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(method_lock_tr_t0, methodological_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(method_lock_tr_t5, methodological_lock_in, theater_ratio, 5, 0.48).
narrative_ontology:measurement(method_lock_tr_t10, methodological_lock_in, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(method_lock_be_t0, methodological_lock_in, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(method_lock_be_t5, methodological_lock_in, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(method_lock_be_t10, methodological_lock_in, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(methodological_lock_in, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of perturbation_epistemology (the mountain constraint that perturbation-based causal inference is epistemically superior to correlation-based screening for mechanism discovery). The upstream constraint establishes the epistemic ground truth; this constraint models the institutional and economic structure that persists despite that ground truth. The methodological lock-in has its own extractiveness value (0.48) reflecting the career incentive asymmetry and resource barriers, distinct from the upstream constraint's near-zero extractiveness (natural law status).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(methodological_lock_in, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
