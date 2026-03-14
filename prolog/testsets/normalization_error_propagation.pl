% ============================================================================
% CONSTRAINT STORY: normalization_error_propagation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_normalization_error_propagation, []).

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
 *   constraint_id: normalization_error_propagation
 *   human_readable: Normalization Error Propagation in Data Processing Systems
 *   domain: computational/epistemology
 *
 * SUMMARY:
 *   Normalization error propagation represents a structural asymmetry in data
 *   processing systems where errors introduced during normalization
 *   (standardization, transformation, rescaling) accumulate through
 *   downstream analyses without transparent tracking or accountability. The
 *   constraint operates through institutional arrangements that treat
 *   normalization as a solved problem rather than a source of epistemic debt.
 *   Data controllers and analysts benefit from the speed and convenience of
 *   normalized pipelines; end users and research communities bear the cost of
 *   propagated errors. The constraint exhibits characteristics of both pure
 *   extraction (Snare) and mixed coordination-extraction (Tangled Rope)
 *   depending on the observer's structural position: normalization genuinely
 *   solves coordination problems across incompatible datasets, but the
 *   solution concentrates error opacity in the hands of those with
 *   normalization authority. Theater ratio (0.64) reflects that normalization
 *   procedures are ritualized as solving the problem—standardizing to
 *   mean-zero unit variance appears to control for dataset differences—when
 *   in fact the error accumulation remains invisible to downstream users.
 *
 * KEY AGENTS:
 *   - Data Consumer/End User: Primary victim (powerless/trapped) — cannot detect or audit error propagation; must trust downstream processing
 *   - Research Field/Statistical Integrity: Primary victim (moderate/constrained) — abstract collective good; bears cost of degraded signal fidelity; capacity exists to demand transparency but constrained by necessity of standardized pipelines
 *   - Data Controller/Normalization Authority: Primary beneficiary (institutional/arbitrage) — controls normalization choices; benefits from simplified data processing; can optimize for speed over error tracking
 *   - Transparency Coalition: Organized agents (organized/mobile) — advocates for error-tracking standards, reproducibility checks, uncertainty quantification; building alternative pathways with sunset logic
 *   - Legacy Normalization Ritual: Institutional system (institutional/arbitrage) — standardized procedures persist through inertia; theater persists because normalization appears to solve the problem
 *   - Mathematical Analysis: Civilizational view (analytical/analytical) — risks treating error propagation as natural law when it is actually a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(normalization_error_propagation, 0.52).
domain_priors:suppression_score(normalization_error_propagation, 0.68).
domain_priors:theater_ratio(normalization_error_propagation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(normalization_error_propagation, extractiveness, 0.52).
narrative_ontology:constraint_metric(normalization_error_propagation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(normalization_error_propagation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(normalization_error_propagation, tangled_rope).
narrative_ontology:human_readable(normalization_error_propagation, "Normalization Error Propagation in Data Processing Systems").
narrative_ontology:topic_domain(normalization_error_propagation, "computational/epistemology").

domain_priors:requires_active_enforcement(normalization_error_propagation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(normalization_error_propagation, initial_data_controller).
narrative_ontology:constraint_beneficiary(normalization_error_propagation, downstream_analysts_with_normalization_authority).
narrative_ontology:constraint_victim(normalization_error_propagation, end_users_of_derived_metrics).
narrative_ontology:constraint_victim(normalization_error_propagation, statistical_integrity_of_datasets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA CONSUMER (SNARE) — End users of normalized datasets cannot detect propagated errors; they must trust downstream processing. No capacity to verify normalization choices or audit error accumulation. Trapped in the epistemic chain with no exit mechanism. Bears full cost of uncorrected errors.
constraint_indexing:constraint_classification(normalization_error_propagation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD EPISTEMOLOGY (TANGLED ROPE) — The statistical integrity of a research domain depends on transparent normalization protocols, but normalization choices also enable coordination across incompatible datasets. Genuine coordination function (comparing across data sources) paired with asymmetric extraction (error costs borne by the field, benefits captured by those controlling normalization choices). Constrained by the necessity of standardized pipelines but capable of demanding protocol transparency.
constraint_indexing:constraint_classification(normalization_error_propagation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NORMALIZATION AUTHORITY (ROPE) — Data controller or processing center that establishes normalization standards. Benefits from coordination: standardized pipelines enable data integration and comparative analysis. Experiences the constraint as solving a collective action problem. Low extraction from this perspective — the coordination function is genuine.
constraint_indexing:constraint_classification(normalization_error_propagation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSPARENCY COALITION (SCAFFOLD) — Open science, data-sharing mandates, and error-audit requirements create alternative verification pathways (error margin disclosure, uncertainty quantification standards, reproducibility checks). These mechanisms have a sunset logic: as error-awareness and validation practices mature, the constraint's extractive force declines. Mobile exit exists via distributed verification.
constraint_indexing:constraint_classification(normalization_error_propagation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PROTOCOL (PITON) — Standardized normalization procedures (z-score centering, min-max scaling, etc.) persist through institutional inertia despite known limitations. The protocols function less effectively than their ritualistic invocation suggests; many analysts apply normalization without understanding error accumulation. Theater ratio high because the ritual of normalization appears to solve the problem, but error propagation remains untracked.
constraint_indexing:constraint_classification(normalization_error_propagation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical perspective, normalization error propagation appears as an immutable property of finite-precision arithmetic and stochastic processes. Any transformation of data introduces accumulated rounding errors and uncertainty quantification drift that cannot be eliminated, only bounded. This perspective risks naturalizing what is actually a contingent institutional arrangement (the choice to propagate untracked errors rather than maintain error bounds through the pipeline).
constraint_indexing:constraint_classification(normalization_error_propagation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(normalization_error_propagation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(normalization_error_propagation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(normalization_error_propagation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(normalization_error_propagation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(normalization_error_propagation, TR),
    TR >= 0.70.

:- end_tests(normalization_error_propagation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Normalization choices introduce bias and error accumulation that benefit the data controller (faster processing, simplified pipelines, authority over data interpretation) while imposing costs on end users (degraded signal fidelity, invisible error bounds, inability to assess result reliability). The extraction is not maximal (≥0.66) because genuine coordination benefits exist—normalization does enable comparison across incompatible datasets. The intermediate value reflects mixed function. Suppression (0.68): High. Multiple barriers prevent users from detecting or escaping normalization error: error bounds are not routinely computed or disclosed, alternatives to normalization are rarely available in standardized pipelines, technical knowledge to audit error propagation is concentrated in specialized roles, institutional convenience incentivizes opacity over transparency. Suppression is sufficient that even mobile agents (replication groups, independent analysts) find it hard to avoid propagated errors—the normalized data is what is available. Theater ratio (0.64): Moderate-high and increasing. Normalization appears to solve the problem of dataset comparability through standardization ritual. Analysts invoke normalization procedures without understanding error accumulation mechanisms. The ritual has increased in theater over time (0.45 → 0.64) as normalization has become more institutionalized and less questioned. Error-tracking alternatives reduce theater by making error propagation explicit.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap separates users and field epistemology (who see Snare/Tangled Rope) from normalization authorities and legacy systems (who see Rope/Piton). The data consumer sees pure extraction—errors they cannot detect or escape. The normalization authority sees pure coordination—solving the problem of comparing datasets. The field sees mixed function—genuine coordination benefit paired with asymmetric error costs. The transparency coalition sees a solvable temporary problem (Scaffold)—error-tracking standards can create alternative pathways with sunset logic. The legacy ritual sees itself as degraded (Piton)—performing normalization without understanding its error mechanisms. The analytical observer risks seeing immutable natural law (Mountain) when the constraint is actually maintained by institutional choice to favor speed over error transparency. This perspectival range demonstrates how the same structural mechanism (error accumulation during normalization) appears differently depending on the agent's power level, exit options, and structural position in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural position relative to the extraction flow. Data consumers and the field's statistical integrity are victims (d ≈ 0.90, high f(d)) with trapped/constrained exits—they experience maximum extraction through propagated errors they cannot audit or escape. The normalization authority is a beneficiary (d ≈ 0.10, low f(d)) with arbitrage options—they can choose whether to implement error-tracking, bearing minimal costs. The transparency coalition has mobile exit options (d ≈ 0.55, moderate f(d)) because they can demand protocol changes and have some capacity to implement alternatives. The mathematical perspective risks low d (0.05, assuming it as a beneficiary of 'natural law' framing) when actually it should be higher—the natural law framing itself is an extraction mechanism. The constraint's effective extraction (chi) is suppressed-but-real from the victim perspective (high d + high f(d) + scope scaling) and appears minimal from the beneficiary perspective (low d + low f(d)).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying that normalization serves genuine coordination (comparing across datasets) while simultaneously enabling extraction (error opacity favoring those with normalization authority). This is the canonical Tangled Rope structure: a legitimate coordination function paired with asymmetric extraction. The false summit risk appears in the analytical/natural law perspective—treating error propagation as an immutable mathematical property rather than as a contingent institutional choice to propagate errors without tracking bounds. The mountain classification would be justified if error accumulation were inherent to any data transformation and impossible to bound; it is actually preventable through error-tracking techniques that are computationally available but institutionally suppressed by convenience incentives. The scaffolding potential is real—error quantification standards, uncertainty propagation protocols, and reproducibility requirements can create alternative pathways. The piton classification is accurate—legacy normalization procedures persist through ritual and institutional inertia despite degraded function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    error_tracking_feasibility,
    'Is continuous error-bound propagation through complex data pipelines computationally and organizationally feasible at scale, or does the cost of tracking exceed the benefit?',
    'Empirical comparison of error-tracking overhead in production systems; cost-benefit analysis of interval arithmetic vs legacy normalization; organizational adoption rates of error quantification standards',
    'If feasible: constraint can shift to Scaffold with real sunset logic. If infeasible: suppression metric should rise to 0.75+ as error-tracking becomes structurally impossible, strengthening Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_tracking_feasibility, empirical, 'Whether error tracking through pipelines is feasible at scale').

omega_variable(
    normalization_necessity_vs_choice,
    'Is normalization a necessary step in data processing (imposed by mathematical properties) or a contingent choice driven by institutional convenience?',
    'Historical analysis of when normalization became standard practice; identification of domains that process unnormalized data successfully; comparison of problem-solving approaches in different eras',
    'If necessary: mountain classification is correct; constraint is inherent to data processing. If contingent: normalization is a Snare whose error costs are obscured by treating it as natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normalization_necessity_vs_choice, conceptual, 'Whether normalization is inherent or contingent').

omega_variable(
    error_attribution_ambiguity,
    'When a downstream analysis produces incorrect results, how much of the error is attributable to normalization choices vs. other factors (model selection, measurement noise, confounds)?',
    'Sensitivity analysis decomposing error sources; ablation studies isolating normalization contribution; meta-analysis of error-attribution practices in the field',
    'If normalization errors are < 10% of total variance: suppression and extractiveness metrics should be lower. If > 40%: both metrics should rise and Snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_attribution_ambiguity, empirical, 'Proportion of errors attributable to normalization').

omega_variable(
    institutional_incentive_asymmetry,
    'Do institutional incentives (publication speed, data access convenience, model performance metrics) systematically favor normalizing and propagating errors over slower error-tracking?',
    'Analysis of publication timelines and data access speeds with vs. without error-tracking; career advancement outcomes for researchers implementing transparency vs. institutional convenience; pressure from funding agencies on normalization standards',
    'If asymmetry exists: beneficiary (normalization authority) has structural advantage; constraint is Snare/Tangled Rope. If no asymmetry: constraint is Rope (coordination without extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_asymmetry, empirical, 'Whether institutional incentives favor error propagation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(normalization_error_propagation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(normerr_tr_t0, normalization_error_propagation, theater_ratio, 0, 0.45).
narrative_ontology:measurement(normerr_tr_t10, normalization_error_propagation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(normerr_tr_t20, normalization_error_propagation, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(normerr_be_t0, normalization_error_propagation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(normerr_be_t10, normalization_error_propagation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(normerr_be_t20, normalization_error_propagation, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(normalization_error_propagation, information_standard).
narrative_ontology:affects_constraint(normalization_error_propagation, data_pipeline_opacity).
narrative_ontology:affects_constraint(normalization_error_propagation, statistical_reproducibility_crisis).

% DUAL FORMULATION NOTE:
% Normalization error propagation is downstream of data quality and measurement noise but structurally distinct. Measurement error (upstream constraint) produces raw data; normalization choices (this constraint) introduce additional systematic error; downstream use (affected constraints) amplifies propagated errors. The decomposition reflects ε-invariance: measuring via 'error accumulation rate' yields ε≈0.52; measuring via 'user-perceived opacity' yields ε≈0.65; these are the same constraint. The intermediate value (0.52) reflects mixed coordination-extraction function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(normalization_error_propagation, analytical, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
