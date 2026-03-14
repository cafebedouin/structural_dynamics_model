% ============================================================================
% CONSTRAINT STORY: ecological_resilience_metric_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecological_resilience_metric_suppression, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ecological_resilience_metric_suppression
 *   human_readable: Ecological Resilience Metric Suppression
 *   domain: environmental/conservation_policy
 *
 * SUMMARY:
 *   Ecological resilience metric suppression represents a structural
 *   constraint where measurement and policy interact to enable resource
 *   extraction beyond sustainable thresholds. The constraint operates through
 *   selective adoption, deemphasis, or replacement of resilience metrics that
 *   would constrain development projects. Rather than overt denial of
 *   ecosystem decline, the suppression mechanism works through metric
 *   substitution: replacing adaptive-capacity metrics (which measure system
 *   capacity to absorb future disturbance) with productivity metrics (which
 *   measure current output). This shift enables short-term extraction while
 *   obscuring long-term degradation. The constraint exhibits all six DR types
 *   depending on observer position: ecosystem capacity perceives pure
 *   extraction (Snare), indigenous communities perceive colonial coercion
 *   (Snare), conservation science experiences mixed coordination and career
 *   pressure (Tangled Rope), extractive industries experience coordination
 *   (Rope), alternative metrics movements see a temporary institutional
 *   problem with sunset (Scaffold), legacy EIA frameworks persist as theater
 *   (Piton), and the civilizational view risks naturalizing contingent
 *   measurement choices as physical law (Mountain). The theater_ratio
 *   trajectory (0.42 → 0.68) reflects the increasing performativity of
 *   environmental review: despite expansion of assessment requirements, the
 *   capacity to prevent projects through metrics has declined as review
 *   processes have become more procedural and less outcome-determinative.
 *
 * KEY AGENTS:
 *   - Ecosystem Adaptive Capacity: Primary victim (powerless/trapped) — biophysical resilience cannot exit; suppressed metrics prevent early warning and adaptation
 *   - Indigenous Communities: Primary victim (powerless/trapped) — dependent on ecosystem stability; trapped in administrative frameworks that suppress traditional ecological knowledge
 *   - Conservation Scientists: Secondary actor (moderate/constrained) — constrained by funding and institutional pressure; benefit from coordination function but face career penalties for inconvenient findings
 *   - Extractive Industries & Development Finance: Primary beneficiary (institutional/arbitrage) — benefits from metric suppression enabling faster project deployment and reduced environmental constraints
 *   - Alternative Metrics Coalition: Organized opposition (organized/mobile) — building parallel frameworks with lower cost, enabling exit from suppression regime
 *   - Legacy Environmental Assessment: Institutional actor (institutional/constrained) — maintains EIA framework through inertia despite low functional verification capacity
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing measurement choices as biophysical law rather than political decision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecological_resilience_metric_suppression, 0.58).
domain_priors:suppression_score(ecological_resilience_metric_suppression, 0.65).
domain_priors:theater_ratio(ecological_resilience_metric_suppression, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecological_resilience_metric_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecological_resilience_metric_suppression, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ecological_resilience_metric_suppression, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecological_resilience_metric_suppression, tangled_rope).
narrative_ontology:human_readable(ecological_resilience_metric_suppression, "Ecological Resilience Metric Suppression").
narrative_ontology:topic_domain(ecological_resilience_metric_suppression, "environmental/conservation_policy").

domain_priors:requires_active_enforcement(ecological_resilience_metric_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecological_resilience_metric_suppression, extractive_industries).
narrative_ontology:constraint_beneficiary(ecological_resilience_metric_suppression, development_finance_institutions).
narrative_ontology:constraint_victim(ecological_resilience_metric_suppression, ecosystem_adaptive_capacity).
narrative_ontology:constraint_victim(ecological_resilience_metric_suppression, indigenous_communities).
narrative_ontology:constraint_victim(ecological_resilience_metric_suppression, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECOSYSTEM ADAPTIVE CAPACITY (SNARE) — The biophysical constraint cannot exit. As resilience metrics are suppressed or replaced with narrow productivity measures, ecosystems lose adaptive capacity to future perturbations. Trapped in measurement regimes that do not register irreversible loss until collapse occurs. Bears full extraction cost — degraded monitoring enables resource overharvest.
constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIGENOUS COMMUNITIES (SNARE) — Structurally dependent on ecosystem stability for subsistence and territorial rights. Trapped by colonial administrative frameworks that do not recognize traditional ecological knowledge or resilience metrics. When development projects proceed under suppressed metrics, communities have no formal recourse. Maximum extraction: loss of land, livelihoods, and cultural continuity.
constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSERVATION SCIENTISTS (TANGLED ROPE) — Constrained by funding dependence and institutional pressure to align with development narratives. But also benefit from coordination function: ecosystem monitoring, adaptive management protocols, and scientific collaboration create genuine coordination value. Moderate extraction — career incentives push toward suppression of inconvenient metrics, yet some agency exists through NGO funding and international science networks.
constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXTRACTIVE INDUSTRIES & DEVELOPMENT FINANCE (ROPE) — Net beneficiaries. Experiences the metric suppression as coordination: harmonizing impact assessment standards, streamlining environmental review, and establishing 'acceptable resilience thresholds' enable efficient project deployment. Low experienced extraction — benefits from arbitrage between different jurisdictional metric standards.
constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE METRICS COALITION (SCAFFOLD) — Organized agents (IUCN, biodiversity databases, citizen science networks) are building parallel resilience frameworks: functional diversity indices, landscape connectivity models, post-disturbance recovery rates. These alternatives have sunset logic: as biological monitoring becomes cheaper and more distributed, the traditional suppression mechanism (expensive centralized monitoring) loses force. Organized agents can exit toward cheaper, more comprehensive metrics.
constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY ENVIRONMENTAL ASSESSMENT (PITON) — Traditional Environmental Impact Assessment (EIA) procedures include resilience components, but these have become performative. EIAs are largely theater: baseline ecosystem surveys produce data that is rarely used for adaptive threshold-setting or post-project monitoring. The framework persists because alternatives haven't fully replaced it, not because resilience assessment works. Theater ratio reflects the gap between EIA protocols and actual adaptive management.
constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Civilizationally, ecosystems have finite resilience capacity — this is a constraint of biophysical reality, not a policy choice. Resilience decline is inevitable under sustained pressure; the only variable is whether decline is measured. From this view, metric suppression merely obscures an immutable physical boundary, making it a false summit: the constraint is not that resilience follows laws of thermodynamics, but that we choose measurement regimes that prevent seeing the boundary until it is crossed.
constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecological_resilience_metric_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ecological_resilience_metric_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecological_resilience_metric_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ecological_resilience_metric_suppression, TR),
    TR >= 0.70.

:- end_tests(ecological_resilience_metric_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The suppression enables extractive industries to avoid ecosystem constraints that would otherwise limit development. The extraction is not maximal because alternative metrics are emerging and some measurement transparency persists through scientific publications and NGO monitoring. Suppression (0.65): High. Institutional barriers to metric transparency include: discretionary interpretation of 'acceptable' resilience thresholds, focus on narrow economic valuation metrics, structural underfunding of long-term monitoring, legal/administrative procedures that exclude indigenous knowledge, and career incentives that reward industry-friendly impact assessments. Theater ratio (0.68): High. Environmental review procedures have expanded substantially (more assessment requirements, more documentation, more stakeholder consultation), but the procedural expansion has not translated to increased capacity to prevent or redirect projects. EIA outputs are rarely used for adaptive management or binding thresholds. The framework persists because alternatives haven't fully replaced it and because procedures serve legitimation functions for both regulators and industry.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (Rope) and victim (Snare) perspectives reveals the asymmetry: extractive industries experience the constraint as coordination — metrics are standardized, thresholds are harmonized, procedures enable project alignment. Trapped agents experience the same constraint as pure extraction — the standardization of metrics that ignore resilience enables overharvest without accountability. The Tangled Rope perspective (conservation science) reveals the mechanism: it is not accidental that metrics emphasizing productivity win out over metrics emphasizing adaptive capacity. Institutional pressure (funding allocation, publication expectations, career advancement) systematically rewards research that supports development narratives. The Scaffold perspective (alternative metrics coalition) reveals that the suppression is contingent — as monitoring costs decline and distributed data becomes abundant, the cost-based justification for suppressing resilience metrics loses force.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (extractive industries, development finance) have low directionality (d ≈ 0.15): they benefit from the constraint and have arbitrage options (can shift projects to less-regulated jurisdictions). This produces negative or minimal effective extraction from their perspective (Rope classification). Victims (ecosystem, indigenous communities) have high directionality (d ≈ 0.90): they bear costs from suppressed metrics without exit options (trapped). This produces maximum experienced extraction (Snare classification). Conservation scientists occupy middle ground (d ≈ 0.55): they are partially beneficiaries (funded research positions, coordination value) and partially victims (career pressure, suppressed results). This produces moderate extraction (Tangled Rope classification). The analytical observer has moderate-high directionality (d ≈ 0.72) because the observer position itself is not structurally embedded in the constraint system but can see all positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through empirical testing of the omega variables. The constraint is classified as Tangled Rope because genuine coordination exists (standardized metrics enable inter-project learning, monitoring infrastructure serves multiple stakeholders) alongside asymmetric extraction (metrics are systematically biased toward productivity over resilience). If the alternative metrics emerge and integration occurs (omega variable: indigenous_knowledge_integration), the constraint's extractive function declines and it may shift toward Rope or Scaffold. If ecosystem collapse occurs before metric integration (omega variable: suppression_causality confirms suppression causes degradation), the constraint will be reclassified retrospectively as Snare. The Piton classification of legacy EIA derives from the theater gate: assessment procedures expand in response to political pressure but do not constrain outcomes, indicating institutional inertia rather than functional verification. The mountain classification at civilizational scope is a false summit — the constraint is not that resilience follows physical law but that measurement choices are naturalized as unavoidable, which is a category error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_threshold_ambiguity,
    'What resilience metric threshold constitutes meaningful adaptive capacity vs. acceptable loss-of-function?',
    'Post-collapse analysis of ecosystems: retrospective identification of critical thresholds by comparing pre-collapse and post-collapse metrics; controlled experiments in simplified systems (microcosms, restored areas) to identify tipping points',
    'If threshold is high (ecosystem preserves 80%+ original function): many current projects exceed acceptable loss. If threshold is low (ecosystem resilient down to 20% function): current suppression is less damaging. Threshold uncertainty enables metric manipulation — beneficiaries justify suppression by setting thresholds high enough that current extraction appears acceptable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_threshold_ambiguity, empirical, 'Definition of critical resilience thresholds').

omega_variable(
    alternative_metric_equivalence,
    'Do alternative resilience metrics (functional diversity, connectivity indices, recovery rates) capture the same information as traditional biodiversity-based metrics, or do they measure complementary properties?',
    'Correlation analysis across metric frameworks; analysis of cases where metrics diverge to determine which predicts ecosystem vulnerability more accurately; longitudinal comparison of ecosystem trajectories predicted by different metric suites',
    'If equivalent: metric switching is costless — constraint is purely political/institutional. If complementary: suppressing any one metric loses information; robust resilience assessment requires multiple frameworks, raising measurement cost and reducing ability to justify suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_metric_equivalence, empirical, 'Whether alternative resilience metrics are equivalent or complementary').

omega_variable(
    suppression_causality,
    'Is metric suppression causing ecosystem degradation (suppressed metrics enable overharvest), or does ecosystem degradation cause metric suppression (beneficiaries suppress metrics post-facto to avoid accountability)?',
    'Temporal analysis of metric introduction/suppression relative to extraction intensity; jurisdictional comparison (areas with vs without metric suppression); analysis of metric changes preceding vs following development decisions',
    'If suppression causes degradation: removing suppression (metric transparency) prevents overharvest and enables earlier intervention. If causality is reverse: suppression is epiphenomenal — addressing extraction intensity directly is more effective than metric policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_causality, empirical, 'Whether metric suppression causes or follows ecosystem degradation').

omega_variable(
    indigenous_knowledge_integration,
    'Can traditional ecological knowledge (TEK) metrics integrate with scientific resilience frameworks, or does institutional structure prevent their recognition as valid evidence?',
    'Policy analysis of TEK recognition in environmental assessment; case studies comparing ecosystem outcomes in TEK-managed vs. scientific-metric-managed areas; analysis of institutional barriers to TEK integration (epistemological discrimination, legal barriers, resource allocation)',
    'If integration is possible: suppression mechanism is institutional capture, not scientific necessity — diversified metrics would include TEK. If integration is prevented structurally: this constraint exhibits cognitive/institutional colonialism alongside resource extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_knowledge_integration, conceptual, 'Whether indigenous ecological knowledge can integrate with scientific metrics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecological_resilience_metric_suppression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecores_tr_t0, ecological_resilience_metric_suppression, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ecores_tr_t10, ecological_resilience_metric_suppression, theater_ratio, 10, 0.58).
narrative_ontology:measurement(ecores_tr_t20, ecological_resilience_metric_suppression, theater_ratio, 20, 0.68).
narrative_ontology:measurement(ecores_tr_t5, ecological_resilience_metric_suppression, theater_ratio, 5, 0.5).

% Extraction over time
narrative_ontology:measurement(ecores_be_t0, ecological_resilience_metric_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ecores_be_t10, ecological_resilience_metric_suppression, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ecores_be_t20, ecological_resilience_metric_suppression, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ecores_be_t5, ecological_resilience_metric_suppression, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecological_resilience_metric_suppression, resource_allocation).
narrative_ontology:boltzmann_floor_override(ecological_resilience_metric_suppression, 0.18).
narrative_ontology:affects_constraint(ecological_resilience_metric_suppression, biodiversity_loss_measurement).
narrative_ontology:affects_constraint(ecological_resilience_metric_suppression, ecosystem_tipping_point_detection).
narrative_ontology:affects_constraint(ecological_resilience_metric_suppression, indigenous_land_rights_suppression).

% DUAL FORMULATION NOTE:
% Ecological resilience metric suppression is downstream of development financing institutions and extractive industry structures, and upstream of specific ecosystem collapse cases. The constraint represents a shared measurement framework that affects multiple domains (conservation, land rights, climate adaptation). Each downstream constraint inherits the metric suppression as a structural feature — inability to accurately measure resilience propagates uncertainty through the entire ecosystem assessment process.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecological_resilience_metric_suppression, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
