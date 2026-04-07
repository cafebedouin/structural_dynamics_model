% ============================================================================
% CONSTRAINT STORY: metabolic_scaling_exponent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metabolic_scaling_exponent, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: metabolic_scaling_exponent
 *   human_readable: Metabolic Scaling Exponent Constraint in Biology
 *   domain: biological_physics/metabolic_theory
 *
 * SUMMARY:
 *   The metabolic scaling exponent constraint operates within the
 *   intersection of comparative physiology, evolutionary biology, and
 *   biophysical theory. A central claim in metabolic ecology holds that basal
 *   metabolic rate scales with body mass according to a universal exponent
 *   near 0.75 (three-quarters power law), derived from geometric constraints
 *   on resource distribution networks and thermodynamic efficiency. However,
 *   empirical studies consistently report scaling exponents ranging from 0.67
 *   to 1.0 across taxa, ecological contexts, and measurement protocols. This
 *   constraint exhibits mixed classification: unified scaling theory
 *   advocates view the 0.75 exponent as a coordination mechanism enabling
 *   cross-taxa comparison (Rope); empirical researchers studying metabolic
 *   diversity experience suppression of their observations as exceptions that
 *   must be explained away (Snare); moderate research groups face career
 *   barriers to publishing heterodox exponents (Tangled Rope); mechanistic
 *   alternatives are building consensus around context-dependent scaling
 *   (Scaffold); traditional comparative physiology literature treats the
 *   universal exponent as an inherited theoretical framework disconnected
 *   from predictive precision (Piton); and civilizational-scale analysis
 *   risks naturalizing what is actually a contingent theoretical choice as a
 *   law of biology (Mountain). The constraint's theater ratio (0.65) reflects
 *   that many scaling law papers prioritize theoretical elegance and
 *   mathematical closure over empirical precision or mechanistic fidelity at
 *   organism complexity. The extractiveness (0.38) is moderate: the unified
 *   exponent doctrine provides genuine coordination benefits (enabling
 *   comparative work and funding justification) while simultaneously
 *   extracting from empirical diversity (suppressing alternative mechanisms
 *   and narrowing field prediction capacity).
 *
 * KEY AGENTS:
 *   - Unified Scaling Theory Community: Primary beneficiary (institutional/arbitrage) — captures citation dominance, theoretical synthesis, and funding narrative coherence through universal exponent consensus
 *   - Empirical Metabolic Diversity: Primary victim (powerless/trapped) — full range of observed exponents suppressed through theoretical forcing; cannot exit the constraint imposed by unified frameworks
 *   - Empirical Research Groups: Secondary victim (moderate/constrained) — face career barriers and citation penalties for publishing non-standard exponents; also benefit from theoretical framework structure
 *   - Metabolic Diversity Coalition: Organized actors (organized/constrained) — multi-scale physiology researchers, open-data initiatives building alternative context-dependent frameworks
 *   - Comparative Scaling Literature: Institutional archive (institutional/arbitrage) — maintains performative tradition of scaling law papers with mathematical elegance decoupled from predictive precision
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent theoretical consensus as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metabolic_scaling_exponent, 0.38).
domain_priors:suppression_score(metabolic_scaling_exponent, 0.48).
domain_priors:theater_ratio(metabolic_scaling_exponent, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metabolic_scaling_exponent, extractiveness, 0.38).
narrative_ontology:constraint_metric(metabolic_scaling_exponent, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(metabolic_scaling_exponent, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metabolic_scaling_exponent, tangled_rope).
narrative_ontology:human_readable(metabolic_scaling_exponent, "Metabolic Scaling Exponent Constraint in Biology").
narrative_ontology:topic_domain(metabolic_scaling_exponent, "biological_physics/metabolic_theory").

domain_priors:requires_active_enforcement(metabolic_scaling_exponent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(metabolic_scaling_exponent, dynamic_energy_budget_framework).
narrative_ontology:constraint_beneficiary(metabolic_scaling_exponent, unified_metabolic_theory_advocates).
narrative_ontology:constraint_victim(metabolic_scaling_exponent, empirical_metabolic_diversity).
narrative_ontology:constraint_victim(metabolic_scaling_exponent, alternative_mechanistic_hypotheses).
narrative_ontology:constraint_victim(metabolic_scaling_exponent, field_prediction_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL METABOLIC DIVERSITY (SNARE) — The full range of observed metabolic scaling exponents (0.67–1.0 across taxa) cannot exit the constraint imposed by unified scaling theories that demand a single universal exponent near 0.75. Empirical exceptions are systematically suppressed through measurement protocol disputes, statistical reanalysis, and theoretical dismissal. The trapped agent bears the full cost of theoretical forcing.
constraint_indexing:constraint_classification(metabolic_scaling_exponent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMPIRICAL RESEARCH GROUPS (TANGLED ROPE) — Constrained by dominant theoretical frameworks and citation impacts but also benefit from the unified scaling narrative that provides interpretive structure and funding justification. Groups studying metabolic diversity have agency but face career barriers to heterodox results. Mixed extraction and coordination.
constraint_indexing:constraint_classification(metabolic_scaling_exponent, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIFIED SCALING THEORY COMMUNITY (ROPE) — Benefits from the theoretical consensus. Views scaling exponents as a coordination mechanism: shared framework enables comparative biology, cross-taxa prediction, and mechanistic understanding. The exponent unifies disparate observations into coherent theory. Net beneficiary experiencing coordination benefits.
constraint_indexing:constraint_classification(metabolic_scaling_exponent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: METABOLIC DIVERSITY COALITION (SCAFFOLD) — Organized agents (multi-scale physiology societies, open-data initiatives, heterodox mechanistic researchers) are building alternative frameworks emphasizing scaling diversity rather than universal exponents. Sunset clause: as mechanistic models at organism-level complexity mature, the need for universal scaling laws diminishes. Temporary constraint with declining enforcement.
constraint_indexing:constraint_classification(metabolic_scaling_exponent, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPARATIVE SCALING LITERATURE (PITON) — The metabolic level-design literature (DEB models, metabolic-level hypotheses) is substantially performative. Theoretical elegance and mathematical closure have become goals in themselves, decoupled from predictive precision or mechanistic fidelity. The constraint persists through institutional inertia and citation cascades, not because it produces the best empirical predictions. Theater ratio reflects this performative drift.
constraint_indexing:constraint_classification(metabolic_scaling_exponent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, metabolic scaling may reflect fundamental physical constraints on resource distribution networks and fractal-like supply systems. This perspective sees a universal exponent as an immutable consequence of network geometry and thermodynamic limits. However, empirical exponent variation (0.67–1.0) contradicts universality — the false summit detection reveals that naturalization of the 0.75 consensus conceals contingent theoretical choices.
constraint_indexing:constraint_classification(metabolic_scaling_exponent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metabolic_scaling_exponent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metabolic_scaling_exponent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metabolic_scaling_exponent, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(metabolic_scaling_exponent, TR),
    TR >= 0.70.

:- end_tests(metabolic_scaling_exponent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The unified exponent provides genuine coordination benefits — it enables cross-taxa comparison, predicts metabolic rates with moderate accuracy, and simplifies ecological models. However, it achieves this coordination partly through suppression of empirical diversity and alternative mechanisms. The value reflects moderate extraction: the coordination function is real, but so is the cost to alternative hypotheses. Suppression (0.48): Moderate-high barriers to publishing non-standard exponents exist through publication bias, theoretical gatekeeping by reviewers trained in unified scaling, and citation cascades that privilege consensus papers. However, suppression is not total — heterodox research is published, though with lower impact and slower accumulation. Theater ratio (0.65): Rising over the interval. Modern metabolic papers often emphasize mathematical sophistication and theoretical coherence more than empirical precision or mechanistic fidelity. The scaling law literature has become increasingly performative as papers generate variants and refinements of universal exponent arguments disconnected from new empirical data.
 *
 * PERSPECTIVAL GAP:
 *   The unified scaling perspective sees coordination (Rope) — a shared framework enabling prediction and comparison across 11 orders of magnitude in body mass. The empirical diversity perspective sees extraction (Snare) — their observations are systematically marginalized and reinterpreted as exceptions. Research groups see mixed coordination and suppression (Tangled Rope) — the framework enables their work but constrains what results they can publish. The diversity coalition sees a temporary consensus that open-science mechanistic models are rendering obsolete (Scaffold) — organism-level complexity models reduce the need for universal exponents. The literature sees its own degraded ritual (Piton) — mathematical sophistication has become an end in itself. The civilizational observer risks seeing natural law (Mountain) — fractal networks and thermodynamic limits seem to require 0.75. But the perspectival gap reveals the false summit: empirical exponent diversity (0.67–1.0) directly contradicts universality.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to extraction and coordination flows. The unified theory community experiences low d (high beneficiary status, arbitrage exit) — they benefit from consensus and can pivoting to new frameworks if needed. Empirical diversity experiences high d (victim status, trapped exit) — it cannot articulate alternatives within the dominant discourse and bears the suppression cost. Research groups experience moderate d (constrained exit) — they can publish alternatives at career cost but not without penalty. The organized diversity coalition experiences lower d (organized exit, access to alternative publishing and funding) than trapped agents. The literature itself experiences near-zero d (institutional arbitrage) — it benefits from consensus and can selectively cite heterodox work without changing canonical narrative. The analytical observer risks d ≈ 0.7 (naturalizing a contingent choice as immutable, partially falling for the false summit) unless applying cross-perspectival analysis to detect the error.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival decomposition. The question 'Is there a universal metabolic scaling exponent?' is not answered by a single type but by the presheaf of classifications across observer positions. The unified theory sees coordination (Rope, valid perspective); empirical diversity sees extraction (Snare, equally valid); the analytical observer risks seeing nature law (Mountain, but false). The mandatrophy resolves by recognizing that all three are describing the same structural phenomenon from different epistemological positions. The universal exponent IS a coordination mechanism for comparative biology (Rope classification valid). It ALSO suppresses empirical diversity that cannot be incorporated into the universal framework (Snare classification valid). It is NOT a law of physics despite its mathematical elegance (Mountain classification false). The 0.75 consensus exists not because nature requires it but because the unified scaling framework has become institutionally entrenched while maintaining genuine predictive utility for mid-range body masses. Mandatrophy resolution: accept multi-perspectival validity while rejecting false universality claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exponent_mechanistic_basis,
    'Is the observed variation in metabolic scaling exponents (0.67–1.0) due to genuine mechanistic differences in resource distribution networks across taxa, or does it reflect measurement error, allometric confounds, and statistical artifacts?',
    'High-resolution metabolic measurements across body size ranges within single taxa; mechanistic modeling of resource networks at organism scale; cross-validation of exponent estimates using phylogenetically independent methods',
    'If genuine mechanistic diversity: exponent is not universal, constraint classification shifts to Rope/Scaffold for all perspectives. If artifact: unified exponent is justified, constraint becomes Mountain-like (natural physical limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exponent_mechanistic_basis, empirical, 'Whether scaling exponent variation reflects genuine mechanism or measurement artifact').

omega_variable(
    network_geometry_universality,
    'Do space-filling resource distribution networks (fractal models) actually produce a universal 2/3 or 3/4 exponent, or does network topology vary sufficiently across taxa to permit multiple stable exponents?',
    'Computational modeling of alternative network topologies (branching angles, terminal segment properties); empirical characterization of vascular and respiratory network geometry across phyla; testing whether exponent predictions from network models match observed values',
    'If universal: theoretical prediction justified, mountain-like classification recovered. If topology-dependent: exponent is contingent on organism architecture, constraint is Rope/Tangled Rope coordination of diverse mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_geometry_universality, empirical, 'Whether fractal network geometry produces universal metabolic exponent').

omega_variable(
    suppression_mechanism_empirical,
    'Is suppression of alternative exponent values driven by statistical publication bias (journals preferring papers confirming 0.75), theoretical gatekeeping (reviewers dismissing non-universal claims), or genuine empirical implausibility of alternatives?',
    'Meta-analysis of published exponent estimates and their confidence intervals; analysis of rejected/unpublished datasets via data repositories; examination of citation patterns for papers reporting non-standard exponents; reanalysis of historical raw data with modern statistical methods',
    'If publication bias/gatekeeping: suppression is social, not empirical — constraint is Snare/Tangled Rope. If genuine implausibility: suppression is justified by evidence, constraint approaches Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_empirical, empirical, 'Whether suppression reflects bias or genuine empirical constraint').

omega_variable(
    scale_dependency_threshold,
    'Below what organism body mass does the unified scaling exponent lose predictive validity? At what scale does mechanistic heterogeneity dominate?',
    'Systematic metabolic measurements across organism sizes from unicells to megafauna; piecewise regression identifying transitions in scaling relationship; mechanistic modeling at organism and ecosystem scales',
    'If sharp threshold exists: exponent is valid above threshold, constraint is contextual Scaffold. If no threshold: exponent claims universality it cannot support, constraint approaches Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_dependency_threshold, empirical, 'Scale-dependent validity threshold for metabolic scaling exponent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metabolic_scaling_exponent, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mse_tr_t0, metabolic_scaling_exponent, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mse_tr_t15, metabolic_scaling_exponent, theater_ratio, 15, 0.58).
narrative_ontology:measurement(mse_tr_t30, metabolic_scaling_exponent, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(mse_be_t0, metabolic_scaling_exponent, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mse_be_t15, metabolic_scaling_exponent, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(mse_be_t30, metabolic_scaling_exponent, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metabolic_scaling_exponent, information_standard).
narrative_ontology:affects_constraint(metabolic_scaling_exponent, metabolic_ecology_prediction_reliability).
narrative_ontology:affects_constraint(metabolic_scaling_exponent, organism_complexity_mechanistic_modeling).

% DUAL FORMULATION NOTE:
% The metabolic scaling exponent constraint is upstream of specific metabolic ecology predictions (food web structure, ecosystem productivity) but represents a distinct structural constraint on theoretical framework selection. Decomposition: universal_exponent_coordination (ε ≈ 0.15, Rope) handles the genuine cross-taxa comparison benefit; scaling_diversity_suppression (ε ≈ 0.55, Snare) handles the empirical cost. This story integrates both via Tangled Rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
