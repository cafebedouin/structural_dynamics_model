% ============================================================================
% CONSTRAINT STORY: biodiversity_collapse_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biodiversity_collapse_threshold, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biodiversity_collapse_threshold
 *   human_readable: Biodiversity Collapse Threshold
 *   domain: ecology/conservation_biology
 *
 * SUMMARY:
 *   The biodiversity collapse threshold represents a natural law constraint
 *   in the Deferential Realism framework. As species richness declines due to
 *   habitat loss, overexploitation, and climate change, ecological networks
 *   approach critical points beyond which cascading trophic collapses become
 *   inevitable and irreversible on human timescales. This constraint operates
 *   through ecological structure — the exponential dependence of network
 *   stability on species richness — rather than through institutional,
 *   economic, or political mechanisms. Once alpha diversity falls below
 *   critical thresholds (typically 30-50% of regional baseline in empirical
 *   studies), beta diversity (species turnover across space) and gamma
 *   diversity (total species richness) collapse in accelerating cascades. No
 *   amount of technological substitution, institutional coordination, or
 *   policy intervention can fully overcome the fundamental network dynamics
 *   that drive this constraint. The constraint exhibits zero degrees of
 *   freedom — it emerges naturally from the physics and mathematics of
 *   complex ecological networks and cannot be negotiated, extracted, or
 *   suspended.
 *
 * KEY AGENTS:
 *   - Species and Organisms: Primary trapped agents (powerless/trapped) — locked into dependencies on specific ecological relationships; face extinction once collapse cascades
 *   - Dependent Ecosystems: Primary victim (powerless/trapped) — mutualistic networks, pollination systems, nutrient cycling loops all collapse together
 *   - Agricultural and Resource-Dependent Communities: Secondary constrained agent (moderate/constrained) — cannot exit dependence on ecosystem services; face food security collapse
 *   - Conservation Institutions: Institutional observer (institutional/mobile) — possess resources and authority but encounter immutable natural limits; can slow approach to thresholds but not prevent collapse once triggered
 *   - Analytical Framework: Universal observer (analytical/analytical) — can measure the constraint but cannot change its underlying structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biodiversity_collapse_threshold, 0.12).
domain_priors:suppression_score(biodiversity_collapse_threshold, 0.03).
domain_priors:theater_ratio(biodiversity_collapse_threshold, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biodiversity_collapse_threshold, extractiveness, 0.12).
narrative_ontology:constraint_metric(biodiversity_collapse_threshold, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(biodiversity_collapse_threshold, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biodiversity_collapse_threshold, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(biodiversity_collapse_threshold, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biodiversity_collapse_threshold, mountain).
narrative_ontology:human_readable(biodiversity_collapse_threshold, "Biodiversity Collapse Threshold").
narrative_ontology:topic_domain(biodiversity_collapse_threshold, "ecology/conservation_biology").

domain_priors:emerges_naturally(biodiversity_collapse_threshold).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SPECIES AND ECOSYSTEMS (MOUNTAIN) — Biodiversity collapse is immutable from the perspective of organisms dependent on ecosystem services. Once trophic cascades begin at critical thresholds, the constraint becomes irreversible on human timescales. No exit exists for species locked into mutualistic dependencies or keystone species relationships.
constraint_indexing:constraint_classification(biodiversity_collapse_threshold, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AGRICULTURAL AND RESOURCE-DEPENDENT COMMUNITIES (MOUNTAIN) — Below critical biodiversity thresholds, pollinator collapse, soil microbial loss, and fishery crashes become structural constraints on food production. These are fixed physical limits — no amount of technological substitution fully replaces ecosystem services. The constraint appears unchangeable at generational timescales.
constraint_indexing:constraint_classification(biodiversity_collapse_threshold, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational/universal perspective, biodiversity collapse at specific thresholds is a natural law: once alpha diversity falls below critical levels, beta diversity and gamma diversity collapse in cascading fashion. The constraint emerges naturally from ecological structure — network stability decreases exponentially as species richness falls. No institutional workaround can fully overcome this immutable dynamic.
constraint_indexing:constraint_classification(biodiversity_collapse_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: CONSERVATION INSTITUTIONS (MOUNTAIN) — Even institutions with substantial resources and policy authority cannot prevent biodiversity collapse once threshold conditions are triggered. The constraint is perceived as an immutable natural limit — conservation efforts can slow the approach to critical thresholds but cannot reverse collapse once it occurs. The physical law (trophic cascade dynamics) is not negotiable.
constraint_indexing:constraint_classification(biodiversity_collapse_threshold, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biodiversity_collapse_threshold_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(biodiversity_collapse_threshold, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biodiversity_collapse_threshold, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(biodiversity_collapse_threshold, ExtMetricName, E),
    domain_priors:suppression_score(biodiversity_collapse_threshold, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(biodiversity_collapse_threshold),
    narrative_ontology:constraint_metric(biodiversity_collapse_threshold, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(biodiversity_collapse_threshold, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(biodiversity_collapse_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low, approaching the mountain floor. The constraint does not 'extract' in the institutional sense — it imposes pure structural limits. The metric reflects that the constraint's binding mechanism is not coercive overhead or institutional capture, but fundamental network mathematics. Suppression (0.03): Negligible. The constraint does not suppress alternatives through active enforcement — it operates through the inherent instability of low-diversity networks. Theater ratio (0.08): Minimal. The constraint exhibits almost no performative element. The physics of trophic cascades does not require theatrical legitimation — the collapse happens or it doesn't, regardless of narrative framing. Accessibility collapse (0.92): Very high. Once critical thresholds are crossed, alternative states (stable low-diversity ecosystems) are not accessible to the original high-diversity system. The constraint exhibits near-total accessibility collapse — the degrees of freedom available to the system collapse exponentially as it approaches criticality. Resistance (0.08): Very low. The constraint emerges naturally with minimal resistance from contingent factors. The underlying ecological mathematics is robust across system parameters — resistance would require changing fundamental properties of trophic network stability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap because it is classified as a mountain from all perspectives. Trapped organisms see immutable collapse. Constrained communities see immutable collapse. Institutional actors with substantial resources see immutable collapse. The analytical observer sees immutable collapse. This uniformity is diagnostic — it indicates a genuine natural law constraint rather than a negotiable institutional arrangement. The constraint's structure is invariant across observational position because the underlying mechanism is ecological physics, not social power. The lack of perspectival gap is itself the gold standard for mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation is required for mountain constraints. The constraint does not flow toward or away from beneficiaries and victims — it imposes structural limits that apply uniformly to all agents. There are no beneficiaries in the institutional sense; all agents face the same immutable constraint. This is distinct from a snare (which concentrates extraction on victims) or a rope (which coordinates among beneficiaries). The biodiversity collapse threshold is a pure structural limit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through complete uniformity of classification. All perspectives arrive at mountain, which demonstrates that the constraint's binding mechanism is not institutional extraction (which would vary by observer position) but natural law (which does not vary). The absence of perspectival disagreement indicates that the underlying mechanism is physical rather than social. If perspectives diverged (some seeing rope, others seeing snare), the constraint would be a false summit — a contingent institutional arrangement mistakenly naturalized. The uniformity across all six perspectives confirms the mountain classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_precision_ambiguity,
    'Where exactly do critical biodiversity collapse thresholds exist, and are they sharp discontinuities or gradual transitions?',
    'Empirical measurement of trophic network stability across species richness gradients; catastrophe theory analysis of ecological state spaces; paleoclimate analysis of rapid ecosystem shifts',
    'If thresholds are sharp (discontinuous): mountain classification holds — collapse is sudden and irreversible. If gradual: constraint might be rope-like at slow transition rates (continuous intervention possible). Current evidence suggests hybrid (critical slowing down precedes collapse).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_precision_ambiguity, empirical, 'Precision and character of biodiversity collapse thresholds').

omega_variable(
    ecosystem_substitutability,
    'Can technological substitution (vertical farming, synthetic pollination, cultured meat, desalination) reduce dependence on wild ecosystem services below critical thresholds?',
    'Long-term comparative analysis of synthetic vs ecosystem-derived services; scaling laws for substitution technology; failure-mode analysis of technology dependencies',
    'If highly substitutable: constraint may not be a mountain — human systems could maintain function below critical biodiversity thresholds. If poorly substitutable: mountain classification confirmed — collapse is non-negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_substitutability, empirical, 'Degree of technological substitutability for ecosystem services').

omega_variable(
    recovery_reversibility,
    'Are collapsed ecosystems genuinely irreversible, or can restoration accelerate recovery across critical thresholds?',
    'Paleoclimate reconstruction of ecosystem recovery timescales; experimental restoration of highly degraded systems; analysis of secondary succession dynamics in tropical/temperate systems',
    'If irreversible on human timescales (> 1000 years): mountain classification confirmed. If reversible within 50-100 years: constraint might be tangled_rope (extraction + coordination of restoration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_reversibility, empirical, 'Reversibility and timescale of ecosystem recovery').

omega_variable(
    anthropogenic_threshold_vs_natural_turnover,
    'Are current biodiversity loss rates and thresholds fundamentally different from natural extinction rates and ecosystem reorganization, or do they differ only in magnitude and speed?',
    'Comparative paleontology of extinction rates across geological timescales; analysis of extinction selectivity (preferential loss of apex predators vs random); modeling of ecosystem reassembly post-collapse',
    'If anthropogenic thresholds are qualitatively different (non-reversible, non-recoverable): mountain classification holds. If they are quantitative variations on natural processes: constraint might be more negotiable (rope-like with proper intervention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anthropogenic_threshold_vs_natural_turnover, conceptual, 'Distinction between anthropogenic and natural biodiversity thresholds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biodiversity_collapse_threshold, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biodiverse_tr_t0, biodiversity_collapse_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(biodiverse_tr_t25, biodiversity_collapse_threshold, theater_ratio, 25, 0.07).
narrative_ontology:measurement(biodiverse_tr_t50, biodiversity_collapse_threshold, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(biodiverse_be_t0, biodiversity_collapse_threshold, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(biodiverse_be_t25, biodiversity_collapse_threshold, base_extractiveness, 25, 0.1).
narrative_ontology:measurement(biodiverse_be_t50, biodiversity_collapse_threshold, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biodiversity_collapse_threshold, global_infrastructure).
narrative_ontology:affects_constraint(biodiversity_collapse_threshold, pollinator_decline_threshold).
narrative_ontology:affects_constraint(biodiversity_collapse_threshold, fishery_collapse_threshold).
narrative_ontology:affects_constraint(biodiversity_collapse_threshold, soil_microbial_loss_threshold).

% DUAL FORMULATION NOTE:
% The biodiversity collapse threshold is the overarching constraint affecting multiple domain-specific thresholds (pollinator decline, fishery collapse, soil health). Each domain-specific threshold has its own extractiveness value reflecting the particular causal mechanisms, but all are downstream of the fundamental principle that network stability degrades exponentially with species loss.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
