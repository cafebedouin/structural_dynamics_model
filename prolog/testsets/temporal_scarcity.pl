% ============================================================================
% CONSTRAINT STORY: temporal_scarcity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_temporal_scarcity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temporal_scarcity
 *   human_readable: The "Scoop Economy" in Digital Media
 *   domain: technological/social
 *
 * SUMMARY:
 *   The structural requirement in digital media to publish immediately creates
 *   a scarcity of synthesis and verification. Truth-seeking requires time for
 *   cross-referencing and reflection, but the professional medium rewards
 *   velocity to capture algorithmic attention. This results in a constraint
 *   that functions as a Piton: the speed once served to inform the public
 *   quickly (a Scaffold), but now primarily serves platform engagement metrics
 *   while degrading the quality of information, maintained by institutional
 *   inertia and the theatrical performance of "breaking news."
 *
 * KEY AGENTS (by structural relationship):
 *   - News Consumers: Primary target (powerless/trapped) — bear the cost of low-quality information.
 *   - Digital Platforms: Primary beneficiary (institutional/arbitrage) — benefit from high-velocity engagement.
 *   - Media Outlets: Secondary actor (institutional/constrained) — participate in the system to remain relevant, seeing it as a necessary coordination tool.
 *   - Analytical Observer: Sees the full structure of atrophied function and theatrical maintenance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% A Piton has low functional extraction but high theatricality.
% The extraction (ε=0.15) represents the small amount of actual value transfer
% (attention for low-grade info), while the high theater (0.72) reflects the
% performative "breaking news" cycle.
domain_priors:base_extractiveness(temporal_scarcity, 0.15).
domain_priors:suppression_score(temporal_scarcity, 0.35).
domain_priors:theater_ratio(temporal_scarcity, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_scarcity, extractiveness, 0.15).
narrative_ontology:constraint_metric(temporal_scarcity, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(temporal_scarcity, theater_ratio, 0.72).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(temporal_scarcity, piton).
narrative_ontology:human_readable(temporal_scarcity, "The \"Scoop Economy\" in Digital Media").

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(temporal_scarcity, digital_platforms).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(temporal_scarcity, news_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (PITON)
% News consumers are trapped in an information ecosystem where speed is
% prioritized over depth. The constraint is not coercive enough to be a Snare,
% but is an inertial, low-value structure they cannot easily exit.
% χ = 0.15 * f(d≈0.95) * σ(1.0) ≈ 0.15 * 1.42 = 0.213. This is <= 0.25.
% With theater > 0.7, this classifies as Piton.
constraint_indexing:constraint_classification(temporal_scarcity, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE MEDIA OUTLET (ROPE)
% Media outlets are constrained to participate to remain relevant. For them,
% the system is a coordination mechanism (a Rope) for competing in the
% attention economy. They perceive the extraction as low because it's a
% cost of doing business, and they overlook the theatrical decay.
% χ is very low, classifying as Rope.
constraint_indexing:constraint_classification(temporal_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The observer sees the high theatricality (breaking news banners, constant
% updates) and the atrophied function (low synthesis, high error rate).
% The core purpose has decayed, leaving only inertial performance.
% χ = 0.15 * f(d≈0.72) * σ(1.2) ≈ 0.15 * 1.15 * 1.2 = 0.207. This is <= 0.25.
% With theater > 0.7, this classifies as Piton.
constraint_indexing:constraint_classification(temporal_scarcity, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_scarcity_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the media outlet (sees Rope) and the
    % analytical observer (sees Piton).
    constraint_indexing:constraint_classification(temporal_scarcity, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(temporal_scarcity, piton, context(agent_power(analytical), _, _, _)).

test(piton_threshold_validation) :-
    % Verify the theater ratio meets the Piton threshold.
    domain_priors:theater_ratio(temporal_scarcity, TR),
    config:param(piton_theater_floor, Floor),
    TR >= Floor.

:- end_tests(temporal_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics are chosen to model a classic Piton: a constraint whose
 *   original function has atrophied, leaving behind performative rituals.
 *   Base extractiveness (ε=0.15) is low, reflecting the minimal actual value
 *   provided by high-velocity, low-synthesis news. The key metric is the
 *   high theater_ratio (0.72), representing the "breaking news" cycle that
 *   is more about performance than substantive information delivery.
 *   Suppression (0.35) is moderate, reflecting institutional inertia rather
 *   than active coercion.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the institutional participants (media outlets) and
 *   the targets/observers. Media outlets, trapped in a competitive dynamic,
 *   view the constraint as a necessary coordination mechanism (Rope) for
 *   survival and relevance. They focus on the functional aspect of competing
 *   for attention. In contrast, news consumers (powerless) and analytical
 *   observers see the decayed output and high theatricality, correctly
 *   identifying it as a Piton.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `digital_platforms` gain engagement and ad revenue from
 *     the high churn of content, regardless of its depth.
 *   - Victims: `news_consumers` bear the cost through a degraded information
 *     environment, spending attention for low-quality signal. Their status as
 *     victims with trapped exit options drives their high directionality `d`,
 *     but the low base extraction `ε` results in a Piton, not a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This case shows how a potential Scaffold (rapidly informing the public
 *   in crises) can decay into a Piton when its function is subsumed by
 *   metrics (engagement, speed) that reward theatricality over substance.
 *   The high theater_ratio is the primary diagnostic tool that prevents
 *   misclassifying this inertial system as a functional Rope or Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_temporal_scarcity,
    'Was the "scoop economy" ever a functional Scaffold, or was it always a Piton driven by competitive theater?',
    'Historical analysis of news quality and public understanding during the transition from print to digital media, comparing periods of technological change.',
    'If it was a functional Scaffold, its decay into a Piton is a story of institutional failure. If it was always a Piton, it reveals a fundamental incompatibility between deep synthesis and ad-driven media models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_temporal_scarcity, empirical, 'Uncertainty about the historical function (Scaffold vs. Piton) of rewarding publication speed.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_scarcity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is low (< 0.46), so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is coupled with systems that create theatrical neutrality.
narrative_ontology:affects_constraint(temporal_scarcity, theatrical_neutrality_snare).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */