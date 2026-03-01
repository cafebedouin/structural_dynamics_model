% ============================================================================
% CONSTRAINT STORY: structural_position_constraint_divergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_position_constraint_divergence, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_position_constraint_divergence
 *   human_readable: Structural Position Constraint Divergence
 *   domain: social_systems/institutional_dynamics/stratification_mechanics
 *
 * SUMMARY:
 *   Structural Position Constraint Divergence is the meta-constraint that the
 *   Deferential Realism framework itself describes: identical institutional
 *   mechanisms are experienced as coordination by privileged actors and
 *   extraction by marginalized actors based solely on power position. This is
 *   not a contingent feature of specific institutions but a mathematical
 *   property of indexical classification. The same base constraint properties
 *   (extractiveness ε, suppression σ) produce different effective extraction
 *   (χ) when filtered through different structural positions via the
 *   directionality function f(d). A constraint with ε=0.40 experienced by a
 *   beneficiary with arbitrage exit (d≈0.05, f(d)≈-0.12) yields χ<0 (Rope
 *   classification), while the same constraint experienced by a victim with
 *   trapped exit (d≈0.95, f(d)≈1.42) yields χ≈0.57 (Snare classification).
 *   The divergence is not a bug — it is the framework's central insight. This
 *   constraint story models the divergence itself as a Mountain: an
 *   unchangeable structural property of how power differentials interact with
 *   institutional mechanisms. The extractiveness is minimal (0.08) because
 *   the divergence does not itself extract — it describes the mathematical
 *   structure that enables other constraints to extract asymmetrically. The
 *   accessibility collapse is high (0.92) because no amount of institutional
 *   reform can eliminate the divergence without eliminating power
 *   differentials entirely, and resistance is low (0.08) because the
 *   divergence reasserts itself across all attempts to create 'neutral'
 *   institutions.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Meta-position (analytical/analytical) — sees the divergence as the framework's core mathematical property
 *   - Privileged Institution: Beneficiary position (institutional/arbitrage) — experiences constraints as coordination; sees divergence as natural
 *   - Marginalized Actor: Victim position (powerless/trapped) — experiences constraints as extraction; sees divergence as immutable within biographical time
 *   - Reform Coalition: Organized position (organized/constrained) — works to reduce specific extractions but recognizes divergence as structural invariant
 *   - Mobile Middle: Gradient navigator (moderate/mobile) — can change own position but cannot eliminate the divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_position_constraint_divergence, 0.08).
domain_priors:suppression_score(structural_position_constraint_divergence, 0.03).
domain_priors:theater_ratio(structural_position_constraint_divergence, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_position_constraint_divergence, extractiveness, 0.08).
narrative_ontology:constraint_metric(structural_position_constraint_divergence, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(structural_position_constraint_divergence, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_position_constraint_divergence, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(structural_position_constraint_divergence, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_position_constraint_divergence, mountain).
narrative_ontology:human_readable(structural_position_constraint_divergence, "Structural Position Constraint Divergence").
narrative_ontology:topic_domain(structural_position_constraint_divergence, "social_systems/institutional_dynamics/stratification_mechanics").

domain_priors:emerges_naturally(structural_position_constraint_divergence).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The structural fact that power position determines constraint experience is invariant across all social systems. This is not a contingent institutional arrangement but a mathematical property of how indexical classification works: the same base constraint properties (ε, suppression) produce different effective extraction (χ) when filtered through different power positions via the sigmoid directionality function f(d). The divergence IS the framework's core insight — it cannot be eliminated without eliminating power differentials themselves.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: PRIVILEGED INSTITUTION (MOUNTAIN) — From the beneficiary position, the divergence appears as an unchangeable feature of social organization. Institutions with arbitrage exit options and beneficiary status experience constraints as coordination mechanisms (low χ via low d) regardless of the constraint's base extractiveness. This perspective sees the divergence as natural law because changing it would require dismantling the power structure that defines the institution's position.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MARGINALIZED ACTOR (MOUNTAIN) — From the victim position with no exit, the divergence is experienced as an immutable constraint. Powerless agents with trapped exit options experience the same institutional mechanisms as extraction (high χ via high d). The actor cannot change their structural position within a biographical timeframe — the divergence is a mountain because the power differential that produces it is a mountain at this timescale.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: REFORM COALITION (MOUNTAIN) — Even organized agents working to reduce power differentials recognize the divergence as a structural property that persists across reform efforts. Changing specific institutional mechanisms (reducing ε or suppression) does not eliminate the divergence — it only shifts the magnitude. The divergence itself is invariant: as long as power differentials exist, the same constraint will be experienced differently by differently-positioned actors.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MOBILE MIDDLE (MOUNTAIN) — Actors with moderate power and mobile exit options experience the divergence as a gradient they can navigate but not eliminate. Moving from victim to beneficiary position (or vice versa) changes one's experience of constraints but does not change the fact that the divergence exists. The structural position determines the experience — this is perceived as unchangeable even by those who can change their own position.
constraint_indexing:constraint_classification(structural_position_constraint_divergence, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_position_constraint_divergence_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(structural_position_constraint_divergence, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_position_constraint_divergence, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(structural_position_constraint_divergence, ExtMetricName, E),
    domain_priors:suppression_score(structural_position_constraint_divergence, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(structural_position_constraint_divergence),
    narrative_ontology:constraint_metric(structural_position_constraint_divergence, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(structural_position_constraint_divergence, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(structural_position_constraint_divergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The divergence itself does not extract — it is the structural property that enables asymmetric extraction in other constraints. The small non-zero value reflects the cognitive and organizational cost of recognizing and accounting for perspectival differences. Suppression (0.03): Minimal. The divergence is not maintained by active enforcement — it emerges naturally from the interaction of power differentials and institutional mechanisms. Accessibility collapse (0.92): Very high. The divergence cannot be eliminated without eliminating power differentials entirely. All attempts to create 'neutral' institutions that treat all actors identically fail because the actors' different structural positions (power, exit options, relationship to extraction flow) cause them to experience the same mechanism differently. Resistance (0.08): Very low. The divergence reasserts itself across all institutional designs. Even explicitly egalitarian mechanisms produce divergent experiences when actors have different power positions. Theater ratio (0.15): Very low. The divergence is not performative — it is a real structural property with measurable consequences (Δχ across perspectives, classification type divergence).
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this constraint as Mountain, but the meaning of 'Mountain' differs across positions. The analytical observer sees a mathematical property of indexical classification — the divergence follows necessarily from the χ formula and the definition of structural position. The privileged institution sees natural law — the divergence is how social systems 'naturally' work (naturalizing their own privilege). The marginalized actor sees biographical immutability — the divergence is unchangeable within their lifetime because they cannot change their structural position. The reform coalition sees a structural invariant — the divergence persists across institutional reforms because it is upstream of any specific institution. The mobile middle sees a navigable but ineliminable gradient. The gap is not in classification but in ontology: is this a mathematical truth, a naturalized ideology, a biographical constraint, a structural limit, or a permanent feature of stratified systems? The framework's answer: all of these simultaneously, depending on the observer's position. The divergence is a Mountain from all perspectives, but the accessibility collapse has different sources (mathematical necessity, power entrenchment, biographical timescale, structural depth, gradient persistence).
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint is unusual in that it has no beneficiaries or victims in the conventional sense — it is a meta-constraint describing the mathematical structure that produces beneficiary/victim asymmetries in other constraints. All perspectives classify as Mountain because the divergence is invariant across power positions. The analytical observer sees it as a mathematical property of indexical classification. The privileged institution sees it as natural law (because changing it would require dismantling their privilege). The marginalized actor sees it as immutable (because they cannot change their structural position within biographical time). The reform coalition sees it as a structural invariant that persists across reform efforts. The mobile middle sees it as a gradient they can navigate but not eliminate. The uniformity of classification (all Mountain) is itself diagnostic: this is one of the rare constraints where the perspectival gap is not in classification type but in the interpretation of what the Mountain represents (mathematical necessity vs naturalized privilege vs biographical immutability vs structural invariant).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by being its own resolution. The mandatrophy asks: how do we distinguish coordination from extraction when the same mechanism appears as both depending on who you ask? The answer: you don't. The divergence is real. The privileged actor genuinely experiences coordination (low χ). The marginalized actor genuinely experiences extraction (high χ). Both are correct. The framework does not adjudicate between them — it models both perspectives and makes the divergence itself measurable (Δχ, classification type gap). This constraint story models the divergence as a Mountain to formalize that it is not a contingent feature of specific institutions but a structural property of how power differentials interact with constraints. The extractiveness is minimal because the divergence itself does not extract — it is the mathematical structure that enables asymmetric extraction in other constraints. The Mountain classification across all perspectives reflects that the divergence is invariant: it cannot be eliminated by changing observables, timeframes, or institutional designs without eliminating power differentials entirely. This is the framework's core insight formalized as a constraint story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_position_constraint_divergence, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_position_constraint_divergence, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is the meta-level formalization of the framework's indexical classification system. It does not decompose into multiple stories because it describes the mathematical structure that produces divergent classifications in other constraints. All other constraint stories in the corpus are downstream of this one — they instantiate the divergence that this story formalizes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
