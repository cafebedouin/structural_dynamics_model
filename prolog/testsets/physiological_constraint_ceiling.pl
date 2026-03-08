% ============================================================================
% CONSTRAINT STORY: physiological_constraint_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_physiological_constraint_ceiling, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: physiological_constraint_ceiling
 *   human_readable: Physiological Constraint Ceiling in Craft Labor
 *   domain: labor_systems/knowledge_transmission/physical_limits
 *
 * SUMMARY:
 *   The physiological constraint ceiling in craft labor represents a pure
 *   mountain: an irreducible physical limit that binds independent of social,
 *   economic, or institutional arrangements. The constraint manifests in
 *   occupations requiring sustained respiratory effort — glassblowing, brass
 *   instrument performance, traditional bellows operation, certain textile
 *   processes. The observable is lung capacity decline measured in sustained
 *   blow time (seconds of continuous exhalation at functional pressure),
 *   cough frequency, and hemoptysis (blood in sputum). The narrative arc
 *   follows a craftsperson whose sustained blow time declines from 22 seconds
 *   (journeyman standard) to 14 seconds (below functional threshold for
 *   complex work) over a 15-20 year period due to progressive interstitial
 *   lung disease. No amount of skill accumulation, economic incentive, or
 *   institutional support can restore alveolar gas exchange capacity once
 *   fibrotic remodeling or emphysematous destruction has occurred. The
 *   constraint is a mountain from all perspectives because it reflects the
 *   physics of gas diffusion and the irreversibility of certain forms of lung
 *   tissue damage.
 *
 * KEY AGENTS:
 *   - Declining Craftsperson: Powerless/trapped — experiences the ceiling as immediate and absolute; no exit from biology
 *   - Guild Master: Moderate/constrained — observes the pattern biographically; can adjust training and role assignments but cannot prevent the decline
 *   - Industrial Medicine Institution: Institutional/arbitrage — can measure and document but not reverse; enables occupational exit but not physiological restoration
 *   - Labor Union: Organized/mobile — can negotiate safety and compensation but cannot alter the underlying capacity limit
 *   - Analytical Observer: Analytical/analytical — recognizes the constraint as universal physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(physiological_constraint_ceiling, 0.08).
domain_priors:suppression_score(physiological_constraint_ceiling, 0.02).
domain_priors:theater_ratio(physiological_constraint_ceiling, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(physiological_constraint_ceiling, extractiveness, 0.08).
narrative_ontology:constraint_metric(physiological_constraint_ceiling, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(physiological_constraint_ceiling, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(physiological_constraint_ceiling, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(physiological_constraint_ceiling, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(physiological_constraint_ceiling, mountain).
narrative_ontology:human_readable(physiological_constraint_ceiling, "Physiological Constraint Ceiling in Craft Labor").
narrative_ontology:topic_domain(physiological_constraint_ceiling, "labor_systems/knowledge_transmission/physical_limits").

domain_priors:emerges_naturally(physiological_constraint_ceiling).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DECLINING CRAFTSPERSON (MOUNTAIN) — Experiences the lung capacity decline as an immutable physical barrier. No amount of skill, effort, or institutional intervention can restore sustained blow time once alveolar damage progresses. The constraint is immediate and local but absolute within that scope.
constraint_indexing:constraint_classification(physiological_constraint_ceiling, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE GUILD MASTER (MOUNTAIN) — Observes the pattern across multiple craftspeople over decades. Recognizes that no training modification, workshop ventilation improvement, or economic incentive can prevent the physiological ceiling from eventually binding. The constraint is biographical in scope but structurally invariant.
constraint_indexing:constraint_classification(physiological_constraint_ceiling, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE INDUSTRIAL MEDICINE INSTITUTION (MOUNTAIN) — Documents the constraint across industries and generations. Can measure the decline trajectory (spirometry, diffusion capacity, arterial blood gas) but cannot reverse the underlying pathology. Institutional resources enable exit from specific occupations but not from the physiological limit itself.
constraint_indexing:constraint_classification(physiological_constraint_ceiling, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — Recognizes the constraint as a universal physical law: alveolar gas exchange capacity is bounded by surface area, diffusion distance, and perfusion matching. Once fibrotic scarring or emphysematous destruction reduces functional lung volume, no social arrangement can restore it. The constraint is a mountain from all perspectives because it reflects irreducible biology.
constraint_indexing:constraint_classification(physiological_constraint_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE LABOR UNION (MOUNTAIN) — Can organize for workplace safety, respiratory protection, early retirement provisions, and disability compensation, but cannot organize away the physiological decline itself. The union's power is real but orthogonal to the constraint — they can mitigate consequences but not alter the underlying capacity ceiling.
constraint_indexing:constraint_classification(physiological_constraint_ceiling, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(physiological_constraint_ceiling_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(physiological_constraint_ceiling, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(physiological_constraint_ceiling, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(physiological_constraint_ceiling, ExtMetricName, E),
    domain_priors:suppression_score(physiological_constraint_ceiling, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(physiological_constraint_ceiling),
    narrative_ontology:constraint_metric(physiological_constraint_ceiling, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(physiological_constraint_ceiling, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(physiological_constraint_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint extracts only the minimal metabolic cost of breathing against damaged lungs — there is no asymmetric transfer of value between agents. The craftsperson loses capacity, but no one gains it. The small non-zero value reflects that the constraint does impose a cost (reduced productive capacity, career truncation) but this cost is not extraction in the DR sense — it is pure loss, not transfer. Suppression (0.02): Negligible. The constraint does not suppress alternatives through coercion or information asymmetry. Alternative occupations exist; the constraint only binds within the specific domain requiring sustained respiratory effort. The craftsperson can exit the occupation (though not without cost). Accessibility collapse (0.92): Very high. Once the physiological decline begins, the space of accessible interventions collapses rapidly. No training, equipment, or institutional arrangement can restore alveolar surface area. Resistance (0.08): Very low. The constraint is not maintained by active enforcement or institutional inertia — it persists because lung tissue damage is largely irreversible given current medical technology. Theater ratio (0.05): Negligible. There is no performative component — the constraint is measured directly via spirometry, diffusion capacity testing, and arterial blood gas analysis. The observable (sustained blow time) is a functional measure with no theatrical overlay.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap in classification — all five perspectives classify as mountain. The gap that does exist is in scope and time horizon: the craftsperson experiences the constraint as immediate and local (their own lungs, right now), while the analytical observer recognizes it as universal and civilizational (a law of biology that applies to all humans across all time). But both perspectives agree on the classification: this is an immutable limit. The uniformity across perspectives is the diagnostic signature of a true mountain — when powerless and institutional, trapped and arbitrage, immediate and civilizational all converge on the same type, the constraint is genuinely invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no beneficiaries or victims in the structural sense because it involves no transfer of value between agents. The declining craftsperson bears a cost, but no agent captures a corresponding benefit. Directionality is not applicable — the constraint is a pure loss function, not an extraction mechanism. All perspectives derive their d values from the canonical fallback for their power atoms, but since ε is very low (0.08), even the powerless/trapped perspective experiences minimal effective extraction. The constraint is a mountain not because extraction is distributed equally, but because there is no extraction to distribute.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints involve extraction or coordination between agents. Some constraints are simply physical limits that bind independent of social structure. The mountain classification is not a naturalization of contingent arrangements (false summit) but a recognition of genuine physical law. The constraint could be confused with extractive labor systems (the craftsperson's declining capacity might be attributed to exploitative working conditions), but the structural data disambiguates: the decline trajectory is independent of wage level, working hours, or power relations. A craftsperson in a worker cooperative with excellent ventilation and a craftsperson in a coercive workshop both face the same physiological ceiling once lung damage occurs. The constraint is a mountain because it reflects biology, not because it has been naturalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(physiological_constraint_ceiling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(physiological_constraint_ceiling, information_standard).
narrative_ontology:boltzmann_floor_override(physiological_constraint_ceiling, 0.01).

% DUAL FORMULATION NOTE:
% This constraint is structurally distinct from occupational lung disease constraints (silicosis, asbestosis, byssinosis) which involve exposure-response relationships and preventable pathology. The physiological constraint ceiling applies even when exposure is eliminated — it represents the irreversible endpoint of lung damage, not the damage accumulation process itself. A separate constraint story would be needed for the extractive labor conditions that cause the lung damage; this story models only the capacity ceiling once damage exists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
