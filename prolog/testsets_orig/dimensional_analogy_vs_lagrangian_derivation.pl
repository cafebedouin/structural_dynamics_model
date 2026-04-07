% ============================================================================
% CONSTRAINT STORY: dimensional_analogy_vs_lagrangian_derivation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dimensional_analogy_vs_lagrangian_derivation, []).

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
 *   constraint_id: dimensional_analogy_vs_lagrangian_derivation
 *   human_readable: Dimensional Analogy vs Lagrangian Derivation in NMCC Mass Prediction
 *   domain: theoretical_physics/high_energy_physics/cosmology
 *
 * SUMMARY:
 *   The dimensional analogy vs Lagrangian derivation constraint captures a
 *   fundamental methodological limit in theoretical physics: dimensional
 *   scaling arguments from one sector (electric charges in QED) to another
 *   (magnetic monopoles in a hypothetical magnetic sector) can suggest mass
 *   scales but cannot replace first-principles derivation from the magnetic
 *   sector Lagrangian. The specific case involves NMCC (non-minimal
 *   color-charged) particle mass predictions of 2.4 GeV and 4.4 TeV derived
 *   by scaling electric sector parameters rather than solving magnetic sector
 *   field equations. The constraint is that such dimensional analogies, while
 *   heuristically useful, do not constitute theoretical derivations — the
 *   binding calculation for alpha_m ~ 34 must come from QCD-analog dynamics,
 *   not from rescaling QED results. This is a mountain constraint because the
 *   gap between analogy and derivation is a logical feature of gauge field
 *   theory structure, not a contingent feature of institutional practice or
 *   resource availability. No change in funding, collaboration structure,
 *   computational resources, or career incentives alters the fact that
 *   dimensional scaling cannot replace solving the field equations.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a structural feature of gauge theory methodology
 *   - Theoretical Physics Community: Institutional perspective (institutional/analytical) — transmits the constraint as an unchangeable methodological standard
 *   - Phenomenologist: Individual researcher (powerful/mobile) — has resources and exit options but faces the same logical constraint
 *   - Graduate Student: Early-career researcher (moderate/constrained) — limited power and exit but perceives the constraint identically
 *   - Undergraduate Student: Pedagogical recipient (powerless/trapped) — no autonomy or exit; learns the constraint as a logical fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dimensional_analogy_vs_lagrangian_derivation, 0.08).
domain_priors:suppression_score(dimensional_analogy_vs_lagrangian_derivation, 0.02).
domain_priors:theater_ratio(dimensional_analogy_vs_lagrangian_derivation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, extractiveness, 0.08).
narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dimensional_analogy_vs_lagrangian_derivation, mountain).
narrative_ontology:human_readable(dimensional_analogy_vs_lagrangian_derivation, "Dimensional Analogy vs Lagrangian Derivation in NMCC Mass Prediction").
narrative_ontology:topic_domain(dimensional_analogy_vs_lagrangian_derivation, "theoretical_physics/high_energy_physics/cosmology").

domain_priors:emerges_naturally(dimensional_analogy_vs_lagrangian_derivation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The constraint that dimensional scaling arguments cannot replace Lagrangian first-principles derivation is a structural feature of theoretical physics methodology. The gap between analogy-based prediction and field-theoretic derivation reflects the logical structure of gauge theories, not a contingent institutional arrangement.
constraint_indexing:constraint_classification(dimensional_analogy_vs_lagrangian_derivation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THEORETICAL PHYSICS COMMUNITY (MOUNTAIN) — The requirement that mass predictions be derivable from Lagrangian first principles rather than dimensional analogy is perceived as an unchangeable methodological standard. No amount of institutional reorganization changes the logical gap between scaling arguments and field-theoretic calculation.
constraint_indexing:constraint_classification(dimensional_analogy_vs_lagrangian_derivation, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: PHENOMENOLOGIST (MOUNTAIN) — Individual researchers with career mobility and resources still face the same constraint: dimensional scaling from the electric sector (QED) to predict magnetic monopole masses does not constitute a derivation from magnetic sector dynamics. The methodological gap is independent of the researcher's power or exit options.
constraint_indexing:constraint_classification(dimensional_analogy_vs_lagrangian_derivation, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: GRADUATE STUDENT (MOUNTAIN) — Even agents with constrained exit options and limited power perceive the constraint identically: you cannot derive binding energies and mass spectra from dimensional analogy alone. The constraint is pedagogically transmitted as a logical requirement, not as an institutional norm that could be changed.
constraint_indexing:constraint_classification(dimensional_analogy_vs_lagrangian_derivation, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UNDERGRADUATE STUDENT (MOUNTAIN) — The most powerless agent in the system (no research autonomy, no exit options, local scope, immediate time horizon) still encounters the constraint as immutable: dimensional analysis gives you scaling relations, but binding energies require solving the field equations. This is taught as a logical fact, not a policy.
constraint_indexing:constraint_classification(dimensional_analogy_vs_lagrangian_derivation, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dimensional_analogy_vs_lagrangian_derivation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(dimensional_analogy_vs_lagrangian_derivation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dimensional_analogy_vs_lagrangian_derivation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, ExtMetricName, E),
    domain_priors:suppression_score(dimensional_analogy_vs_lagrangian_derivation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dimensional_analogy_vs_lagrangian_derivation),
    narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dimensional_analogy_vs_lagrangian_derivation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dimensional_analogy_vs_lagrangian_derivation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal extraction — it requires additional theoretical work (deriving from Lagrangian rather than scaling from analogy) but this is a methodological requirement inherent to the structure of gauge theories, not an artificial barrier. The small non-zero value reflects that dimensional analogy is computationally cheaper than full field-theoretic calculation, so the constraint does impose some additional labor cost. Suppression (0.02): Negligible. There are no suppressed alternatives — dimensional analogy and Lagrangian derivation are not competing methodologies where one is artificially blocked. They serve different epistemic functions (heuristic suggestion vs rigorous derivation). Theater ratio (0.15): Very low. The constraint is functional, not performative. The requirement for Lagrangian derivation serves the genuine epistemic purpose of ensuring predictions follow from the theory's dynamical structure rather than from dimensional coincidence. Accessibility collapse (0.92): Very high. The constraint is accessible to all trained physicists — the distinction between dimensional scaling and field-theoretic derivation is taught in graduate coursework and is not hidden behind specialized knowledge barriers. Resistance (0.08): Very low. Attempts to bypass the constraint (claiming dimensional analogy constitutes derivation) are immediately identified as methodological errors by the community. The constraint is robust against institutional or social pressure.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for this constraint — all five perspectives classify as mountain. The undergraduate student with no power, no exit options, and immediate time horizon perceives the constraint identically to the analytical observer with civilizational time horizon and universal scope. This uniformity is the diagnostic signature of a genuine mountain: the constraint is invariant across all indexical positions because it reflects a logical structure (the gap between dimensional scaling and field-theoretic derivation) rather than a social arrangement. The constraint is pedagogically transmitted, computationally verified, and methodologically enforced, but these transmission mechanisms do not constitute the constraint — they recognize it. The constraint would exist even if no human had yet discovered gauge field theory; it is a feature of the mathematical structure itself.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims — it is a logical feature of gauge field theory structure. All agents, regardless of power level or exit options, experience the constraint identically: dimensional scaling from the electric sector cannot replace Lagrangian derivation from the magnetic sector. There is no extraction flow because there is no asymmetric distribution of costs or benefits. The constraint applies uniformly to all theoretical work in this domain. Directionality values are not applicable — the constraint is not a relationship between agents but a relationship between methodological approaches (analogy vs derivation) that all agents face equally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mountain classification at its purest: a logical limit that appears identical from all perspectives and cannot be bypassed by any change in institutional arrangements, resource allocation, or social organization. The mandatrophy resolution is straightforward — there is no risk of mislabeling coordination as extraction or vice versa because there is no coordination function and no extraction. The constraint is not solving a collective action problem (no coordination), not distributing costs asymmetrically (no extraction), not maintained by enforcement (emerges from logical structure), and not performative (serves genuine epistemic function). The dimensional analogy vs Lagrangian derivation gap is a mountain because it is a structural feature of how gauge theories work: binding energies and mass spectra depend on the dynamics encoded in the Lagrangian, and dimensional scaling from a different sector cannot capture those dynamics. This is not a claim about current theoretical tools or computational limits — even with infinite computational resources, dimensional analogy would not constitute derivation, because the two approaches answer different questions (what scale? vs what dynamics?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dimensional_analogy_vs_lagrangian_derivation, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dimensional_analogy_vs_lagrangian_derivation, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a methodological mountain that applies to any dimensional scaling argument across gauge theory sectors. It is not specific to NMCC mass prediction but is instantiated in that case. Other constraints in the same family would include dimensional scaling for any hypothetical particle sector (e.g., dark photon mass from visible photon scaling, axion mass from pion scaling). Each such case would be a separate constraint story with its own empirical observables, but all would share the same mountain classification for the methodological gap between analogy and derivation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
