% ============================================================================
% CONSTRAINT STORY: thermodynamics_entropy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thermodynamics_entropy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: thermodynamics_entropy
 *   human_readable: The Second Law of Thermodynamics (Entropy Increase)
 *   domain: physics/thermodynamics/natural_law
 *
 * SUMMARY:
 *   The Second Law of Thermodynamics stands as a canonical natural law
 *   constraint — an irreducible physical limit that applies uniformly to all
 *   agents, all power levels, all time horizons, and all spatial scopes. No
 *   institution enforces it, no coordination mechanism created it, no
 *   beneficiary group benefits from it disproportionately, and no victim
 *   group bears its cost asymmetrically. The law states that entropy in an
 *   isolated system must increase or remain constant; it cannot spontaneously
 *   decrease. This constraint is simultaneously the most universal and the
 *   most impersonal: it applies whether you are powerless or institutional,
 *   immediate or civilizational, local or universal. The Second Law
 *   structures the energetic possibility space within which all other
 *   constraints operate. No institution can negotiate its terms, no
 *   technology can circumvent it, and no observer can perceive a loophole
 *   through different measurement. The extractiveness value (0.12) reflects
 *   the minimal definitional 'cost' of the law's existence — the universal
 *   requirement that all systems must dissipate energy — but this is not
 *   extraction in the DR sense (asymmetric value transfer). Rather, it is the
 *   baseline resource cost of thermodynamic existence itself. The theater
 *   ratio (0.08) indicates negligible performative content: the law is
 *   measured directly through calorimetry, entropy calculations, and
 *   temperature observations, with almost no interpretive mediation or
 *   institutional ritual.
 *
 * KEY AGENTS:
 *   - Living Systems: Universal agent (all power levels/exit options) — subject to entropy accumulation through metabolism, reproduction, and death. No exit.
 *   - Engineers & Technologists: Moderate to institutional agents (constrained/arbitrage exit) — optimize efficiency within Carnot bounds but cannot violate the law.
 *   - Physicists & Theorists: Analytical agents (analytical/analytical exit) — understand the mathematical foundations and combinatorial inevitability of entropy increase.
 *   - Energy Industries: Institutional agent (institutional/arbitrage exit) — operate within thermodynamic limits; institutional power does not exempt the constraint.
 *   - The Physical Universe: Non-agent participant — entropy increase is the constraint's structural basis; the universe is the enforcement mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thermodynamics_entropy, 0.12).
domain_priors:suppression_score(thermodynamics_entropy, 0.02).
domain_priors:theater_ratio(thermodynamics_entropy, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thermodynamics_entropy, extractiveness, 0.12).
narrative_ontology:constraint_metric(thermodynamics_entropy, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(thermodynamics_entropy, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thermodynamics_entropy, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thermodynamics_entropy, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thermodynamics_entropy, mountain).
narrative_ontology:human_readable(thermodynamics_entropy, "The Second Law of Thermodynamics (Entropy Increase)").
narrative_ontology:topic_domain(thermodynamics_entropy, "physics/thermodynamics/natural_law").

domain_priors:emerges_naturally(thermodynamics_entropy).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIVING SYSTEM (MOUNTAIN) — All living organisms are embedded in entropy increase. Metabolic processes require constant energy dissipation; growth, maintenance, and reproduction all generate waste heat and disorder. No organism can exit the constraint or negotiate with thermodynamic law. The second law is an immutable boundary of existence itself.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEER (MOUNTAIN) — Can optimize efficiency and slow entropy increase locally, but cannot violate the law. Every engine, refrigerator, and heat pump must accept that some energy becomes waste heat. The constraint is unchangeable even with power and resources. Engineering victory is minimizing, not escaping, entropy production.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: PHYSICIST (MOUNTAIN) — The second law emerges from combinatorics of microstates and the ergodic hypothesis. Entropy increase is a statistical inevitability, not a mechanism that could be designed differently. The law is independent of how we measure it, what units we use, or which observer evaluates it. Universal, immutable, foundational.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ENERGY INDUSTRY (MOUNTAIN) — Even the most powerful institutional actors cannot exceed Carnot efficiency limits or eliminate waste heat. Fossil fuels, nuclear reactors, solar collectors — all obey the second law. Institutional power does not exempt the constraint; it only funds better engineering within thermodynamic bounds.
constraint_indexing:constraint_classification(thermodynamics_entropy, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermodynamics_entropy_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamics_entropy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thermodynamics_entropy, ExtMetricName, E),
    domain_priors:suppression_score(thermodynamics_entropy, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thermodynamics_entropy),
    narrative_ontology:constraint_metric(thermodynamics_entropy, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thermodynamics_entropy, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thermodynamics_entropy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The second law imposes no asymmetric value extraction. All agents — powerless and institutional alike — are subject to identical thermodynamic limits. The value 0.12 reflects only the baseline resource dissipation required by the law's existence, not extraction of benefit from one group to another. Suppression (0.02): Negligible. There are no alternatives to suppress, no escape routes to close off, no coordination mechanisms to override. Agents cannot even imagine violating the law — it is not suppressed alternative but impossible alternative. Theater ratio (0.08): Minimal. The law is not maintained through ritual or institutional performance. Entropy increase is measured directly through calorimetry and statistical mechanics. Scientific practice involves experimental verification, but this is hypothesis-testing, not theater. The small residual value reflects the necessary interpretive work in boundary definitions and thermodynamic system specification, but interpretation is not performative. Accessibility collapse (0.92): Very high. No agent has access to a state in which the second law does not apply. All possible configurations obey it. The collapse is asymptotic to 1.0. Resistance (0.08): Very low. No actor or coalition actively resists the second law's application. Resistance is not zero because some historical scientists (perpetual-motion researchers) attempted to deny it, and some contemporary fringe thinkers still contest Maxwell's demon arguments. But organized, sustained resistance is negligible. The law's authority is not actively defended because no meaningful challenge exists.
 *
 * PERSPECTIVAL GAP:
 *   Minimal to absent. This is the diagnostic feature of a genuine mountain constraint: all perspectives produce mountain classification. Power, time horizon, exit options, and spatial scope do not change the classification because the law is immutable regardless of observer position. A powerless organism and an institutional energy company experience identical thermodynamic constraints. An engineer operating on immediate timeframes and a physicist thinking on civilizational timeframes both obey the same Carnot limits. Local subsystems and universal systems are both governed by entropy increase (once system boundaries are clarified). The absence of perspectival gap is evidence of the constraint's universality and immutability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is undefined for a genuine mountain constraint because there is no asymmetric extraction flow. No agent benefits disproportionately from the second law; no agent bears its cost disproportionately. The law applies universally and symmetrically. The f(d) sigmoid evaluation is non-applicable because d has no meaningful value. All agents experience the constraint's force directly, without mediation through institutional structures or power differentials. This universality is precisely what distinguishes the second law from extractive constraints that benefit some agents at others' expense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    time_asymmetry_grounding,
    'Is entropy increase a fundamental asymmetry of time or an emergent statistical property from symmetric microscopic laws?',
    'Foundational physics: debate between fundamental second law (time asymmetry at base level) vs emergent second law (microstates symmetric, entropy increase emerges from initial conditions). T-symmetry violation vs coarse-graining perspectives.',
    'Philosophical only — does not affect classification. Both interpretations produce mountain classification. Affects narrative framing of ''why'' the law exists, not ''whether'' it is immutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(time_asymmetry_grounding, conceptual, 'Whether entropy increase reflects fundamental time asymmetry or statistical emergence').

omega_variable(
    maxwell_demon_logical_status,
    'Do closed-system theoretical arguments against Maxwell''s demon establish the second law''s inviolability, or do they only establish practical impossibility?',
    'Formal analysis of information-theoretic demon constructions; examination of whether information-theoretic cost arguments prove logical inevitability or only practical infeasibility. Szilard vs Landauer frameworks.',
    'Does not change classification. Whether the second law is logically required or only practically universal, both produce mountain. But affects confidence in universality scope: purely practical bound is narrower than logical bound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maxwell_demon_logical_status, conceptual, 'Logical vs practical status of second-law inviolability').

omega_variable(
    open_system_boundary_ambiguity,
    'For a subsystem, entropy can decrease if we account for heat flow to the environment. Does this make the second law observer-relative (depends on system boundary choice) rather than universal?',
    'Clarification: total entropy (system + environment) always increases. Local decrease is always paired with larger increase elsewhere. The law is universal if we define ''system'' to include all relevant degrees of freedom. If observer selects a subsystem boundary, they must account for environment.',
    'Does not change classification. The second law''s universality is preserved by always considering the full system. Observer-relative subsystem entropy is not observer-relative universality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_system_boundary_ambiguity, conceptual, 'Whether open-system entropy decrease contradicts universality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thermodynamics_entropy, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thermo_tr_t0, thermodynamics_entropy, theater_ratio, 0, 0.08).
narrative_ontology:measurement(thermo_tr_t100, thermodynamics_entropy, theater_ratio, 100, 0.08).
narrative_ontology:measurement(thermo_tr_t1000, thermodynamics_entropy, theater_ratio, 1000, 0.08).

% Extraction over time
narrative_ontology:measurement(thermo_be_t0, thermodynamics_entropy, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(thermo_be_t100, thermodynamics_entropy, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(thermo_be_t1000, thermodynamics_entropy, base_extractiveness, 1000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thermodynamics_entropy, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
