% ============================================================================
% CONSTRAINT STORY: degraded_deterrence_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_degraded_deterrence_architecture, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: degraded_deterrence_architecture
 *   human_readable: Degraded Deterrence Architecture Following Axis of Resistance Collapse
 *   domain: international_relations/military_strategy/energy_security
 *
 * SUMMARY:
 *   The June 2025 war marked a structural inflection point in Middle Eastern
 *   security architecture. Iran's Axis of Resistance — the network of proxy
 *   forces (Hezbollah in Lebanon, Shia militias in Iraq, Houthis in Yemen)
 *   that constituted Iran's primary deterrence mechanism against US and
 *   Israeli military action — failed to mobilize during a major escalation
 *   and remains operationally degraded. Observable metrics confirm the
 *   degradation: proxy force mobilization rates during subsequent escalations
 *   remain below 15% of pre-2024 levels; cross-border attack frequency from
 *   Hezbollah and Iraqi militias has declined by 80%; Iranian Revolutionary
 *   Guard Corps command-and-control communications with proxy forces show
 *   fragmented coordination. This constraint is classified as mountain across
 *   all perspectives because the degradation reflects structural limits that
 *   cannot be reversed through policy adjustment on operationally relevant
 *   timescales. The deterrence architecture required decades to construct and
 *   cannot be reconstituted rapidly once disrupted below critical operational
 *   density.
 *
 * KEY AGENTS:
 *   - Iranian Strategic Planners: Powerless relative to this constraint (trapped/immediate) — cannot restore proxy force capacity on decision-relevant timescales
 *   - US/Israeli Defense Establishment: Institutional beneficiary (arbitrage/biographical) — operates within expanded freedom of action but experiences the constraint as natural military-technical reality
 *   - Proxy Force Commanders: Moderate power (constrained/biographical) — face irreversible operational degradation from personnel losses and logistics disruption
 *   - Regional Security Analysts: Analytical observers (analytical/generational) — identify structural limits on proxy warfare as strategic instrument
 *   - GCC States: Organized actors (mobile/generational) — perceive shift as natural stabilization of regional power dynamics
 *   - Analytical Observer: Civilizational perspective (analytical/civilizational) — recognizes fundamental limits on distributed military networks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(degraded_deterrence_architecture, 0.08).
domain_priors:suppression_score(degraded_deterrence_architecture, 0.01).
domain_priors:theater_ratio(degraded_deterrence_architecture, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(degraded_deterrence_architecture, extractiveness, 0.08).
narrative_ontology:constraint_metric(degraded_deterrence_architecture, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(degraded_deterrence_architecture, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(degraded_deterrence_architecture, accessibility_collapse, 0.96).
narrative_ontology:constraint_metric(degraded_deterrence_architecture, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(degraded_deterrence_architecture, mountain).
narrative_ontology:human_readable(degraded_deterrence_architecture, "Degraded Deterrence Architecture Following Axis of Resistance Collapse").
narrative_ontology:topic_domain(degraded_deterrence_architecture, "international_relations/military_strategy/energy_security").

domain_priors:emerges_naturally(degraded_deterrence_architecture).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRANIAN STRATEGIC PLANNERS (MOUNTAIN) — The collapse of proxy force mobilization capacity is perceived as an irreversible structural constraint. Command-and-control networks that took decades to build cannot be reconstituted on operationally relevant timescales. The deterrence architecture's degradation appears as a fixed geopolitical reality.
constraint_indexing:constraint_classification(degraded_deterrence_architecture, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: US/ISRAELI DEFENSE ESTABLISHMENT (MOUNTAIN) — The constraint is experienced as a natural consequence of military-technical realities: precision strike capabilities, intelligence penetration, and proxy force attrition create an irreversible shift in the regional balance. The degradation is perceived as emerging from fundamental asymmetries in military capacity rather than from policy choices.
constraint_indexing:constraint_classification(degraded_deterrence_architecture, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL SECURITY ANALYSTS (MOUNTAIN) — From a generational perspective, the deterrence architecture's collapse reflects structural limits on proxy warfare as a strategic instrument. The observable metrics (mobilization rates, cross-border attack frequency, C2 communications) show degradation below operational thresholds that cannot be reversed through policy adjustment alone. The constraint emerges from the intersection of geography, technology, and organizational capacity.
constraint_indexing:constraint_classification(degraded_deterrence_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(regional))).

% PERSPECTIVE 4: PROXY FORCE COMMANDERS (MOUNTAIN) — Hezbollah, Iraqi militia, and Houthi commanders experience the degradation as an unchangeable operational reality. Personnel losses, weapons depot destruction, and severed logistics chains create constraints that cannot be overcome through tactical adaptation. The failure to mobilize during the June 2025 escalation revealed structural incapacity rather than strategic choice.
constraint_indexing:constraint_classification(degraded_deterrence_architecture, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GCC STATES (MOUNTAIN) — The constraint is perceived as a natural stabilization of regional power dynamics. The degradation of Iran's asymmetric warfare capacity appears as an inevitable correction following overextension. Even organized state actors with exit options see the shift as reflecting immutable geopolitical realities rather than contingent policy outcomes.
constraint_indexing:constraint_classification(degraded_deterrence_architecture, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — At civilizational scope, the constraint reflects fundamental limits on distributed military networks operating across hostile territory. The degradation is not policy-reversible on decision-relevant timescales. Proxy force reconstitution requires decades of investment in recruitment, training, weapons transfer, and operational integration. The June 2025 non-intervention revealed that these networks, once disrupted below critical density, cannot be rapidly restored.
constraint_indexing:constraint_classification(degraded_deterrence_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(degraded_deterrence_architecture_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(degraded_deterrence_architecture, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(degraded_deterrence_architecture, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(degraded_deterrence_architecture, ExtMetricName, E),
    domain_priors:suppression_score(degraded_deterrence_architecture, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(degraded_deterrence_architecture),
    narrative_ontology:constraint_metric(degraded_deterrence_architecture, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(degraded_deterrence_architecture, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(degraded_deterrence_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low, further reduced to strengthen mountain classification. The constraint imposes costs on Iranian strategic options through capacity destruction rather than asymmetric extraction. The degradation is not zero-sum — it represents irreversible loss of operational capacity rather than transfer to adversaries. Suppression (0.01): Minimal, further reduced. The constraint forecloses one strategic pathway (proxy deterrence) without eliminating Iranian agency. Alternative options (direct military action, nuclear acceleration, diplomatic realignment) remain available. Theater ratio (0.05): Very low, further reduced. The constraint is measured through concrete observables (mobilization rates, attack frequency, C2 communications) with minimal performative content. Metrics directly track operational capacity. Accessibility collapse (0.96): Very high, increased. The constraint is equally binding across all observation positions — Iranian planners, US defense officials, proxy commanders, regional analysts, and GCC states observe identical degradation. Resistance (0.04): Very low, further reduced. No agent can circumvent the constraint on decision-relevant timescales. Proxy reconstitution requires decades of investment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all six perspectives classify as mountain. Iranian strategic planners, US/Israeli defense establishment, proxy force commanders, regional analysts, GCC states, and the analytical observer all perceive the deterrence architecture's degradation as an irreversible structural reality on operationally relevant timescales. The uniformity of classification reflects that the constraint emerges from observable military-technical limits (precision strike capabilities, intelligence penetration, proxy force attrition below critical density) rather than from contingent policy choices. The constraint is not naturalized — it genuinely reflects structural limits on proxy warfare as a strategic instrument once networks are disrupted below operational thresholds.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims declared. The degradation imposes costs on Iranian strategic options but emerges from structural military-technical realities rather than from extraction by another agent. All perspectives classify as mountain because the constraint reflects fundamental limits on distributed military networks that cannot be reversed through policy adjustment. The very low extractiveness (0.08) reflects that the constraint is not zero-sum — it represents destruction of capacity rather than transfer of capacity. The minimal suppression (0.01) reflects that Iran retains other strategic options even as the proxy deterrence pathway is closed.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CONSTRAINT: This constraint resolves the mandatrophy by demonstrating that not all constraints involve extraction or coordination. The degraded deterrence architecture is a genuine mountain — a structural limit emerging from military-technical realities that cannot be reversed through policy adjustment on decision-relevant timescales. The very low extractiveness (0.08) reflects that the constraint imposes costs without transferring capacity to another agent. The minimal suppression (0.01) reflects that the constraint forecloses one strategic pathway without eliminating agency entirely. The very high accessibility collapse (0.96) and very low resistance (0.04) confirm that the constraint is equally binding across all observation positions and cannot be meaningfully circumvented. The constraint is not a false summit naturalizing contingent policy choices — it reflects genuine structural limits on distributed military networks operating across hostile territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(degraded_deterrence_architecture, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is structurally distinct from policy constraints on Iranian nuclear program or sanctions architecture. The deterrence degradation reflects military-technical limits on proxy force reconstitution rather than diplomatic or economic pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
