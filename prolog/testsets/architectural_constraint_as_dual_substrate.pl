% ============================================================================
% CONSTRAINT STORY: architectural_constraint_as_dual_substrate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_architectural_constraint_as_dual_substrate, []).

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
 *   constraint_id: architectural_constraint_as_dual_substrate
 *   human_readable: Architectural Constraint as Dual Substrate for Hierarchy and Collective Organization
 *   domain: organizational_dynamics/labor_relations/institutional_power
 *
 * SUMMARY:
 *   The architectural constraint as dual substrate represents a fundamental
 *   structural property of spatial organization: any physical arrangement
 *   that segregates a population to enforce hierarchy simultaneously
 *   concentrates that population in ways that enable collective coordination
 *   and resistance. This is not a contingent institutional design but an
 *   emergent property of spatial topology and social coordination dynamics.
 *   The constraint appears across organizational contexts: factory floors
 *   separate workers from management, creating both hierarchical control and
 *   union organizing capacity; prison architecture segregates inmates,
 *   creating both institutional control and gang formation; residential
 *   segregation concentrates populations, creating both discriminatory
 *   control and community solidarity; military barracks separate ranks,
 *   creating both command hierarchy and unit cohesion. The dual substrate
 *   property is a mathematical invariant: spatial concentration reduces
 *   coordination costs for the concentrated population regardless of the
 *   architect's intent. The constraint's base extractiveness (0.12) reflects
 *   minimal inherent extraction — the dual substrate property itself is
 *   neutral, though the hierarchical arrangements it enables may be
 *   extractive. Suppression (0.03) is negligible because the constraint
 *   emerges from physical and social topology, not from active enforcement.
 *   Theater ratio (0.08) is minimal because the constraint operates through
 *   genuine physical and coordination mechanisms, not performative rituals.
 *   This is a genuine mountain: all perspectives classify it as such because
 *   the dual substrate property is an immutable feature of how spatial
 *   organization interacts with collective coordination capacity.
 *
 * KEY AGENTS:
 *   - Segregated Worker: Powerless/trapped — experiences spatial division as natural fact; dual substrate invisible at immediate horizon
 *   - Union Organizer: Moderate/constrained — recognizes enabling function (concentration enables organizing) but naturalizes extractive function (hierarchical separation)
 *   - Management Architect: Institutional/arbitrage — designed hierarchical function, perceives collective organization function as unintended emergent consequence
 *   - Urban Planner: Powerful/mobile — observes pattern across contexts, recognizes as structural invariant of spatial organization
 *   - Analytical Observer: Analytical/analytical — identifies dual substrate as mathematical property of spatial networks and coordination dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(architectural_constraint_as_dual_substrate, 0.12).
domain_priors:suppression_score(architectural_constraint_as_dual_substrate, 0.03).
domain_priors:theater_ratio(architectural_constraint_as_dual_substrate, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, extractiveness, 0.12).
narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(architectural_constraint_as_dual_substrate, mountain).
narrative_ontology:human_readable(architectural_constraint_as_dual_substrate, "Architectural Constraint as Dual Substrate for Hierarchy and Collective Organization").
narrative_ontology:topic_domain(architectural_constraint_as_dual_substrate, "organizational_dynamics/labor_relations/institutional_power").

domain_priors:emerges_naturally(architectural_constraint_as_dual_substrate).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SEGREGATED WORKER / IMMEDIATE (MOUNTAIN) — Physical separation from management appears as unchangeable spatial fact. The worker experiences the architectural division as a natural feature of the workplace, not as a designed constraint. At immediate time horizon, the dual substrate property is invisible — only the separation itself is perceived.
constraint_indexing:constraint_classification(architectural_constraint_as_dual_substrate, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: UNION ORGANIZER / BIOGRAPHICAL (MOUNTAIN) — Recognizes that spatial concentration enables collective organization but perceives the architectural substrate itself as immutable. The organizer works within the constraint, using the concentration it creates, but does not question the physical division as a contingent design choice. The dual substrate is partially visible — the enabling function is recognized, the extractive function is naturalized.
constraint_indexing:constraint_classification(architectural_constraint_as_dual_substrate, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MANAGEMENT ARCHITECT / GENERATIONAL (MOUNTAIN) — Designed the spatial segregation to enforce hierarchy (separate entrances, floors, facilities for different ranks) but perceives the dual substrate property as an unintended emergent consequence. Management sees the hierarchical function as designed and the collective organization function as a natural byproduct of spatial concentration. The constraint appears as a law of organizational physics: concentrate workers and they will organize.
constraint_indexing:constraint_classification(architectural_constraint_as_dual_substrate, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / CIVILIZATIONAL (MOUNTAIN) — Recognizes the dual substrate property as a structural invariant of spatial organization: any architectural division that concentrates a population simultaneously enables both hierarchical control (through segregation) and collective coordination (through proximity). This is not a contingent institutional arrangement but a mathematical property of spatial networks. The constraint is a genuine mountain — it emerges from the topology of physical space and social coordination. The dual substrate is fully visible and recognized as immutable.
constraint_indexing:constraint_classification(architectural_constraint_as_dual_substrate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: URBAN PLANNER / GENERATIONAL (MOUNTAIN) — Observes the pattern across multiple organizational contexts (factories, prisons, schools, hospitals, military bases) and recognizes it as a structural invariant. Spatial segregation always produces dual substrate effects: the same architecture that enforces hierarchy creates the conditions for collective resistance. The planner can choose different spatial arrangements but cannot escape the dual substrate property — it is a law of spatial organization, not a design choice.
constraint_indexing:constraint_classification(architectural_constraint_as_dual_substrate, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(architectural_constraint_as_dual_substrate_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(architectural_constraint_as_dual_substrate, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(architectural_constraint_as_dual_substrate, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, ExtMetricName, E),
    domain_priors:suppression_score(architectural_constraint_as_dual_substrate, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(architectural_constraint_as_dual_substrate),
    narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(architectural_constraint_as_dual_substrate, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(architectural_constraint_as_dual_substrate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The dual substrate property itself is structurally neutral — it is a mathematical fact about spatial concentration and coordination costs. The hierarchical arrangements that exploit this property may be extractive, but the constraint itself (the dual substrate property) has minimal inherent extraction. The value reflects that spatial segregation does impose some coordination costs on cross-group interaction, but these are inherent to physical distance, not designed extraction. Suppression (0.03): Negligible. The constraint emerges from the topology of physical space and the mathematics of coordination costs. No active enforcement is required to maintain the dual substrate property — it is an automatic consequence of spatial concentration. Resistance (0.08): Very low. Attempts to eliminate the dual substrate property fail because it is a structural invariant. Architects can choose different spatial arrangements (integrated vs segregated, centralized vs distributed), but any arrangement that concentrates a population will reduce coordination costs for that population, enabling both hierarchical control and collective resistance. Accessibility collapse (0.92): Very high. The dual substrate property is accessible to all observers who understand spatial topology and coordination dynamics. The property is not hidden behind complexity or institutional barriers — it is a direct consequence of physical proximity and social coordination. Theater ratio (0.08): Minimal. The constraint operates through genuine physical and coordination mechanisms. Spatial proximity genuinely reduces coordination costs; spatial segregation genuinely enables both hierarchical control and collective organization. There is no performative layer obscuring the mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap — all observers classify it as mountain because the dual substrate property is a genuine structural invariant. The worker, organizer, architect, planner, and analytical observer all perceive the constraint as immutable, though they differ in their awareness of the dual substrate property itself. The immediate worker sees only separation; the biographical organizer sees the enabling function; the generational architect sees the hierarchical function; the civilizational analyst sees both functions as emergent from the same spatial topology. But all agree that the constraint is unchangeable — it is a law of spatial organization, not a contingent institutional arrangement. The uniformity of classification confirms the mountain hypothesis: this is not a naturalized extraction mechanism but a genuine physical and mathematical limit.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a uniform-type mountain constraint with no beneficiaries or victims declared. The dual substrate property is structurally neutral — it is a mathematical fact about spatial organization that can be exploited for either hierarchical control or collective resistance. All perspectives experience the constraint as immutable because it emerges from physical and social topology, not from institutional design. The constraint's neutrality is reflected in the absence of beneficiary/victim declarations and the uniform mountain classification across all perspectives. Directionality values are not computed because the constraint has no extraction flow — it is a substrate that enables other constraints (hierarchical control systems, collective organizing mechanisms) but is not itself extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CONFIRMATION: This constraint resolves the mandatrophy by demonstrating that the dual substrate property is a genuine structural invariant, not a naturalized extraction mechanism. The key diagnostic: the constraint enables both hierarchical control AND collective resistance from the same spatial arrangement. If this were a disguised snare (hierarchical control naturalized as spatial necessity), it would not simultaneously enable the resistance mechanism. The dual substrate property is genuinely neutral — it is a mathematical fact about coordination costs in spatial networks. Architects and institutions exploit this property for hierarchical control, but the property itself is not extractive. The constraint passes all mountain gates: emerges naturally (true — follows from spatial topology), accessibility collapse (0.92 — visible to all who understand coordination dynamics), resistance (0.08 — attempts to eliminate fail because it's a structural invariant), extractiveness (0.12 — minimal inherent extraction), suppression (0.03 — no active enforcement needed). The uniform mountain classification across all perspectives confirms that this is not a perspectival artifact but a genuine immutable constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(architectural_constraint_as_dual_substrate, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(architectural_constraint_as_dual_substrate, information_standard).

% DUAL FORMULATION NOTE:
% The architectural constraint as dual substrate is a foundational constraint that enables multiple downstream constraints: hierarchical control systems (which exploit the segregation function), collective organizing mechanisms (which exploit the concentration function), and spatial discrimination patterns (which exploit both functions). Each downstream constraint has its own extractiveness value reflecting the specific institutional arrangement. The dual substrate itself is neutral — a mathematical property of spatial organization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
