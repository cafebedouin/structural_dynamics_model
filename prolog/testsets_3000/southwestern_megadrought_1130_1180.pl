% ============================================================================
% CONSTRAINT STORY: southwestern_megadrought_1130_1180
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_southwestern_megadrought_1130_1180, []).

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
 *   constraint_id: southwestern_megadrought_1130_1180
 *   human_readable: Southwestern Megadrought 1130–1180 CE
 *   domain: paleoclimatology/environmental_history
 *
 * SUMMARY:
 *   The Southwestern Megadrought (ca. 1130–1180 CE) was a 50-year period of
 *   severely reduced precipitation across the American Southwest, documented
 *   through tree ring reconstructions (Palmer Drought Severity Index) and
 *   archaeological evidence of settlement abandonment and food stress. This
 *   megadrought directly preceded the collapse of some Ancestral Puebloan
 *   societies (Chacoan culture) and is temporally correlated with widespread
 *   abandonment of major settlements. However, the causal status of the
 *   drought is contested: it functions as a triggering constraint (removing
 *   the ability to maintain large sedentary populations) but some communities
 *   adapted through migration or intensified agriculture, suggesting the
 *   drought was severe but not absolutely deterministic of cultural collapse.
 *   The constraint classification as pure Mountain is diagnostic:
 *   precipitation is determined by ocean-atmosphere dynamics, not by human
 *   choice or institutional design. The extractiveness value (0.08) reflects
 *   that the constraint operates on subsistence capacity—lives and
 *   livelihoods are affected—but this is not 'extraction' in the structural
 *   sense. No agent benefits from the drought; no suppression mechanism
 *   maintains it. It is a natural law operating on human populations.
 *
 * KEY AGENTS:
 *   - Ancestral Puebloan Communities: Powerless/trapped agents (generational time scale) — dependent on regional precipitation; no escape route without abandoning territory and identity
 *   - Regional Climate System: The constraint itself (institutional/global scale) — persistent ocean-atmosphere circulation pattern
 *   - Paleoclimatologists: Analytical observers (civilizational scale) — document the constraint via tree rings and model attribution
 *   - Agricultural Societies: Institutional victims (generational scale) — subsistence systems collapse under persistent water deficit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(southwestern_megadrought_1130_1180, 0.08).
domain_priors:suppression_score(southwestern_megadrought_1130_1180, 0.02).
domain_priors:theater_ratio(southwestern_megadrought_1130_1180, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, extractiveness, 0.08).
narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(southwestern_megadrought_1130_1180, mountain).
narrative_ontology:human_readable(southwestern_megadrought_1130_1180, "Southwestern Megadrought 1130–1180 CE").
narrative_ontology:topic_domain(southwestern_megadrought_1130_1180, "paleoclimatology/environmental_history").

domain_priors:emerges_naturally(southwestern_megadrought_1130_1180).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANCESTRAL PUEBLOAN COMMUNITIES (MOUNTAIN) — Complete dependence on precipitation for agriculture. No exit option: cannot move out of the Southwest's biogeographic envelope without abandoning accumulated settlement, knowledge systems, and territorial claims. Trapped by geography and subsistence mode. The 50-year precipitation deficit is an immutable constraint at generational scale.
constraint_indexing:constraint_classification(southwestern_megadrought_1130_1180, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALEOCLIMATOLOGIST / OBSERVATIONAL (MOUNTAIN) — Tree ring reconstruction (PDSI indices) documents the megadrought as a climate phenomenon with zero degrees of freedom from a causal perspective. The precipitation deficit is determined by large-scale ocean-atmosphere circulation (persistent La Niña-like conditions). No agent chooses or maintains this state — it emerges from solar forcing, ocean temperature gradients, and atmospheric dynamics. Accessibility collapse is maximal: the causal mechanism is fully outside human control or agency.
constraint_indexing:constraint_classification(southwestern_megadrought_1130_1180, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL CLIMATE SYSTEM (MOUNTAIN) — The megadrought is a persistent state of the climate system emerging from large-scale atmospheric drivers. The Pacific Decadal Oscillation (PDO) and sea surface temperature anomalies maintain the circulation pattern that suppresses precipitation over the Southwest. This is an unchangeable property of how the climate system operates during this forcing configuration — not chosen by any agent, not maintained by suppression, but determined by thermodynamic and hydrodynamic laws.
constraint_indexing:constraint_classification(southwestern_megadrought_1130_1180, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: AGRICULTURAL SOCIETIES (MOUNTAIN) — For societies organized entirely around irrigation agriculture and rain-fed farming, a 50-year precipitation deficit produces existential constraint. Food production capacity collapses. Starvation, resource conflict, and forced migration are not choices but structural necessities. The constraint is unchangeable within the society's technological and institutional capacity.
constraint_indexing:constraint_classification(southwestern_megadrought_1130_1180, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(southwestern_megadrought_1130_1180_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(southwestern_megadrought_1130_1180, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(southwestern_megadrought_1130_1180, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, ExtMetricName, E),
    domain_priors:suppression_score(southwestern_megadrought_1130_1180, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(southwestern_megadrought_1130_1180),
    narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(southwestern_megadrought_1130_1180, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(southwestern_megadrought_1130_1180_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The megadrought constrains subsistence capacity but does not represent asymmetric extraction from one agent to another. No group benefits while others are taxed; rather, all populations within the affected region experience the same ambient precipitation deficit. The value (0.08) reflects that the constraint does impose severe costs, but these costs are not extracted or transferred—they are experienced uniformly across all dependent populations. Suppression (0.02): Minimal. There is no suppression mechanism—no enforcement infrastructure, no penalty for non-compliance, no alternative pathways that are suppressed. Precipitation either falls or does not. Theater ratio (0.05): Minimal. The megadrought is not performative. Tree ring width correlates directly with precipitation; the relationship is causal and observable. No pretense, no institutional theater, no symbolic compensation—the constraint's mechanism is fully transparent.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the megadrought as Mountain because the constraint's status as a natural law is invariant across observer positions. The Ancestral Puebloan communities experience it as an immutable limit (trapped, generational). The climate system exhibits it as a persistent circulation pattern (institutional). The paleoclimatologist observes it as forced climate variability (analytical). The agricultural societies encounter it as existential constraint (institutional). None of these perspectives sees the megadrought as coordinated, extractive, theatrical, or mutable. The perspectival gap is minimal—all observers agree on the classification because the constraint's mechanisms are fully external to human choice or institutional design.
 *
 * MANDATROPHY ANALYSIS:
 *   The Southwest Megadrought exemplifies the mandatrophy-free case: no risk of misclassifying coordination as extraction or vice versa because the constraint has no coordination function. The megadrought did not solve a collective action problem; it created one. Subsequent Puebloan responses (migration, settlement reorganization, adoption of kivas for water rituals) are coordination mechanisms that emerge IN RESPONSE to the constraint, not part of the constraint itself. The constraint is purely a natural law: precipitation deficit determined by climate dynamics. The mandatrophy is resolved by the absence of any coordination-vs-extraction ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forcing_attribution_confidence,
    'What fraction of the megadrought is forced by external solar/ocean drivers versus internal climate variability?',
    'Climate model ensemble comparisons: forced (greenhouse gas + solar) vs. unforced (internal variability only) simulations. Attribution of the 50-year PDSI anomaly to each forcing class.',
    'If forced fraction > 95%: mountain classification is robust (natural law). If forced fraction < 75%: some portion of the drought reflects unpredictable internal variability, potentially opening Rope-like interpretation (societies could have adapted if the variability were less extreme but not impossible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forcing_attribution_confidence, empirical, 'Attribution of megadrought to forced vs. internal climate variability').

omega_variable(
    technological_adaptation_feasibility,
    'Could Ancestral Puebloan technology (ca. 1130–1180) have adapted to the megadrought through irrigation, storage, or migration?',
    'Archaeological evidence of contemporaneous adaptations elsewhere; hydrologic feasibility of groundwater wells or long-distance water transport with available technology; carrying capacity analysis for migration corridors.',
    'If feasible adaptations existed but were not adopted: the constraint is Snare or Tangled Rope (institutional/cultural barriers), not Mountain. If no feasible adaptations existed: Mountain classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_adaptation_feasibility, empirical, 'Feasibility of technological adaptation to megadrought conditions').

omega_variable(
    megadrought_periodicity,
    'Is the 50-year megadrought a recurrent climate phenomenon (periodic) or a rare anomaly (contingent)?',
    'Tree ring time series extending 2000+ years; wavelet analysis for cyclic components; comparison to other megadroughts in the paleoclimate record (e.g., Medieval Warm Period droughts elsewhere).',
    'If periodic (e.g., occurs once per 200-300 years): societies experienced this constraint repeatedly, suggesting possible adaptation mechanisms that failed due to contingent factors (population growth, political fragmentation). If rare (< once per 1000 years): truly novelty-level constraint, supporting Mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(megadrought_periodicity, empirical, 'Periodicity vs. rarity of megadrought events').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(southwestern_megadrought_1130_1180, 1130, 1180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swmega_tr_t1130, southwestern_megadrought_1130_1180, theater_ratio, 1130, 0.02).
narrative_ontology:measurement(swmega_tr_t1155, southwestern_megadrought_1130_1180, theater_ratio, 1155, 0.03).

% Extraction over time
narrative_ontology:measurement(swmega_be_t1130, southwestern_megadrought_1130_1180, base_extractiveness, 1130, 0.08).
narrative_ontology:measurement(swmega_be_t1140, southwestern_megadrought_1130_1180, base_extractiveness, 1140, 0.08).
narrative_ontology:measurement(swmega_be_t1155, southwestern_megadrought_1130_1180, base_extractiveness, 1155, 0.08).
narrative_ontology:measurement(swmega_be_t1170, southwestern_megadrought_1130_1180, base_extractiveness, 1170, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(southwestern_megadrought_1130_1180, ancestral_puebloan_settlement_reorganization).
narrative_ontology:affects_constraint(southwestern_megadrought_1130_1180, chacoan_societal_collapse).

% DUAL FORMULATION NOTE:
% The megadrought is a climate constraint (Mountain). The societal responses—migration, settlement consolidation, intensified trade and water management—are distinct constraints that emerge downstream of the megadrought. Each response constraint has different extractiveness and coordination type. The megadrought provides the causal background but is not itself a response or adaptation constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
