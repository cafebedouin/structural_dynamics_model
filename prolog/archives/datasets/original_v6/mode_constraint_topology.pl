% ============================================================================
% CONSTRAINT STORY: mode_constraint_topology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mode_constraint_topology, []).

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
 *   constraint_id: mode_constraint_topology
 *   human_readable: Mode Constraint Topology in Cognitive Transmission
 *   domain: cognitive_science/philosophy_of_mind/technology_studies
 *
 * SUMMARY:
 *   The mode constraint topology describes the structural differences in how
 *   thought, speech, writing, and AI-mediated communication constrain
 *   cognitive processes. These are not social conventions or institutional
 *   arrangements but physical and computational properties of the
 *   transmission substrates. Thought operates in massively parallel neural
 *   architecture, permitting contradiction coexistence and non-linear
 *   exploration. Speech requires sequential phoneme production in real-time,
 *   forcing linearization and immediate coherence. Writing creates a durable
 *   external record, imposing accountability and enabling asynchronous
 *   review. AI-mediated communication combines speech-speed interaction with
 *   written record-keeping, creating a novel constraint topology. These
 *   differences are measurable: experimental cognitive science demonstrates
 *   that idea development differs systematically across modes, with thought
 *   showing higher tolerance for contradiction, speech showing forced
 *   real-time resolution, and writing showing revision-driven refinement. The
 *   constraint is a genuine natural law — no agent can eliminate the
 *   structural properties of the medium they operate within, though agents
 *   can choose which medium to use for a given cognitive task.
 *
 * KEY AGENTS:
 *   - Individual Thinker: Universal agent (powerless/trapped at immediate scope) — experiences mode constraints as immediate and inescapable within each mode
 *   - Educational Institution: Institutional agent (institutional/arbitrage at global scope) — can select modes for pedagogy but cannot alter mode properties
 *   - Cognitive Scientist: Analytical observer (analytical/analytical at universal scope) — measures structural differences across modes as physical/computational constraints
 *   - Oral Culture Collective: Organized group (organized/constrained at continental scope) — can maintain oral-dominant practices but cannot eliminate writing's structural differences
 *   - Professional Writer: Individual with high mobility (powerful/mobile at national scope) — navigates across modes but experiences each mode's constraints as fixed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mode_constraint_topology, 0.08).
domain_priors:suppression_score(mode_constraint_topology, 0.03).
domain_priors:theater_ratio(mode_constraint_topology, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mode_constraint_topology, extractiveness, 0.08).
narrative_ontology:constraint_metric(mode_constraint_topology, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(mode_constraint_topology, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mode_constraint_topology, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(mode_constraint_topology, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mode_constraint_topology, mountain).
narrative_ontology:human_readable(mode_constraint_topology, "Mode Constraint Topology in Cognitive Transmission").
narrative_ontology:topic_domain(mode_constraint_topology, "cognitive_science/philosophy_of_mind/technology_studies").

domain_priors:emerges_naturally(mode_constraint_topology).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL THINKER (MOUNTAIN) — Cannot escape the structural constraints of the transmission mode they inhabit. Thought permits contradiction coexistence; speech forces real-time linearization; writing creates durable record; AI provides hybrid properties. These are not negotiable features but inherent properties of the medium.
constraint_indexing:constraint_classification(mode_constraint_topology, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION (MOUNTAIN) — Can choose which modes to emphasize in pedagogy (oral examination vs written essay vs collaborative AI-assisted work) but cannot change the structural properties of each mode. The institution arbitrages across modes but experiences each mode's constraints as fixed when operating within it.
constraint_indexing:constraint_classification(mode_constraint_topology, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Observes that mode constraints are structural properties of information transmission substrates. Thought operates in parallel neural architecture; speech requires sequential phoneme production; writing creates persistent external memory; AI combines speech-speed processing with written durability. These are physical/computational constraints, not social constructions.
constraint_indexing:constraint_classification(mode_constraint_topology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ORAL CULTURE COLLECTIVE (MOUNTAIN) — Organized groups in oral-dominant cultures experience writing's accountability constraint as foreign but cannot eliminate the structural difference: spoken words vanish, written words persist. The collective can choose to remain oral-dominant but cannot make speech create the same record-keeping properties as writing.
constraint_indexing:constraint_classification(mode_constraint_topology, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: PROFESSIONAL WRITER (MOUNTAIN) — Has high mobility across modes (can think, speak, write, use AI tools) but experiences each mode's structural constraints as unchangeable when operating within it. Writing forces externalization and sequencing that thought does not require; AI assistance changes speed but not the fundamental constraint topology.
constraint_indexing:constraint_classification(mode_constraint_topology, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mode_constraint_topology_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(mode_constraint_topology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mode_constraint_topology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mode_constraint_topology, ExtMetricName, E),
    domain_priors:suppression_score(mode_constraint_topology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(mode_constraint_topology),
    narrative_ontology:constraint_metric(mode_constraint_topology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(mode_constraint_topology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(mode_constraint_topology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal extraction — it is a structural property of information substrates, not a mechanism that transfers resources from one agent to another. The small non-zero value reflects that mode choice can have career and epistemic consequences (written work is more accountable than spoken claims, creating asymmetric risk), but this is incidental to the constraint's primary function as a physical/computational limit. Suppression (0.03): Very low. Agents have high freedom to choose which mode to operate in for most cognitive tasks. The constraint does not suppress alternatives — it defines the properties of each alternative. Theater ratio (0.15): Very low. The constraint is functional, not performative. Mode differences are real and measurable, not theatrical. The small non-zero value reflects that some institutional practices around mode choice (e.g., requiring written documentation for accountability) may have performative elements, but the underlying constraint is genuine. Accessibility collapse (0.92): Very high. The constraint is highly accessible — any agent who uses multiple modes can directly experience the structural differences. Resistance (0.08): Very low. The constraint shows minimal resistance to investigation. Cognitive science can measure mode effects experimentally; individuals can introspect on mode differences; the constraint's properties are not hidden or obscured.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all five perspectives classify as mountain. The individual thinker, educational institution, cognitive scientist, oral culture collective, and professional writer all experience mode constraints as structural properties of transmission substrates that cannot be eliminated through social or institutional change. The perspectives differ in scope (immediate to universal) and exit options (trapped to arbitrage), but all converge on the mountain classification because the constraint is a genuine natural law. The small differences in experienced extractiveness across perspectives (due to different d values from power/exit combinations) do not change the classification — all perspectives fall well within the mountain thresholds (ε ≤ 0.25, suppression ≤ 0.05, χ determined by very low base extraction). This uniform classification is diagnostic: it confirms that the constraint is not a false summit (no identifiable beneficiaries), not a coordination mechanism (no institutional enforcement), and not a degraded ritual (low theater ratio). The constraint is what it appears to be — a physical and computational limit on information transmission.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a uniform-type mountain constraint with no beneficiaries or victims. The structural properties of transmission modes are not extraction mechanisms — they are physical and computational limits that apply universally. All agents experience the same mode constraints regardless of power, exit options, or scope. The constraint does not transfer resources from one group to another; it defines the operational properties of cognitive substrates. Directionality is not applicable because there is no extraction flow. The engine will derive d values from the power/exit combinations in each perspective, but these will not produce meaningful extraction differentials because the base extractiveness is very low (0.08) and the constraint has no asymmetric beneficiary/victim structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints are extraction mechanisms. The mode constraint topology is a structural property of information substrates, not a social arrangement that benefits some agents at the expense of others. The mountain classification is not a naturalization of contingent institutional arrangements (false summit) but an accurate recognition of physical and computational limits. The constraint's low extractiveness (0.08), low suppression (0.03), high accessibility collapse (0.92), and low resistance (0.08) all confirm the natural law signature. The constraint does impose costs — writing requires more effort than thought, speech forces real-time coherence, AI mediation introduces new dependencies — but these costs are inherent to the transmission substrate, not extractive overhead layered onto a coordination function. The mandatrophy question 'Is this coordination or extraction?' does not apply because the constraint is neither — it is a physical limit. The framework correctly classifies it as mountain from all perspectives, confirming that the indexical classification system can distinguish genuine natural laws from false summits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mode_constraint_topology, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mode_constraint_topology, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a singleton — it does not decompose into multiple stories with different epsilon values because the structural properties of transmission modes are invariant across observables. Measuring mode constraints via experimental cognitive science, phenomenological introspection, or computational analysis yields the same structural topology: thought permits parallel processing, speech forces sequential production, writing creates durable record, AI combines speed with persistence. The constraint is not observer-dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
