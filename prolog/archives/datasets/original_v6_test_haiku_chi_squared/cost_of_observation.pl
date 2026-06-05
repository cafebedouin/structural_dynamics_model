% ============================================================================
% CONSTRAINT STORY: cost_of_observation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cost_of_observation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cost_of_observation
 *   human_readable: The Evolutionary Cost of Observation
 *   domain: philosophical/evolutionary
 *
 * SUMMARY:
 *   The evolutionary cost of observation describes a meta-constraint: any
 *   situated, embodied agent that maintains internal state and processes
 *   external information incurs an unavoidable tax in energy, attention, risk
 *   exposure, and opportunity cost. This constraint operates across
 *   biological evolution (metabolic cost of sensory systems, predation risk
 *   from visibility), epistemology (cost of measurement apparatus and labor),
 *   information theory (Landauer's principle: energy cost of information
 *   processing), and political economy (surveillance infrastructure cost).
 *   The constraint exhibits all five non-mountain types from different
 *   structural positions. From the perspective of the trapped organism, it is
 *   a pure snare: the costs of observation (metabolic overhead, visibility to
 *   predators) cannot be avoided without losing adaptive capacity. From the
 *   analytical view, it appears as a natural law: thermodynamic limits on
 *   information processing are universal. From the evolutionary organism's
 *   view, it is a tangled rope: sensory systems provide genuine coordination
 *   benefits (threat detection, resource location) alongside extraction
 *   costs. From the scientific community's view, it is coordination (rope):
 *   shared instrumentation and data distribute costs. From the surveillance
 *   state's view, it is a degraded piton: observation infrastructure persists
 *   through institutional inertia despite declining functional legitimacy
 *   relative to its stated purposes.
 *
 * KEY AGENTS:
 *   - All Situated Observers: Primary victim (powerless/trapped) — cannot cease observation without ceasing agency; inescapable cost
 *   - Evolutionary Organisms: Secondary victim + beneficiary (moderate/constrained) — sensory systems are adaptive but metabolically expensive
 *   - Scientific Community: Beneficiary (institutional/mobile) — coordinates through shared observation, distributes costs
 *   - Information Processors: Victim (powerless/trapped) — Landauer's principle imposes fundamental thermodynamic cost
 *   - Surveillance State: Primary extractor (institutional/arbitrage) — claims coordination (security) but primarily extracts behavioral control
 *   - Analytical Observer: Naturalizing agent (analytical/analytical) — risks reading contingent costs as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cost_of_observation, 0.58).
domain_priors:suppression_score(cost_of_observation, 0.62).
domain_priors:theater_ratio(cost_of_observation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cost_of_observation, extractiveness, 0.58).
narrative_ontology:constraint_metric(cost_of_observation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cost_of_observation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cost_of_observation, snare).
narrative_ontology:human_readable(cost_of_observation, "The Evolutionary Cost of Observation").
narrative_ontology:topic_domain(cost_of_observation, "philosophical/evolutionary").

% --- Structural relationships ---
narrative_ontology:constraint_victim(cost_of_observation, all_situated_observers).
narrative_ontology:constraint_victim(cost_of_observation, epistemic_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED OBSERVER (SNARE) — Any agent that inhabits a physical substrate and maintains internal state incurs inevitable costs to track, process, and act on observations. Cannot exit observation itself without ceasing to exist as a functional agent. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(cost_of_observation, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From thermodynamic and information-theoretic grounds, observation incurs irreducible costs. Any measurement requires energy expenditure (Landauer's principle), state-space expansion (memory), and computational overhead. These are fundamental features of physics, not contingent institutional arrangements. ε=0.58 contradicts pure mountain status; the engine will flag this as a false summit.
constraint_indexing:constraint_classification(cost_of_observation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: EVOLUTIONARY ORGANISM (TANGLED ROPE) — Organisms benefit from sensory and cognitive capabilities (coordination function: detecting threats, finding resources). But these same systems incur metabolic cost, predation risk, and thermal exposure (extraction function: energy depletion, visibility to predators). d≈0.72, f(d)≈1.13, σ=0.9 → χ≈0.57.
constraint_indexing:constraint_classification(cost_of_observation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: SCIENTIFIC COMMUNITY (ROPE) — Scientists experience observation as a coordination mechanism: sharing observational data, methods, and instrumentation creates networks of knowledge. The cost of instrumentation and labor is distributed, not extracted asymmetrically. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.28.
constraint_indexing:constraint_classification(cost_of_observation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SURVEILLANCE STATE (PITON) — State actors claim observation is coordination (security, public safety) but primarily extract behavioral control and compliance. The performative justification (theater_ratio=0.48) masks the extraction mechanism. d≈0.15, f(d)≈-0.01, σ=0.8 → χ≈-0.001. Piton status from theater gate: institutional inertia maintains surveillance infrastructure despite declining functional legitimacy.
constraint_indexing:constraint_classification(cost_of_observation, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cost_of_observation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cost_of_observation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cost_of_observation, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cost_of_observation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cost_of_observation, TR),
    TR >= 0.70.

:- end_tests(cost_of_observation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint does extract from all observers — the cost of observation (energy, time, risk) is real and substantial. But it is not maximal (ε≥0.70) because: (1) observers gain adaptive benefit from sensory systems, so extraction is partial rather than total; (2) scientific communities have distributed these costs through collaboration, reducing per-agent burden; (3) technological progress has reduced measurement costs over evolutionary and historical timescales. Suppression (0.62): Moderate-high. Significant suppression exists because: (1) alternatives to observation are limited — agents cannot navigate environments without some sensory capacity; (2) the cost is often hidden or naturalized as inevitable rather than contingent; (3) institutional surveillance claims coordination justification, suppressing the control extraction. But suppression is not total because: (1) organisms have evolved efficient sensory systems that minimize cost; (2) scientific communities openly discuss measurement labor and instrument development; (3) privacy movements articulate the extraction mechanism of surveillance. Theater ratio (0.48): Moderate. Some performative content exists, primarily in surveillance infrastructure (claims of security and public safety mask control extraction), but the underlying thermodynamic and biological costs are real and not purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects the ambiguity of whether the constraint is fundamental or contingent. The analytical observer risks reading a contingent cost (current state of sensory/measurement technology) as a natural law (Landauer's principle). The organism's view is tangled rope: sensory systems genuinely enable adaptation, but at real metabolic cost. The evolutionary lens shows improvement over deep time (sensory systems have become more efficient), suggesting the constraint is not immutable. The surveillance state perspective reveals how institutional actors can naturalize extraction (claiming coordination justification for control) while suppressing the underlying cost structure. The scientific community's rope perspective shows that distributed observation (collaboration, open data) reduces per-agent extraction compared to isolated observation.
 *
 * DIRECTIONALITY LOGIC:
 *   All situated observers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — cannot exit observation without ceasing agency. Evolutionary organisms: Victim + constrained + beneficiary → d≈0.72, f(d)≈1.13. High extraction but partial benefit from sensory adaptation. Scientific community: Beneficiary + mobile → d≈0.40, f(d)≈0.40. Low effective extraction due to cost distribution and coordination benefits. Surveillance state: Extractor + arbitrage → d≈0.15, f(d)≈-0.01. Institutional beneficiary from observation extraction (behavioral control), though piton classification derives from theater gate rather than chi. Information processors: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction from thermodynamic constraints. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Observer position risks naturalization of contingent costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_vs_contingent_cost,
    'Is the cost of observation a fundamental feature of any physical information processing, or is it a contingent feature of evolved biological systems and engineered instruments?',
    'Theoretical analysis of abstract computation models (reversible logic, quantum information) vs empirical measurement of actual biological and technological systems; identification of whether cost reduction is asymptotically bounded by physics or merely by engineering constraints',
    'If fundamental: constraint approaches mountain classification (ε→0.15). If contingent: constraint remains snare/tangled_rope (ε≈0.58), and the ''inevitability'' framing is naturalization of engineering choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamental_vs_contingent_cost, conceptual, 'Whether observation cost is fundamental physical law or contingent system design').

omega_variable(
    perception_action_loop_closure,
    'Does the cost of observation scale with the agent''s response bandwidth, or is it independent of whether observations are acted upon?',
    'Comparative analysis: metabolic cost of sensory systems vs cost of processing and action generation across species; measurement of perception-only (non-acting) vs perception-action integrated systems',
    'If cost scales with action: observation is instrumentally costly (tangled rope from organism perspective). If cost is perception-only: observation is structurally unavoidable (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perception_action_loop_closure, empirical, 'Whether observation cost depends on action generation or is perception-intrinsic').

omega_variable(
    information_theoretic_free_lunch,
    'Do quantum information systems or reversible computation eliminate the thermodynamic cost of observation, or is Landauer''s principle truly universal?',
    'Laboratory realization of reversible quantum computations with zero net entropy production during information acquisition; theoretical proof or counterexample to universal lower bounds on measurement cost',
    'If quantum/reversible systems achieve zero-cost observation: constraint is engineering contingency (ε→0.25, mountain possible). If Landauer bound is universal: constraint is fundamental (ε remains 0.58, snare from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_theoretic_free_lunch, empirical, 'Whether reversible quantum observation can achieve zero thermodynamic cost').

omega_variable(
    observer_boundary_definition,
    'Where is the boundary between ''system'' and ''observer''? Does the cost of observation belong to the observed system, the observing system, or is it a property of the interaction itself?',
    'Formal system-theoretic analysis of observer-observed coupling; identification of whether cost assignment is invariant under change of reference frame or perspective',
    'If boundary-dependent: ''cost of observation'' is an artifact of perspective (tangled rope or rope depending on framing). If boundary-invariant: cost is real and intrinsic (snare from all frames).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(observer_boundary_definition, conceptual, 'Whether observer-system boundary assignment determines cost attribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cost_of_observation, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cobs_tr_t0, cost_of_observation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cobs_tr_t250, cost_of_observation, theater_ratio, 250, 0.38).
narrative_ontology:measurement(cobs_tr_t500, cost_of_observation, theater_ratio, 500, 0.48).

% Extraction over time
narrative_ontology:measurement(cobs_be_t0, cost_of_observation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cobs_be_t250, cost_of_observation, base_extractiveness, 250, 0.45).
narrative_ontology:measurement(cobs_be_t500, cost_of_observation, base_extractiveness, 500, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cost_of_observation, information_standard).
narrative_ontology:affects_constraint(cost_of_observation, measurement_problem_quantum_mechanics).
narrative_ontology:affects_constraint(cost_of_observation, embodied_cognition_constraint).
narrative_ontology:affects_constraint(cost_of_observation, surveillance_extraction_mechanism).

% DUAL FORMULATION NOTE:
% Cost of observation decompose into at least three structurally distinct constraints: (1) thermodynamic cost (Landauer's principle, fundamental information processing) — mountain from analytical view; (2) biological cost (metabolic overhead of sensory systems, predation risk) — tangled rope from organism view; (3) institutional cost (surveillance, measurement labor, instrumentation) — snare/piton from observer view. All three are downstream of this meta-constraint; this story captures the unified structure. The decomposition enables analysis of which costs are truly universal vs which are engineering or policy choices.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cost_of_observation, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
