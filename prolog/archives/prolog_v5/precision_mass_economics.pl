% ============================================================================
% CONSTRAINT STORY: precision_mass_economics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_precision_mass_economics, []).

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
 *   constraint_id: precision_mass_economics
 *   human_readable: Precision-Mass Economics in Asymmetric Warfare
 *   domain: military_innovation/asymmetric_warfare/energy_infrastructure
 *
 * SUMMARY:
 *   The precision-mass economics constraint describes the cost asymmetry
 *   where low-cost precision-guided munitions (drones, loitering munitions)
 *   can destroy high-value infrastructure at ratios exceeding 1000:1. A $1000
 *   commercial drone carrying a shaped charge can disable a $100M refinery,
 *   and the refinery operator cannot harden against this threat without costs
 *   that exceed the replacement value of the infrastructure itself. This
 *   constraint is presented as a mountain — an immutable feature of the
 *   current technological landscape — but the presence of identifiable
 *   beneficiaries (drone manufacturers, asymmetric warfare strategists,
 *   resource-constrained states challenging conventional military powers)
 *   triggers the false summit detector. The omega variables document the
 *   irreducible uncertainty: is this a natural law of technological
 *   maturation, or a constructed constraint shaped by R&D investment, export
 *   policy, and institutional procurement decisions? The measurements show
 *   declining extractiveness and theater over the 2014-2024 interval,
 *   reflecting increasing technological maturity and decreasing performative
 *   content as the cost asymmetry becomes undeniable. Observable: Russian
 *   refinery capacity reduced to 4.69M bpd (lowest since 2009) via Ukrainian
 *   drone strikes; unit costs $1000-$5000 per drone vs $50M-$200M per
 *   refinery; reconstruction timelines 6-18 months vs strike frequency
 *   measured in days.
 *
 * KEY AGENTS:
 *   - Targeted Infrastructure Operator: Primary victim candidate (powerless/trapped) — cannot exit the cost asymmetry; defensive hardening costs exceed replacement value; experiences the constraint as immutable physical law
 *   - Conventional Military Planner: Institutional actor (institutional/constrained) — constrained by procurement cycles and doctrine but recognizes the underlying physics; cannot suppress the cost inversion through policy
 *   - Asymmetric Warfare Strategist: Primary beneficiary (institutional/arbitrage) — exploits the cost inversion but did not create it; benefits from technological maturation curves outside their control
 *   - Drone Manufacturers: Secondary beneficiary (institutional/arbitrage) — commercial drone market learning curves create the cost asymmetry; dual-use technology proliferation is unstoppable
 *   - Resource-Constrained Actors: Tertiary beneficiary (organized/mobile) — states and non-state actors that could not challenge conventional forces via symmetric means now have access to precision strike capability at commodity prices
 *   - Defense Economics Analyst: Analytical observer (analytical/analytical) — measures the cost ratio as a structural feature of technological maturity; risks naturalizing what may be a constructed constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(precision_mass_economics, 0.08).
domain_priors:suppression_score(precision_mass_economics, 0.03).
domain_priors:theater_ratio(precision_mass_economics, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(precision_mass_economics, extractiveness, 0.08).
narrative_ontology:constraint_metric(precision_mass_economics, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(precision_mass_economics, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(precision_mass_economics, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(precision_mass_economics, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(precision_mass_economics, mountain).
narrative_ontology:human_readable(precision_mass_economics, "Precision-Mass Economics in Asymmetric Warfare").
narrative_ontology:topic_domain(precision_mass_economics, "military_innovation/asymmetric_warfare/energy_infrastructure").

domain_priors:emerges_naturally(precision_mass_economics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(precision_mass_economics, resource_constrained_actors).
narrative_ontology:constraint_beneficiary(precision_mass_economics, drone_manufacturers).
narrative_ontology:constraint_beneficiary(precision_mass_economics, asymmetric_warfare_strategists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED INFRASTRUCTURE (MOUNTAIN) — Cannot exit the cost asymmetry. A $100M refinery faces $1000 drones regardless of defensive posture. The physics of precision guidance, mass production, and explosive energy density are immutable constraints. No amount of hardening changes the fundamental economics — defense costs scale linearly with perimeter while offense costs scale with unit production.
constraint_indexing:constraint_classification(precision_mass_economics, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CONVENTIONAL MILITARY (MOUNTAIN) — Constrained by doctrine and procurement cycles, but the underlying constraint is physical. The cost asymmetry derives from thermodynamics (explosive energy density), information theory (precision guidance bandwidth requirements), and manufacturing economics (learning curves for mass production). Cannot be negotiated away or suppressed through policy.
constraint_indexing:constraint_classification(precision_mass_economics, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ASYMMETRIC STRATEGIST (MOUNTAIN) — Benefits from the cost inversion but did not create it. The constraint emerges from technological maturation curves: precision guidance miniaturization, commercial drone component commodification, and explosive chemistry. The strategist exploits a natural law of the current technological epoch, not a constructed advantage.
constraint_indexing:constraint_classification(precision_mass_economics, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The precision-mass cost inversion is a structural feature of the current technological landscape, not a policy choice. It derives from: (1) Moore's Law effects on guidance systems, (2) commercial drone market learning curves, (3) explosive energy density limits, (4) infrastructure replacement costs driven by economies of scale. These are physical and economic constraints, not institutional arrangements. The 1000:1 cost ratio is a measurement of technological maturity, not extractive design.
constraint_indexing:constraint_classification(precision_mass_economics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(precision_mass_economics_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(precision_mass_economics, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(precision_mass_economics, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(precision_mass_economics, ExtMetricName, E),
    domain_priors:suppression_score(precision_mass_economics, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(precision_mass_economics),
    narrative_ontology:constraint_metric(precision_mass_economics, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(precision_mass_economics, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(precision_mass_economics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The cost asymmetry is not designed to extract from infrastructure operators — it emerges from the convergence of precision guidance miniaturization, commercial drone commodification, and explosive energy density limits. The 'extraction' is a side effect of technological maturation, not a primary function. The value is above zero because identifiable actors do benefit (drone manufacturers capture market share, asymmetric strategists gain capability), but the benefit is not the constraint's purpose. Suppression (0.03): Negligible. Countermeasures are not being actively suppressed — they are either physically infeasible at scale (directed energy weapons remain expensive and range-limited) or economically nonviable (hardening a refinery against precision strikes costs more than the refinery). The low suppression reflects that alternatives are not being blocked; they simply don't exist yet. Theater ratio (0.12): Very low. The cost asymmetry is measurable and undeniable. There is minimal performative content — the 1000:1 ratio is derived from observable unit costs and replacement costs, not from institutional ritual. The slight theater component reflects doctrinal inertia (conventional militaries still procure as if symmetric warfare dominates) but the underlying economics are transparent. Accessibility collapse (0.92): Very high. All actors, regardless of resources or position, face the same cost asymmetry. A state-of-the-art air defense system cannot change the fundamental economics — precision guidance is cheap, infrastructure is expensive, and explosive energy density is a physical constant. Resistance (0.08): Very low. Attempts to resist the constraint (hardening, active defense, distributed infrastructure) face prohibitive costs. The constraint is nearly immutable within the current technological epoch.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as mountain. The targeted infrastructure operator, the conventional military planner, the asymmetric warfare strategist, and the analytical observer all agree that the cost asymmetry is a structural feature of the current technological landscape. The gap that does exist is in the interpretation of beneficiary presence: does it indicate a false summit (constructed constraint naturalized as physical law), or is it incidental (beneficiaries exploiting a genuine natural law)? The omega variables document this ambiguity. The false summit detector will flag this constraint for review because beneficiaries are declared on a mountain, but the structural data supports the mountain classification — the cost asymmetry derives from measurable physical and economic constraints (Moore's Law, learning curves, energy density, replacement costs), not from institutional arrangements that could be changed by policy.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (resource-constrained actors, drone manufacturers, asymmetric warfare strategists) are declared to trigger the false summit detector, but their directionality values remain low because they did not construct the constraint — they are exploiting a natural feature of technological maturation. The targeted infrastructure operator is not declared as a victim because the constraint does not extract from them in the DR sense — the cost asymmetry is not designed to transfer value from refineries to drone operators. The 'victim' framing would imply intentional extraction, but the constraint emerges from physics and economics, not from institutional design. The infrastructure operator experiences the constraint as a mountain (immutable), and the analytical observer agrees. The perspectival gap is minimal — all agents see the same structural reality. The false summit question is whether the beneficiaries' presence indicates that the constraint was shaped by their influence (R&D investment, export policy, dual-use technology transfer) rather than emerging purely from technological evolution. If the former, the mountain classification naturalizes a constructed advantage. If the latter, the mountain classification is accurate and the beneficiaries are incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that a mountain can have beneficiaries without being a false summit, IF the beneficiaries did not shape the constraint's emergence. The precision-mass cost inversion benefits drone manufacturers and asymmetric strategists, but it was not designed by them — it emerged from the convergence of commercial technology maturation curves (smartphone sensors enabling precision guidance, lithium battery energy density enabling loitering time, 3D printing enabling rapid prototyping). The beneficiaries are exploiting a natural law of the current technological epoch, not maintaining a constructed advantage. The mandatrophy question is: 'Can a mountain have beneficiaries?' The answer is yes, if the beneficiaries are incidental rather than causal. The omega variables document the empirical tests that would distinguish incidental from causal: Did the beneficiaries influence the technological trajectory? Are countermeasures being suppressed institutionally or are they physically infeasible? Is the asymmetry permanent or transient? These questions are resolvable through historical analysis and technological forecasting, making the mandatrophy empirically tractable rather than conceptually undecidable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_naturalization,
    'Is the mountain classification naturalizing what is actually a constructed constraint that benefits identifiable actors (drone manufacturers, asymmetric warfare strategists, resource-constrained states)?',
    'Historical analysis: Did the cost asymmetry emerge from natural technological evolution, or was it shaped by specific R&D investment decisions, export control policies, or dual-use technology transfer? Examine whether the beneficiaries influenced the technological trajectory.',
    'If beneficiaries shaped the trajectory: reclassify as tangled_rope (coordination around precision strike capability with embedded extraction favoring first-movers). If purely emergent: mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalization, empirical, 'Whether beneficiary presence indicates constructed constraint vs natural technological evolution').

omega_variable(
    countermeasure_suppression,
    'Are effective countermeasures (electronic warfare, directed energy weapons, hardened infrastructure) being suppressed by institutional inertia, or are they genuinely infeasible at scale?',
    'Technical feasibility analysis of countermeasures; cost-effectiveness comparison of active defense vs passive hardening vs distributed infrastructure; examination of procurement barriers (institutional) vs physical barriers (technological).',
    'If countermeasures are institutionally suppressed: reclassify as snare (extraction mechanism maintained by suppressing alternatives). If physically infeasible: mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(countermeasure_suppression, empirical, 'Whether countermeasure absence is institutional suppression or physical constraint').

omega_variable(
    technological_epoch_duration,
    'Is this cost asymmetry a permanent feature of precision warfare or a temporary window before countermeasures mature?',
    'Technological trajectory analysis: learning curves for directed energy weapons, AI-enabled point defense, distributed energy infrastructure. Historical precedent: how long did previous offense-defense asymmetries persist (e.g., aircraft carriers vs anti-ship missiles, tanks vs anti-tank weapons)?',
    'If temporary (10-20 year window): reclassify as scaffold (coordination around current-generation precision strike with sunset as countermeasures mature). If permanent: mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_epoch_duration, empirical, 'Whether the cost asymmetry is a permanent technological feature or a transient window').

omega_variable(
    dual_use_export_control,
    'Do export controls on dual-use drone components constitute active enforcement that maintains the asymmetry, or are they ineffective against commercial proliferation?',
    'Analysis of export control effectiveness: black market availability, commercial component substitutability, indigenous production capacity in resource-constrained states. Comparison of controlled vs uncontrolled technology diffusion rates.',
    'If export controls are effective and maintained by beneficiaries: reclassify as tangled_rope (coordination around technology access with embedded extraction). If ineffective: mountain classification holds (commercial proliferation is unstoppable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_export_control, empirical, 'Whether export controls constitute active enforcement maintaining the asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(precision_mass_economics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pme_theater_2014, precision_mass_economics, theater_ratio, 0, 0.15).
narrative_ontology:measurement(pme_theater_2017, precision_mass_economics, theater_ratio, 3, 0.14).
narrative_ontology:measurement(pme_theater_2020, precision_mass_economics, theater_ratio, 6, 0.13).
narrative_ontology:measurement(pme_theater_2024, precision_mass_economics, theater_ratio, 10, 0.12).

% Extraction over time
narrative_ontology:measurement(pme_extract_2014, precision_mass_economics, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(pme_extract_2017, precision_mass_economics, base_extractiveness, 3, 0.1).
narrative_ontology:measurement(pme_extract_2020, precision_mass_economics, base_extractiveness, 6, 0.09).
narrative_ontology:measurement(pme_extract_2024, precision_mass_economics, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(precision_mass_economics, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is a candidate for decomposition if future analysis reveals that the 'precision-mass economics' label conflates multiple structurally distinct claims: (1) the cost asymmetry itself (physical/economic constraint), (2) the suppression of countermeasures (institutional constraint), (3) the export control regime (policy constraint). Current formulation treats these as a unified mountain, but if omega variable resolution shows that countermeasures are institutionally suppressed or export controls are maintaining the asymmetry, decompose into separate stories with different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
