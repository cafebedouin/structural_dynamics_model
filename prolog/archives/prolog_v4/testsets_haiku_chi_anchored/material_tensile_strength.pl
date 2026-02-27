% ============================================================================
% CONSTRAINT STORY: material_tensile_strength
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_material_tensile_strength, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: material_tensile_strength
 *   human_readable: Ultimate Tensile Strength (UTS) as Physical Limit
 *   domain: technological/materials_science
 *
 * SUMMARY:
 *   Ultimate Tensile Strength (UTS) is the maximum engineering stress a
 *   material can withstand before fracture occurs. This constraint is
 *   fundamentally a property of atomic bonding architecture — the interatomic
 *   forces, crystalline lattice structure, and defect density determine the
 *   maximum load-bearing capacity of any given material. UTS appears as a
 *   true natural law from all major perspectives: it is not the product of
 *   institutional arrangement, coordination failure, or extractive power, but
 *   rather an irreducible limit imposed by the physics of interatomic
 *   bonding. The constraint exhibits zero degrees of freedom — no agent can
 *   negotiate, suppress, or arbitrage around it. The only manipulation
 *   available is material selection and design (choosing a stronger material
 *   for the application), which does not change the physical property itself.
 *   The theater ratio is very low (0.15) because measurement of UTS is
 *   directly functional: standardized tensile tests produce actionable
 *   engineering data with minimal performative content.
 *
 * KEY AGENTS:
 *   - Physical scientist: Analyst (analytical/analytical) — observes UTS as fundamental interatomic physics
 *   - Materials engineer: Institutional designer (institutional/arbitrage) — treats UTS as a fixed design constraint requiring material selection
 *   - Component user: Operational actor (powerless/trapped) — experiences UTS as an absolute ceiling on load capacity
 *   - Standards organization: Coordinating body (organized/mobile) — establishes measurement protocols (rope function, not mountain)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(material_tensile_strength, 0.12).
domain_priors:suppression_score(material_tensile_strength, 0.03).
domain_priors:theater_ratio(material_tensile_strength, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(material_tensile_strength, extractiveness, 0.12).
narrative_ontology:constraint_metric(material_tensile_strength, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(material_tensile_strength, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(material_tensile_strength, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(material_tensile_strength, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(material_tensile_strength, mountain).
narrative_ontology:human_readable(material_tensile_strength, "Ultimate Tensile Strength (UTS) as Physical Limit").
narrative_ontology:topic_domain(material_tensile_strength, "technological/materials_science").

domain_priors:emerges_naturally(material_tensile_strength).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER / UNIVERSAL VIEW (MOUNTAIN) — Ultimate tensile strength is a deterministic physical property derived from interatomic bonding strength, crystalline structure, and atomic spacing. The maximum stress a material can sustain before fracture is an irreducible property of its atomic architecture. No agent, institution, or measurement framework can alter this limit — it emerges necessarily from fundamental physics. ε=0.12, suppression=0.03, accessibility_collapse=0.92, resistance=0.08. Universal scope, civilizational timescale.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATERIALS ENGINEER / INSTITUTIONAL VIEW (MOUNTAIN) — From an engineering design perspective, UTS is a non-negotiable material property constraint. Engineers cannot extract value from this limit or suppress it through institutional means. The constraint is fixed: a steel beam with UTS 400 MPa cannot be redesigned to have UTS 800 MPa without changing the material entirely. This is a coordination function (material selection) with zero degrees of freedom regarding the physical limit itself. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPONENT USER / OPERATIONAL VIEW (MOUNTAIN) — From the perspective of someone using a material in a specific application, UTS is an absolute ceiling. A rope rated for 500 kg cannot safely carry 1000 kg — the material fails. The user cannot negotiate, override, or escape this limit. The constraint is experientially real and inescapable. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.14. Despite high directionality value, the mountain gate holds: ε=0.12 ≤ 0.25, suppression=0.03 ≤ 0.05, emerges_naturally=true.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: STANDARDS ORGANIZATION / MEASUREMENT VIEW (ROPE) — ASTM, ISO, and other standards bodies coordinate how UTS is measured and reported. This is pure coordination without extraction: the standards create a common language for material properties, enabling commerce and engineering safety. No agent extracts surplus from the coordination; it solves a collective action problem (heterogeneous testing → uniform reporting). ε=0.05, χ≤0.35. This perspective reveals that the measurement infrastructure around UTS is a rope, but the physical property itself remains a mountain.
constraint_indexing:constraint_classification(material_tensile_strength, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(material_tensile_strength_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(material_tensile_strength, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(material_tensile_strength, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(material_tensile_strength, ExtMetricName, E),
    domain_priors:suppression_score(material_tensile_strength, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(material_tensile_strength),
    narrative_ontology:constraint_metric(material_tensile_strength, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(material_tensile_strength, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(material_tensile_strength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.12): Very low. UTS is not an extractive mechanism. No agent can extract surplus from the constraint itself — the constraint is a property of the material, not a social arrangement. The value reflects minimal institutional overhead in recognizing and applying the limit. Suppression (0.03): Near-zero. There is no suppression of alternatives because there are no alternatives — the material property is what it is. Accessibility collapse (0.92): Very high. The physical limit is completely accessible to any observer with standard materials testing equipment. No obfuscation or hidden complexity exists. Resistance (0.08): Very low. No real-world actor resists the acceptance of UTS as a physical property. It is empirically verifiable and universally accepted. Theater ratio (0.15): Low. Tensile testing is highly functional: apply load, measure force at failure, record stress value. Minimal theatrical performance or status signaling.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap because UTS classifies as Mountain from all strategic viewpoints. The physical scientist and materials engineer both see the same physics; the engineer treats it as a design parameter (choosing which material to use), while the scientist examines the underlying mechanism. The component user experiences the limit as binding but does not perceive variation in how the constraint operates. The standards organization's perspective (rope) is not about UTS itself but about how we measure and communicate UTS — the coordination function is in the measurement protocol, not in the physical property. Unlike constraints with genuine asymmetric power relationships, UTS is democratic: the same limit applies to all agents regardless of their institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no meaningful directionality structure because it is not an extraction or coordination relationship. There are no beneficiaries or victims — UTS is a universal physical constraint that binds all actors equally. The directionality calculations would derive d-values based on exit options and power, but these do not affect the mountain classification because ε=0.12 and suppression=0.03 are so far below the thresholds that f(d) and σ(S) scaling produce negligible effective extraction values across all perspectives. The formula χ = ε × f(d) × σ(S) yields χ ≤ 0.18 even at maximum directionality (d=1.0, f(d)=1.42, σ=1.2), which is consistent with a mountain type.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quantum_uncertainty_boundary,
    'Is UTS fundamentally a quantum mechanical limit or a classical statistical effect of defect populations?',
    'Direct comparison of quantum field calculations vs continuum mechanics fracture models; examination of whether quantum tunneling through energy barriers affects macroscopic failure stress',
    'If quantum mechanical: UTS is a true natural law. If classical/statistical: UTS emerges from defect distribution and could theoretically be engineered away. Mountain classification remains stable either way, but the underlying mechanism changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantum_uncertainty_boundary, empirical, 'Quantum vs classical origin of tensile strength limit').

omega_variable(
    material_composition_discretization,
    'Can arbitrary compositions with arbitrary atomic arrangements achieve any UTS value, or are there fundamental discrete jumps in achievable strength values?',
    'Comprehensive high-throughput materials discovery; testing whether the space of achievable UTS values is continuous or has gaps; examining Hume-Rothery rules and phase diagram constraints',
    'If continuous: UTS is a smooth physical function. If discrete: there are forbidden strength values (structural limit on material space itself).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_composition_discretization, empirical, 'Continuity of achievable UTS values across material space').

omega_variable(
    defect_engineering_ceiling,
    'Can controlled nanoscale defect engineering (grain boundaries, dislocations, vacancies) push against the fundamental UTS limit, or is there a hard ceiling independent of defect population?',
    'Experimental investigation of ultrafine-grained and defect-free whisker materials; comparison of theoretical predictions from continuum mechanics vs quantum mechanics; high-strain-rate testing',
    'If defect engineering can push the limit: UTS has some contingency (quasi-mountain). If hard ceiling exists: mountain classification confirmed universally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(defect_engineering_ceiling, empirical, 'Whether defect engineering can transcend fundamental UTS limit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(material_tensile_strength, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uts_tr_t0, material_tensile_strength, theater_ratio, 0, 0.1).
narrative_ontology:measurement(uts_tr_t5, material_tensile_strength, theater_ratio, 5, 0.14).
narrative_ontology:measurement(uts_tr_t10, material_tensile_strength, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(uts_be_t0, material_tensile_strength, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(uts_be_t5, material_tensile_strength, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(uts_be_t10, material_tensile_strength, base_extractiveness, 10, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(material_tensile_strength, information_standard).
narrative_ontology:affects_constraint(material_tensile_strength, elastic_modulus).
narrative_ontology:affects_constraint(material_tensile_strength, fracture_toughness).
narrative_ontology:affects_constraint(material_tensile_strength, yield_strength).

% DUAL FORMULATION NOTE:
% UTS is upstream of elastic modulus and fracture toughness because it determines the maximum stress regime. These three constraints form a materials mechanics family. However, they are structurally distinct: elastic modulus (the slope of the stress-strain curve) is also a mountain, while fracture toughness (resistance to propagating cracks) involves microstructure defect populations and could decompose into snare and rope perspectives depending on measurement methodology and material design choices. UTS itself remains uniformly mountain across all decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
