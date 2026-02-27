% ============================================================================
% CONSTRAINT STORY: mco_unit_system_discontinuity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mco_unit_system_discontinuity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mco_unit_system_discontinuity
 *   human_readable: Persistence of Imperial Units in a Metric World
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The persistence of Imperial units (feet, inches, pounds) in aerospace,
 *   aviation, and manufacturing sectors despite global adoption of metric
 *   (SI) is a classic Piton constraint: a former functional requirement that
 *   has degraded into institutional inertia and ritual compliance. The
 *   constraint exhibits low extractiveness (ε=0.18) and high theater ratio
 *   (0.78), indicating that the primary mechanism maintaining Imperial is not
 *   active enforcement but rather the switching cost of legacy tooling, CAD
 *   libraries, supply chains, and engineer training. The theater ratio has
 *   increased over the interval from 0.55 (1970, when Imperial was
 *   functionally integrated) to 0.78 (2010, when metric is globally standard
 *   but Imperial persists through ritual). This trajectory is characteristic
 *   of Piton degradation: the functional reason for maintaining the standard
 *   has faded, but the institutional apparatus persists. Regulatory bodies
 *   (FAA, ESA, DoD) have established metric conversion timelines with sunset
 *   clauses, supporting the Scaffold interpretation for forward-looking
 *   actors, but the compliance mechanisms remain weak. Legacy contractors
 *   face suppression (constraints on exit: tooling cannot be instantly
 *   converted) but do not face severe extraction — maintaining Imperial is
 *   largely a distributed coordination ritual that extracts no concentrated
 *   benefit.
 *
 * KEY AGENTS:
 *   - Legacy Aerospace Contractors: Constrained beneficiary (institutional/constrained) — benefit from massive sunk investments in Imperial tooling/specs; exit is suppressed by switching cost
 *   - Metric-Native Engineers: Powerful beneficiary (powerful/mobile) — metric reduces conversion errors and improves global interoperability; high exit mobility to metric-first firms
 *   - Manufacturing Technicians: Moderate coordinators (moderate/mobile) — bilingual in both systems; maintain compatibility bridges
 *   - Scientific Standards Bodies (ISO, NIST): Organized enforcers (organized/constrained) — declare metric as official standard but cannot unilaterally force industry conversion
 *   - Regulatory Agencies (FAA, DoD, ESA): Organized planners (organized/constrained) — set metric conversion timelines but allow extended waivers
 *   - Metric Conversion Coalition: Organized agents (organized/constrained) — advocate for and implement metric migrations; see constraint as temporary with planned sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing unit system choice as a deep structural feature rather than institutional artifact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mco_unit_system_discontinuity, 0.18).
domain_priors:suppression_score(mco_unit_system_discontinuity, 0.42).
domain_priors:theater_ratio(mco_unit_system_discontinuity, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, extractiveness, 0.18).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mco_unit_system_discontinuity, piton).
narrative_ontology:human_readable(mco_unit_system_discontinuity, "Persistence of Imperial Units in a Metric World").
narrative_ontology:topic_domain(mco_unit_system_discontinuity, "technological/institutional").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, legacy_industrial_contractors).
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, aviation_sector).
narrative_ontology:constraint_beneficiary(mco_unit_system_discontinuity, aerospace_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGACY AEROSPACE CONTRACTOR (PITON) — Constrained by massive sunk investment in Imperial-calibrated tooling, CAD libraries, and supply-chain infrastructure. The constraint persists through institutional inertia and switching cost rather than active enforcement. theater_ratio=0.78 reflects that compliance with Imperial specifications is largely performative — measurements could be expressed in metric but are maintained in feet/inches/pounds for ritual consistency. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.002.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: METRIC-NATIVE SOFTWARE ENGINEER (ROPE) — Sees unit system choice as a pure coordination problem. Metric is mathematically simpler (base-10), more interoperable globally, and reduces conversion errors. The constraint operates as coordination around a legacy standard rather than extraction. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.075. Low effective extraction; the engineer has both skill and mobility.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: SCIENTIFIC STANDARDS BODY (PITON) — SI (metric) is the official standard globally; Imperial persists through institutional resistance and legacy systems rather than through active enforcement of the standard itself. theater_ratio=0.78 reflects the performative nature of maintaining Imperial 'for compatibility' while the functional standard (SI) is what actually drives new research. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.0002.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MANUFACTURING TECHNICIAN (ROPE) — Bilingual in both unit systems. Sees Imperial persistence as a coordination mechanism enabling communication with legacy designs and tooling. The constraint solves the problem of maintaining compatibility with existing infrastructure. d≈0.45, f(d)≈0.58, σ=0.9 → χ≈0.082.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: METRIC CONVERSION INITIATIVE (SCAFFOLD) — Organizations (ISO, national standards bodies, EU regulations) are systematically migrating to metric with sunset timelines for Imperial. This is temporary support with a planned exit: as legacy contractors retire and CAD systems fully adopt metric-first design, Imperial requirements will fade. χ ≤ 0.30 satisfied by explicit sunset clauses in regulatory migration schedules. d≈0.35, f(d)≈0.28, σ=1.1 → χ≈0.058.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a universal/civilizational view, unit system choice is a pure coordination problem with no objective 'best answer': any self-consistent system works. The apparent persistence of Imperial could be framed as an equilibrium that reflects the deep structure of coordination cost. However, ε=0.18 and suppression=0.42 contradict a mountain classification — the engine will flag this as a false summit, revealing that naturalizing the unit system discontinuity disguises what is actually a degraded institutional artifact.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mco_unit_system_discontinuity_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(mco_unit_system_discontinuity, TR),
    TR >= 0.70.

:- end_tests(mco_unit_system_discontinuity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint does not concentrate value extraction. Legacy contractors benefit from avoiding conversion costs, but this is a diffuse coordination equilibrium rather than concentrated rent extraction. No single actor is systematically extracting from others. Suppression (0.42): Moderate. Switching costs for legacy tooling and supply chains suppress exit options for entrenched actors, but the suppression is not total — new entrants can build metric-native systems from the ground up, and partial conversions are possible. Theater ratio (0.78): High. Compliance with Imperial specifications in aerospace is substantially performative. Measurements could be expressed in metric but are maintained in feet/inches/pounds for ritual consistency with legacy documentation. The theater has increased over the interval as the functional reason for Imperial has faded but the institutional apparatus persists. The constraint exhibits all hallmarks of Piton degradation: low extractiveness, high theater, and inertial persistence despite the existence of a superior coordination standard (metric). The interval data shows extractiveness declining slightly (0.22 → 0.18) as functionality shifts to metric, while theater increases (0.55 → 0.78) as compliance becomes purely ritualistic.
 *
 * PERSPECTIVAL GAP:
 *   Legacy contractors see Imperial as a functional necessity for system integration (Rope or Piton). Metric-native engineers see it as a pure coordination problem that metric solves better (Rope). Regulatory bodies see it as a temporary migration problem with sunset timelines (Scaffold). Standards bodies see it as an obsolete standard they have officially replaced but cannot directly eliminate (Piton). The analytical observer risks seeing unit system choice as a deep structural feature (Mountain — any consistent system works), but the structural data (low ε, high theater, explicit regulatory sunsets) reveals this as a false summit. The real constraint is institutional inertia, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Legacy aerospace contractors: Beneficiary + constrained → d≈0.05, f(d)≈-0.12. They benefit from avoiding conversion costs but exit is suppressed by switching costs; the net effect is slightly negative extraction (they are subsidized by the constraint). Metric-native engineers: Powerful + mobile → d≈0.45, f(d)≈0.58. They experience the constraint as a coordination inefficiency but have high exit mobility. Manufacturing technicians: Moderate + mobile → d≈0.45, f(d)≈0.58. Bilingual coordinators with low extraction exposure. Regulatory agencies: Organized + constrained → d≈0.35, f(d)≈0.28. They have declared metric standard but cannot force compliance — constrained power. Analytical observer: analytical → d≈0.72, f(d)≈1.15. The false summit test catches the naturalization: unit system choice is not a mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   The Piton classification resolves the mandatrophy by correctly identifying that Imperial persistence is institutional inertia rather than active functional coordination (Rope) or concentrated extraction (Snare). The theater ratio (0.78) confirms that compliance is largely ritualistic — measurements could be metric but are maintained Imperial for symbolic compatibility. The explicit regulatory sunset clauses (FAA, ESA, DoD metric conversion timelines) confirm that this is a degraded constraint: the forward-looking institutions have declared it temporary. The low extractiveness (0.18) rules out Snare. The high theater rules out Rope. The Piton classification captures the real mechanism: a former functional standard maintained through institutional inertia despite the availability of a superior standard (metric). The constraint will degrade further as legacy contractors retire and metric-native design becomes the universal standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_ritual_compliance,
    'How much of Imperial persistence is functional (technical incompatibility) versus ritual (symbolic/organizational inertia)?',
    'Process analysis of conversions in actual manufacturing workflows; measurement of errors introduced by Imperial vs metric specifications; tracking of conversion-step elimination as systems migrate',
    'If primarily functional: constraint is rope-like coordination. If primarily ritual: constraint is degraded piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_ritual_compliance, empirical, 'Functional vs ritual compliance in unit system persistence').

omega_variable(
    switching_cost_asymmetry,
    'What is the actual switching cost (tooling, CAD migration, supply chain recalibration) for legacy contractors versus new market entrants?',
    'Cost analysis of metric conversion for major aerospace/defense contractors; comparison with conversion costs for startups and new entrants; timeline data for completed migrations',
    'If switching cost is low: constraint is theater-based inertia (piton). If switching cost is very high: constraint may be snare-like extraction against contractors forced to maintain dual systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_asymmetry, empirical, 'Switching cost asymmetry between legacy and new entrants').

omega_variable(
    regulatory_sunset_enforceability,
    'Are metric conversion timelines enforced by regulators, or do they allow indefinite Imperial waivers?',
    'Policy analysis of FAA, DoD, ESA regulations; tracking of deadline extensions or waivers granted; measurement of actual vs planned conversion timelines',
    'If enforced: scaffold perspective is correct; constraint has real sunset. If repeatedly waived: scaffold is aspirational; constraint is piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_sunset_enforceability, empirical, 'Enforceability of regulatory metric conversion deadlines').

omega_variable(
    global_competitive_advantage,
    'Does maintaining Imperial give domestic contractors competitive advantage (cost savings from legacy systems) or disadvantage (incompatibility with global supply chains)?',
    'Market share analysis for Imperial-compliant vs metric-first contractors; cost structure comparison; client survey on unit system preferences',
    'If advantage: beneficiary group is correct, extraction is subtle. If disadvantage: persistence is purely inertial, constraint is degraded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_competitive_advantage, empirical, 'Competitive advantage or disadvantage of Imperial persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mco_unit_system_discontinuity, 1970, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mco_tr_t0, mco_unit_system_discontinuity, theater_ratio, 0, 0.55).
narrative_ontology:measurement(mco_tr_t20, mco_unit_system_discontinuity, theater_ratio, 20, 0.68).
narrative_ontology:measurement(mco_tr_t40, mco_unit_system_discontinuity, theater_ratio, 40, 0.78).

% Extraction over time
narrative_ontology:measurement(mco_be_t0, mco_unit_system_discontinuity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mco_be_t20, mco_unit_system_discontinuity, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(mco_be_t40, mco_unit_system_discontinuity, base_extractiveness, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mco_unit_system_discontinuity, information_standard).
narrative_ontology:affects_constraint(mco_unit_system_discontinuity, aerospace_supply_chain_fragmentation).
narrative_ontology:affects_constraint(mco_unit_system_discontinuity, manufacturing_process_standardization).

% DUAL FORMULATION NOTE:
% The Imperial persistence constraint is downstream of historical path-dependency (US industrial dominance in mid-20th century locked in Imperial standards), but functions as a distinct structural phenomenon with its own metrics and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
