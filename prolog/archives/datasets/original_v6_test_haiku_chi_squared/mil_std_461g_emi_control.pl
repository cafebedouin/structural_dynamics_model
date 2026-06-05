% ============================================================================
% CONSTRAINT STORY: mil_std_461g_emi_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mil_std_461g_emi_control, []).

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
 *   constraint_id: mil_std_461g_emi_control
 *   human_readable: MIL-STD-461G EMI Control Standard
 *   domain: technological/military_standards
 *
 * SUMMARY:
 *   MIL-STD-461G establishes interface and verification requirements for
 *   controlling electromagnetic interference in military subsystems and
 *   equipment. The standard creates a bifurcated supplier ecosystem: large
 *   defense primes and established contractors can amortize compliance costs
 *   across contracts, negotiate exemptions, and influence standard revision;
 *   small component suppliers face absolute compliance requirements with
 *   limited exit options. The constraint exhibits both genuine coordination
 *   function (EMI testing prevents incompatible subsystems from integrating)
 *   and asymmetric extraction (compliance cost barrier protects incumbent
 *   suppliers from competition). The theater_ratio (0.58) reflects that the
 *   standard's verification methodology has become increasingly performative
 *   relative to actual EMI risk: checklist compliance testing follows rigid
 *   frequency sweeps and coupling scenarios that may not capture dominant
 *   failure modes in specific system architectures, yet deviation requires
 *   costly test-plan waivers. Over the 20-year interval, theater has
 *   increased as electronics complexity has outpaced the standard's revision
 *   cycles, while extractiveness has grown through the accumulation of
 *   compliance infrastructure costs without fundamental methodology
 *   advancement.
 *
 * KEY AGENTS:
 *   - Small Component Suppliers: Primary victim (powerless/trapped) — cannot enter military supply chain without 461G certification; lacks resources for expensive test chambers
 *   - Mid-Size Defense Contractors: Secondary victim (moderate/constrained) — bears compliance burden but benefits from standardized interfaces
 *   - Defense Prime Contractors: Primary beneficiary (institutional/arbitrage) — can negotiate compliance waivers and exemptions; passes costs to subcontractors
 *   - Military Procurement Authority: Primary beneficiary (institutional/arbitrage) — standardized interface reduces integration risk and procurement complexity
 *   - Innovation Engineering Community: Organized actors (organized/constrained) — developing computational EMI prediction as alternative to empirical testing with sunset potential
 *   - Standards Body (IEEE/NIST): Institutional actor maintaining performative revision cycle (institutional/arbitrage) — benefits from compliance infrastructure market without incentive to accelerate obsolescence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mil_std_461g_emi_control, 0.38).
domain_priors:suppression_score(mil_std_461g_emi_control, 0.52).
domain_priors:theater_ratio(mil_std_461g_emi_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mil_std_461g_emi_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(mil_std_461g_emi_control, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(mil_std_461g_emi_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mil_std_461g_emi_control, tangled_rope).
narrative_ontology:human_readable(mil_std_461g_emi_control, "MIL-STD-461G EMI Control Standard").
narrative_ontology:topic_domain(mil_std_461g_emi_control, "technological/military_standards").

domain_priors:requires_active_enforcement(mil_std_461g_emi_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mil_std_461g_emi_control, defense_prime_contractors).
narrative_ontology:constraint_beneficiary(mil_std_461g_emi_control, military_procurement_authority).
narrative_ontology:constraint_victim(mil_std_461g_emi_control, small_component_suppliers).
narrative_ontology:constraint_victim(mil_std_461g_emi_control, innovation_speed).
narrative_ontology:constraint_victim(mil_std_461g_emi_control, cost_sensitive_contractors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL COMPONENT SUPPLIER (SNARE) — Trapped by military procurement: cannot sell subsystems without MIL-STD-461G certification, certification requires expensive test chambers and specialized expertise, no alternative market path exists. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-SIZE DEFENSE CONTRACTOR (TANGLED ROPE) — Constrained by compliance cost but benefits from standardized interface specification: 461G prevents ad-hoc EMI problems in integrated systems. Beneficiary of coordination function; victim of compliance burden. d≈0.58, f(d)≈0.70, σ=1.0 → χ≈0.27.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEFENSE PRIME CONTRACTOR (ROPE) — Institutional beneficiary with arbitrage exit: can negotiate exemptions, can influence standard revision cycles, can pass compliance costs to subcontractors while capturing integration value. 461G as coordination mechanism: reduces subsystem incompatibility risk. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MILITARY PROCUREMENT AUTHORITY (ROPE) — Institutional beneficiary: standardized interface reduces procurement complexity, enables modular system design, reduces integration risk. Coordination function: 461G ensures subsystems work together without ad-hoc workarounds. Can modify standard and enforce exemptions. d≈0.10, f(d)≈-0.07, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INNOVATION ENGINEERING COMMUNITY (SCAFFOLD) — Organized actors (academic labs, startup incubators, agile defense contractors) see 461G as a temporary bottleneck with a sunset: modular simulation tools (digital twins, virtual EMI testing) are creating alternative verification pathways. As computational EMI prediction matures, the empirical test-chamber requirement loses force. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.22. Sunset estimated at 15-25 years as computational methods achieve flight-critical confidence.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STANDARDS BODY / IEEE-NIST INTERSECTION (PITON) — The standards maintenance process itself is largely performative: 461G revisions (461E→461F→461G) occur every 5-10 years but rarely introduce fundamentally new test methodologies. The standard persists through institutional inertia and procurement bureaucracy, not because its verification methods are optimized for modern electronics. theater_ratio=0.58 reflects substantial performative content: compliance auditing follows checklist logic rather than true EMI risk assessment. The standard works but could be vastly more efficient.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, EMI coupling is a fundamental physical phenomenon: no military system can avoid it entirely, and some form of interface specification is a logical necessity. This view risks naturalizing the *specific* compliance apparatus (test chambers, frequency ranges, measurement procedures) as inevitable when only the *general* need for EMI control is physically constrained. The structural data (ε=0.38, suppression=0.52) contradicts strict mountain classification — the engine will flag this as a false summit revealing the contingent enforcement mechanism beneath the physical necessity.
constraint_indexing:constraint_classification(mil_std_461g_emi_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mil_std_461g_emi_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mil_std_461g_emi_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(mil_std_461g_emi_control, TR),
    TR >= 0.70.

:- end_tests(mil_std_461g_emi_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The compliance burden is substantial and creates genuine barriers to entry for small suppliers (cost multiplier likely 2.5-4.0x). However, extractiveness is not maximal because the underlying EMI control requirement is physically real — some form of interface specification cannot be avoided. The extraction comes primarily from (a) the specific compliance apparatus chosen (empirical test chambers vs computational alternatives) and (b) the ability of incumbents to maintain closure. Suppression (0.52): Moderate-high. Significant barriers exist: test chambers are expensive, specialized expertise is required, alternative market paths are minimal (military procurement is the primary use case for certified subsystems), and compliance timelines create schedule pressure. However, suppression is not total — some alternative pathways exist through civilian standards, through direct negotiation of waivers, and through newer companies entering with lower cost models. Theater ratio (0.58): Moderate. The standard's verification methodology is substantially performative: compliance testing follows rigid frequency sweeps and coupling scenarios that may not capture dominant failure modes in specific architectures. However, the theater is not dominant (>0.70) because the empirical testing does prevent some real incompatibilities, and the specification itself solves a genuine coordination problem.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows a sharp perspectival gap between suppliers and beneficiaries. Small suppliers see a snare (trapped, no exit). Mid-size contractors see tangled rope (mixed coordination and extraction). Primes and procurement see pure rope (coordination benefit with minimal cost to them). The innovation community sees a temporary scaffold with a sunset as computational methods mature. The standards body sees a degraded piton (the revision process is inertial, not optimized). The civilizational observer risks seeing a mountain (EMI control as physical necessity) but the structural data reveals this as a false summit — the specific apparatus is contingent institutional choice, not physical law. The perspectival gap arises because different agents have different exit costs: primes can negotiate exemptions; suppliers cannot; innovation actors see technological paths around the constraint; the standards body has no incentive to accelerate its own obsolescence.
 *
 * DIRECTIONALITY LOGIC:
 *   Small component suppliers: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction. Cannot exit without losing military procurement channel entirely. Mid-size contractors: Victim + constrained (but also beneficiary of coordination function) → d≈0.58, f(d)≈0.70. Mixed extraction and benefit. Can negotiate but face real constraints. Defense primes: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary with strong exit options (can negotiate exemptions, can influence standard). Procurement authority: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.07. Net beneficiary, can modify or enforce standards. Innovation community: Organized + constrained → d≈0.42, f(d)≈0.42. Moderate extraction from compliance costs but sees technological escape path (scaffold sunset). Standards body: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater gate, not directionality — standards body maintains the apparatus for institutional continuity, not for extraction (though that is a side effect).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint resolves the mandatrophy by distinguishing the *physical necessity* of EMI control (which approaches a mountain) from the *specific institutional apparatus* chosen to enforce it (which is contingent, extractive, and politically constructed). The constraint ID and metrics refer to the apparatus, not the underlying physics. The analytical observer's mountain is a false summit: the engine detects this through the ε=0.38 value (too high for a true mountain) combined with the enforcement flag. The small supplier's snare is real (trapped by market structure). The prime contractor's rope is real (genuine coordination benefit). The innovation community's scaffold is real (computational alternatives approaching maturity). The standards body's piton is real (revision cycle driven by bureaucratic inertia, not technical optimization). All six types are legitimate readings of the constraint from specific structural positions. No single type 'is' MIL-STD-461G — the presheaf IS the answer. The mandatrophy resolution is: the constraint is a tangled rope (legitimate coordination with asymmetric extraction) that presents as a mountain (physical necessity) to distract from its extraction mechanism. Unmasking this presupposes the organizational perspective (institutional actors benefit and maintain the apparatus) and the supplier perspective (powerless agents trapped by the apparatus). From neither perspective alone is the full structure visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_validation_confidence,
    'At what computational prediction accuracy does MIL-STD-461G''s empirical test-chamber requirement become obsolete?',
    'Correlation analysis between simulated EMI predictions and physical test results across diverse subsystem types; error rates and confidence intervals as simulation fidelity improves over time',
    'If achievable at <5% error: scaffold sunset timeline accelerates (10-15 years). If asymptotic at >15% error: physical testing remains mandatory indefinitely, killing the sunset hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_validation_confidence, empirical, 'Computational EMI prediction accuracy threshold for empirical test obsolescence').

omega_variable(
    supplier_carteling_extraction,
    'Are defense contractors using 461G compliance requirements to maintain barriers to entry for new suppliers, beyond what the technical standard itself requires?',
    'Analysis of EMI test cost variance across identical subsystem types; comparison of supplier entry rates pre-standard vs post-standard; investigation of whether ''approved test labs'' restriction creates artificial scarcity',
    'If significant carteling detected: constraint is primarily extractive snare (beneficiaries extracting via monopolistic access to compliance infrastructure). If minimal: constraint is primarily coordination with legitimate efficiency costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supplier_carteling_extraction, empirical, 'Whether 461G is weaponized for supplier exclusion beyond technical necessity').

omega_variable(
    actual_emi_failure_correlation,
    'What fraction of real EMI failures in deployed military systems would have been caught by MIL-STD-461G testing, vs prevented by other design practices?',
    'Post-hoc analysis of EMI-related system failures in the field; correlation with 461G compliance records; identification of failure modes outside the standard''s scope',
    'If >80% correlation: standard is functionally effective coordination mechanism (rope). If <40% correlation: standard is theater with low actual risk prevention (piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_emi_failure_correlation, empirical, 'Correlation between 461G compliance and prevention of actual fielded EMI failures').

omega_variable(
    cost_multiplication_factor,
    'What is the actual cost multiplier for bringing a subsystem to 461G compliance vs no standard?',
    'Comparative cost analysis: identical subsystems with/without 461G certification path; includes test chamber access, engineering labor, schedule delays, and design iteration cycles',
    'If factor <1.5x: compliance burden is modest (tangled rope justified). If factor >3.0x: extraction dominates coordination benefit (snare classification more accurate). If factor >5.0x: small suppliers face genuine exit barrier.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_multiplication_factor, empirical, 'Actual cost multiplier for MIL-STD-461G compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mil_std_461g_emi_control, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mil461g_tr_t0, mil_std_461g_emi_control, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mil461g_tr_t10, mil_std_461g_emi_control, theater_ratio, 10, 0.5).
narrative_ontology:measurement(mil461g_tr_t20, mil_std_461g_emi_control, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(mil461g_be_t0, mil_std_461g_emi_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mil461g_be_t10, mil_std_461g_emi_control, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(mil461g_be_t20, mil_std_461g_emi_control, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mil_std_461g_emi_control, enforcement_mechanism).
narrative_ontology:affects_constraint(mil_std_461g_emi_control, military_procurement_modular_design).
narrative_ontology:affects_constraint(mil_std_461g_emi_control, defense_contractor_supply_chain_lock_in).
narrative_ontology:affects_constraint(mil_std_461g_emi_control, computational_emi_prediction_viability).

% DUAL FORMULATION NOTE:
% MIL-STD-461G decomposes into two structurally distinct constraints: (1) the physical necessity of EMI control in integrated systems (approaches mountain, ε≈0.05), and (2) the institutional apparatus used to enforce compliance (tangled rope, ε=0.38). The standard's symbolic authority derives from conflating these two claims. The JSON story tracks the apparatus, not the underlying physics. Related constraint families track the modular design requirement it enables and the supplier lock-in it creates as downstream effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mil_std_461g_emi_control, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
