% ============================================================================
% CONSTRAINT STORY: regulatory_capture_in_aviation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_in_aviation, []).

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
 *   constraint_id: regulatory_capture_in_aviation
 *   human_readable: Regulatory Capture in Aviation Safety Standards
 *   domain: economic_policy/transportation_regulation
 *
 * SUMMARY:
 *   Regulatory capture in aviation occurs when aircraft manufacturers and
 *   incumbent airlines systematically influence the regulatory agencies
 *   tasked with setting safety and competitive standards. The constraint
 *   exhibits the full structure of capture: manufacturers have superior
 *   technical expertise, regulators depend on manufacturer-provided testing
 *   and certification data, regulatory staff rotate through industry
 *   positions, and incumbent carriers have established relationships with
 *   regulators spanning decades. The public and new entrants face trapped
 *   positions — they cannot exit the regulated airspace, cannot influence
 *   standards, and bear asymmetric costs of relaxed standards or
 *   protectionist regulations. The constraint functions as both genuine
 *   coordination (aviation safety requires uniform standards) and systematic
 *   extraction (standards are written to favor incumbents and manufacturers).
 *   Theater ratio (0.65) reflects that formal international standards-setting
 *   processes, technical committees, and consensus procedures are largely
 *   performative — the actual policy outcomes are determined by industry
 *   capture before the formal process begins.
 *
 * KEY AGENTS:
 *   - Aircraft Manufacturers (Boeing, Airbus): Institutional beneficiaries (institutional/arbitrage) — capture enables cost externalization, extended certification timelines, influence over competitive standards
 *   - Incumbent Airlines (Legacy carriers): Institutional beneficiaries (institutional/arbitrage) — benefit from grandfathering, asymmetric new entrant requirements, resistance to fuel-efficiency mandates
 *   - Flying Public: Primary victim (powerless/trapped) — cannot exit regulated airspace, bear safety risk from weakened standards, have no voice in standard formation
 *   - New Entrant Carriers: Secondary victim (powerless/trapped) — face asymmetric certification barriers, cannot compete on equal terms despite equivalent safety records
 *   - FAA/Regulatory Agencies: Institutional actor with mixed position (institutional/constrained) — genuine coordination function overlaid with capture; constrained by manufacturer expertise monopoly and funding dependency
 *   - Aviation Safety Standards: Abstract victim (powerless/trapped) — standards are degraded when capture prioritizes manufacturer convenience over technical rigor; no mechanism for self-correction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination requirement and systematic extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_in_aviation, 0.58).
domain_priors:suppression_score(regulatory_capture_in_aviation, 0.68).
domain_priors:theater_ratio(regulatory_capture_in_aviation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_in_aviation, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_in_aviation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(regulatory_capture_in_aviation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_in_aviation, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_in_aviation, "Regulatory Capture in Aviation Safety Standards").
narrative_ontology:topic_domain(regulatory_capture_in_aviation, "economic_policy/transportation_regulation").

domain_priors:requires_active_enforcement(regulatory_capture_in_aviation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_in_aviation, aircraft_manufacturers).
narrative_ontology:constraint_beneficiary(regulatory_capture_in_aviation, incumbent_airlines).
narrative_ontology:constraint_victim(regulatory_capture_in_aviation, flying_public).
narrative_ontology:constraint_victim(regulatory_capture_in_aviation, new_entrant_carriers).
narrative_ontology:constraint_victim(regulatory_capture_in_aviation, aviation_safety_standards).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FLYING PUBLIC (SNARE) — Passengers and crews cannot exit the regulatory regime; they bear safety risk from weakened standards while having no voice in their formation. No exit option exists — flying within regulated airspace is mandatory for most long-distance travel. Maximum experienced extraction: regulatory capture transfers certification burden from manufacturers to regulators, who substitute political accommodation for technical rigor.
constraint_indexing:constraint_classification(regulatory_capture_in_aviation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEW ENTRANT CARRIERS (SNARE) — Cannot exit the regulatory system; face asymmetric certification requirements designed to favor incumbents. Incumbent airlines have established relationships with regulators and benefit from grandfathered certifications; new entrants must navigate full modern certification regimes. Trapped by the regulatory monopoly on airworthiness standards.
constraint_indexing:constraint_classification(regulatory_capture_in_aviation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: AIRCRAFT MANUFACTURERS (ROPE) — Primary beneficiary of regulatory capture. Manufactures perceive the constraint as coordination: regulatory standards ensure market stability, establish barriers to competition, and allow manufacturers to influence certification timelines and technical requirements. Net beneficiary position — extraction flows toward this agent. Arbitrage exit option reflects ability to relocate production, influence policy through trade agreements, and access alternative markets.
constraint_indexing:constraint_classification(regulatory_capture_in_aviation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT AIRLINES (ROPE) — Beneficiary of regulatory capture through grandfathering, favorable certification timelines, and resistance to fuel-efficiency standards that would require fleet modernization. Perceive the constraint as coordination: they coordinate with regulators on safety standards that protect their competitive position. Arbitrage exit option reflects ability to lobby for policy changes, access alternative markets, and offshore operations.
constraint_indexing:constraint_classification(regulatory_capture_in_aviation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCIES (TANGLED ROPE) — FAA and equivalent bodies face genuine coordination requirements: they must establish uniform standards, certify aircraft safety, and manage airspace efficiently. But they are constrained by industry capture — their technical staff are trained by manufacturers, their leadership rotates through industry positions, and their funding depends on manufacturer cooperation. Extraction and coordination coexist: genuine safety coordination is overlaid with asymmetric policy influence flowing toward manufacturers.
constraint_indexing:constraint_classification(regulatory_capture_in_aviation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL STANDARDS BODIES (PITON) — ICAO and equivalent bodies maintain performative coordination of international aviation standards. Theater ratio is high (0.65) — the formal standards-setting process creates elaborate technical committees, consensus procedures, and documentation, but the actual policy outcomes are largely captured by major manufacturers and incumbent carriers before the formal process begins. The formal procedure is inertially maintained despite low functional efficacy.
constraint_indexing:constraint_classification(regulatory_capture_in_aviation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, aviation regulation coordinates a genuine collective action problem: heterogeneous actors (manufacturers, airlines, regulators, public) must establish uniform safety standards to enable safe commercial aviation at scale. This coordination function is real. But the extraction mechanism is equally real: manufacturers and incumbent carriers have systematically captured the regulatory process to shift compliance costs onto competitors and the public, weaken fuel-efficiency and environmental standards, and slow innovation by new entrants. The constraint exhibits both genuine coordination (safety standards) and systematic extraction (distributional asymmetry).
constraint_indexing:constraint_classification(regulatory_capture_in_aviation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_in_aviation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_in_aviation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_in_aviation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_in_aviation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_in_aviation, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_in_aviation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Manufacturers and incumbent carriers extract substantial value through relaxed certification standards, extended timelines, reduced compliance costs, and protection from competition. The extraction is significant but not total — genuine safety coordination remains, and catastrophic safety failures (which would trigger public/political response) still trigger regulatory response. The rising trajectory over the 45-year interval reflects accumulating capture: initially (t=0, ε=0.35) capture was limited by stronger regulatory capacity and closer public scrutiny post-Challenger. By t=30, manufacturing consolidation and regulatory budget constraints increased dependence on manufacturer expertise (ε=0.48). By t=45, institutional inertia and revolving-door employment have deepened capture (ε=0.62). Suppression (0.68): High. Barriers to exit and alternative pathways include: (1) regulatory monopoly on airworthiness certification — no private alternatives exist; (2) information asymmetry — manufacturers have superior technical knowledge; (3) political barriers — capture is sustained by congressional relationships and lobbying; (4) coordination costs — establishing alternative standards-setting processes would require international agreement; (5) career barriers — regulatory staff depend on industry employment. Theater ratio (0.65): Moderate-high. International standards bodies (ICAO), FAA advisory committees, and formal certification procedures create elaborate procedural machinery. But the actual decisions are captured before the formal process — manufacturer positions are pre-negotiated, regulatory staff are trained by manufacturers, and consensus procedures rubber-stamp outcomes determined by power asymmetries.
 *
 * PERSPECTIVAL GAP:
 *   The gap between manufacturer (Rope) and public (Snare) perspectives is the clearest indicator of capture. If the constraint were pure coordination, all perspectives would produce the same type. The fact that beneficiaries see coordination (Rope) while victims see pure extraction (Snare) is diagnostic of systematic capture. The regulatory agency's Tangled Rope position is ambiguous — they have genuine coordination function (safety standards) overlaid with extraction mechanism (capture). This ambiguity is the signature of regulatory capture: the institution contains both legitimate function and systematic bias, making it difficult for internal reform (the regulatory agency cannot unilaterally escape capture without external pressure).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the beneficiary/victim structure and exit options. Aircraft manufacturers are institutional agents with arbitrage options (can relocate production, access alternative markets, influence policy through trade leverage) who are identified as beneficiaries — they benefit from capture. The derivation pipeline assigns them low d (beneficiary + arbitrage → d ≈ 0.15). Incumbent airlines have similar institutional position with arbitrage options and benefit from grandfathering and competitive protection — low d. The flying public are powerless agents with trapped exit (no alternative to regulated airspace) and are victims — they bear costs. High d (victim + trapped → d ≈ 0.95). New entrant carriers are powerless/moderate agents with constrained exit (high regulatory barriers but not impossible) and are victims — they face asymmetric requirements (d ≈ 0.80). The directional pattern reveals why chi (effective extractiveness) differs across perspectives: beneficiaries have low or negative f(d), experiencing the constraint as coordination; victims have high f(d), experiencing it as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by decomposing capture into its component mechanisms. (1) Genuine Coordination Function: Aviation safety requires uniform standards — heterogeneous manufacturers, airlines, and regulators cannot each maintain independent safety protocols. This coordination function is real and justifies some regulatory overhead. (2) Extraction Mechanism: Capture manifests as manufacturers and incumbents using regulatory authority to shift compliance costs onto competitors, the public, and future safety. This extraction is quantifiable: relaxed certification timelines (manufacturer benefit), asymmetric new-entrant requirements (incumbent benefit), reduced fuel-efficiency standards (manufacturer cost reduction), and information monopolies (manufacturer expertise barrier). (3) The Synthesis: The constraint is Tangled Rope because it genuinely coordinates safety AND systematically extracts value through captured standards. The mandatrophy is NOT 'is this capture or coordination?' but 'how much of the regulatory overhead is legitimate coordination cost vs systematic extraction?'. The theater ratio (0.65) reflects that the formal process is substantially performative — the real decisions happen in closed meetings and industry-regulator relationships. The extractiveness (0.58) reflects that extraction is substantial but not total — catastrophic safety failures still trigger regulatory response, constraining the degree of capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manufacturer_safety_expertise_monopoly,
    'Does regulatory reliance on manufacturer expertise constitute unavoidable information asymmetry or manufactured epistemic capture?',
    'Historical analysis of certification decisions that deviated from manufacturer recommendations; comparison of safety outcomes when regulators developed independent technical capacity vs periods of reliance on manufacturer expertise',
    'If unavoidable: capture is coordination cost, not extraction. If manufactured: capture is systematic extraction mechanism that could be dismantled by investing in independent regulatory expertise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturer_safety_expertise_monopoly, empirical, 'Whether manufacturer expertise monopoly is inevitable or artificially constructed').

omega_variable(
    safety_standard_effectiveness_measurement,
    'Do captured regulations (relaxed design standards, extended certification timelines) actually produce lower accident rates for manufacturing economy, or do they merely reduce manufacturer costs while shifting risk elsewhere?',
    'Longitudinal accident rate analysis by aircraft type and certification era; correlation between regulatory stringency and operational safety; decomposition of accidents by root cause (design defect vs operational error vs maintenance failure)',
    'If captured standards improve safety outcomes: extraction is justified as efficient regulation. If captured standards reduce safety: extraction is pure rent-seeking with negative externality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safety_standard_effectiveness_measurement, empirical, 'Whether captured regulations achieve their stated safety objective').

omega_variable(
    new_entrant_exit_barrier_causation,
    'Do asymmetric certification requirements for new entrants reflect legitimate regulatory conservatism toward unproven manufacturers or deliberate capture-driven protectionism?',
    'Analysis of certification timelines and technical requirements by manufacturer incumbency status; comparison of accident rates for new-entrant aircraft vs incumbent aircraft at equivalent technology levels; historical cases of successful market entry despite capture',
    'If legitimate conservatism: new entrant barriers are coordination cost. If deliberate protectionism: barriers are pure extraction that transfers consumer surplus to incumbents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_entrant_exit_barrier_causation, empirical, 'Whether new entrant certification barriers serve safety or protectionism').

omega_variable(
    regulatory_funding_dependency_mechanism,
    'Does manufacturer-funded testing and FAA reliance on manufacturer-submitted data create a financial incentive structure that systematically biases regulatory decisions toward manufacturers?',
    'Institutional analysis of FAA funding sources; comparison of certification outcomes for internally-developed standards vs manufacturer-funded research; tracking of regulatory staff career trajectories through industry positions',
    'If financial dependency exists: it is a structural mechanism that maintains capture regardless of personnel intentions. If dependency is minimal: capture relies on weaker mechanisms (expertise monopoly, political pressure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_funding_dependency_mechanism, empirical, 'Whether regulatory funding dependency creates systematic bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_in_aviation, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_aviation_tr_t0, regulatory_capture_in_aviation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(regcap_aviation_tr_t15, regulatory_capture_in_aviation, theater_ratio, 15, 0.58).
narrative_ontology:measurement(regcap_aviation_tr_t30, regulatory_capture_in_aviation, theater_ratio, 30, 0.65).
narrative_ontology:measurement(regcap_aviation_tr_t45, regulatory_capture_in_aviation, theater_ratio, 45, 0.7).

% Extraction over time
narrative_ontology:measurement(regcap_aviation_be_t0, regulatory_capture_in_aviation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_aviation_be_t15, regulatory_capture_in_aviation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(regcap_aviation_be_t30, regulatory_capture_in_aviation, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(regcap_aviation_be_t45, regulatory_capture_in_aviation, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_in_aviation, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_in_aviation, aviation_fuel_efficiency_standards).
narrative_ontology:affects_constraint(regulatory_capture_in_aviation, aircraft_certification_timelines).
narrative_ontology:affects_constraint(regulatory_capture_in_aviation, airline_merger_approval_process).

% DUAL FORMULATION NOTE:
% Regulatory capture in aviation is upstream of specific regulatory outcomes (fuel standards, merger approvals, certification timelines). Each downstream constraint has its own extractiveness reflecting specific policy domain; the capture constraint represents the structural mechanism that biases all downstream outcomes toward manufacturer and incumbent preference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_in_aviation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
