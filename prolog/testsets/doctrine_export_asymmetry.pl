% ============================================================================
% CONSTRAINT STORY: doctrine_export_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doctrine_export_asymmetry, []).

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
 *   constraint_id: doctrine_export_asymmetry
 *   human_readable: Doctrine Export Asymmetry in Military Innovation Cycles
 *   domain: military_innovation/asymmetric_warfare/procurement_systems
 *
 * SUMMARY:
 *   The doctrine export asymmetry constraint captures the structural tension
 *   between Ukrainian rapid-cycle military innovation (3-month
 *   problem-to-deployment) and Western procurement timelines (5-7 year
 *   acquisition cycles). Ukrainian forces demonstrate counter-drone tactics,
 *   precision-mass economics, and infrastructure targeting methods that NATO
 *   allies cannot adopt at operationally relevant speeds due to institutional
 *   acquisition pathways. The constraint exhibits high extraction (0.68)
 *   because NATO tactical readiness bears the cost of doctrine lag while
 *   defense industrial incumbents and procurement bureaucracies benefit from
 *   timeline protection of market position and institutional authority. The
 *   theater ratio (0.65) reflects that developmental testing regimes have
 *   become substantially performative: controlled-environment certification
 *   cannot replicate adaptive adversary behavior, and testing timelines
 *   exceed the operational lifespan of the tactics being validated. The
 *   Hegseth directive (2026 target for Ukrainian baseline) represents
 *   institutional recognition of the problem but faces uncertain
 *   implementation — the scaffold perspective depends on whether reform
 *   materializes or becomes theater. Gulf state direct procurement from
 *   Ukraine (bypassing Western acquisition entirely) demonstrates that the
 *   constraint is institutional rather than technical: the same systems
 *   Western militaries cannot field in under 5 years are being deployed by
 *   non-NATO actors in under 6 months.
 *
 * KEY AGENTS:
 *   - NATO Tactical Units: Primary victim (powerless/trapped) — face adversaries using Ukrainian-derived tactics while locked into multi-year procurement cycles; no alternative acquisition authority
 *   - Allied Force Planners: Secondary victim (moderate/constrained) — recognize innovation gap but face career-ending costs for deviation from institutional pathways
 *   - Defense Industrial Incumbents: Primary beneficiary (institutional/arbitrage) — procurement timelines protect market share from rapid-cycle disruptors; long development cycles justify high margins
 *   - Procurement Bureaucracy: Mixed actor (institutional/constrained) — genuinely coordinates complex requirements but also extracts rents through process complexity; benefits from institutional continuity
 *   - Rapid Innovation Coalitions: Organized reformers (organized/mobile) — Defense Innovation Unit, AUKUS tech-sharing, Gulf state direct procurement building alternative pathways with sunset logic
 *   - Legacy Testing Regimes: Degraded institution (institutional/arbitrage) — developmental testing largely performative; maintained through inertia rather than validation effectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible mix of coordination (safety, interoperability) and extraction (incumbent protection, bureaucratic rents)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doctrine_export_asymmetry, 0.68).
domain_priors:suppression_score(doctrine_export_asymmetry, 0.72).
domain_priors:theater_ratio(doctrine_export_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doctrine_export_asymmetry, extractiveness, 0.68).
narrative_ontology:constraint_metric(doctrine_export_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(doctrine_export_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doctrine_export_asymmetry, snare).
narrative_ontology:human_readable(doctrine_export_asymmetry, "Doctrine Export Asymmetry in Military Innovation Cycles").
narrative_ontology:topic_domain(doctrine_export_asymmetry, "military_innovation/asymmetric_warfare/procurement_systems").

domain_priors:requires_active_enforcement(doctrine_export_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doctrine_export_asymmetry, defense_industrial_incumbents).
narrative_ontology:constraint_beneficiary(doctrine_export_asymmetry, procurement_bureaucracy).
narrative_ontology:constraint_victim(doctrine_export_asymmetry, nato_tactical_readiness).
narrative_ontology:constraint_victim(doctrine_export_asymmetry, allied_force_effectiveness).
narrative_ontology:constraint_victim(doctrine_export_asymmetry, rapid_innovation_adopters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATO TACTICAL UNITS (SNARE) — Frontline units face adversaries using Ukrainian-derived counter-drone and precision-mass tactics while locked into 5-7 year procurement cycles. Cannot exit institutional acquisition pathways; bear full cost of doctrine lag in combat effectiveness. Trapped by institutional dependencies with no alternative procurement authority.
constraint_indexing:constraint_classification(doctrine_export_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALLIED FORCE PLANNERS (SNARE) — Military planners recognize the innovation gap (Ukrainian 3-month cycle vs NATO 5-year cycle) but face institutional barriers: acquisition regulations, contractor dependencies, testing requirements, budget cycles. Constrained rather than trapped — some emergency procurement authority exists — but costs of deviation are career-ending. High extraction through institutional lock-in.
constraint_indexing:constraint_classification(doctrine_export_asymmetry, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: DEFENSE INDUSTRIAL INCUMBENTS (ROPE) — Prime contractors benefit from procurement timelines that favor established platforms over rapid-cycle innovation. The constraint coordinates their market position: long development cycles justify high margins, testing requirements create barriers to entry, and institutional relationships ensure contract continuity. Net beneficiary — the doctrine lag protects market share from disruptive entrants.
constraint_indexing:constraint_classification(doctrine_export_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROCUREMENT BUREAUCRACY (TANGLED ROPE) — Acquisition institutions genuinely coordinate complex requirements (safety, interoperability, lifecycle support) but also extract rents through process complexity. The bureaucracy benefits from institutional continuity and budget authority while recognizing that procurement timelines impose tactical costs. Mixed coordination and extraction — the testing and certification infrastructure serves real functions but has accumulated extractive overhead.
constraint_indexing:constraint_classification(doctrine_export_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RAPID INNOVATION COALITIONS (SCAFFOLD) — Organized actors (Defense Innovation Unit, AUKUS tech-sharing, Gulf state direct procurement from Ukraine) are building alternative pathways that bypass traditional acquisition. The Hegseth directive (2026 target for 2022 Ukrainian baseline) represents institutional recognition that the procurement gap is a solvable coordination problem with a sunset. Low effective extraction because coalition has agency and sees exit path through institutional reform.
constraint_indexing:constraint_classification(doctrine_export_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY TESTING REGIMES (PITON) — Traditional developmental testing for counter-drone systems and precision-mass munitions is largely performative: controlled-environment tests cannot replicate adaptive adversary behavior, and certification timelines (18-24 months) exceed the operational lifespan of the tactics being tested. The testing ritual persists through institutional inertia despite Ukrainian combat data providing faster validation. Piton classification derives from theater gate — maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(doctrine_export_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global civilizational perspective, the doctrine export asymmetry exhibits both genuine coordination functions (safety, interoperability, lifecycle support) and asymmetric extraction (incumbent protection, bureaucratic rents, tactical readiness costs). The constraint is not a natural law — procurement timelines are institutional choices — but also not pure extraction, as some testing and certification overhead is inherent to complex military systems. Tangled Rope reflects the irreducible mix.
constraint_indexing:constraint_classification(doctrine_export_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doctrine_export_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(doctrine_export_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(doctrine_export_asymmetry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(doctrine_export_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(doctrine_export_asymmetry, TR),
    TR >= 0.70.

:- end_tests(doctrine_export_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. NATO tactical units bear significant combat effectiveness costs from doctrine lag (5-7 year adoption cycle vs 3-month Ukrainian cycle), while defense industrial incumbents capture market protection benefits and procurement bureaucracies maintain institutional authority. The extraction is not maximal (0.68 rather than 0.85+) because some procurement overhead represents genuine coordination cost (safety validation, interoperability testing, lifecycle support). The value reflects that the career asymmetry and tactical readiness gap are real and substantial, but not all timeline overhead is pure rent-seeking. Suppression (0.72): High. Institutional barriers to rapid adoption include acquisition regulations (FAR/DFARS compliance), contractor dependencies (sole-source platforms), testing requirements (developmental and operational), budget cycle constraints (annual appropriations), and career risk for deviation. Suppression is not total — emergency procurement authority exists, and some rapid-cycle pathways (DIU, AUKUS) are emerging — but costs of institutional deviation remain severe. Theater ratio (0.65): Moderate-high. Developmental testing for counter-drone and precision-mass systems is substantially performative: controlled environments cannot replicate adaptive adversary behavior, certification timelines (18-24 months) exceed tactical lifespan, and Ukrainian combat data provides faster validation. The theater has increased over the interval as adversary adaptation speed has outpaced institutional testing cycles. Rapid innovation pathways bypass this theater — their validation mechanism (combat data, allied field testing) has different failure modes but lower performative content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the gap between Ukrainian rapid-cycle innovation and Western procurement timelines — appears as pure extraction (Snare) from the perspective of trapped tactical units, as market coordination (Rope) from the perspective of defense industrial incumbents, as mixed coordination-extraction (Tangled Rope) from the perspective of procurement bureaucracy and the analytical observer, as a solvable temporary problem (Scaffold) from the perspective of rapid innovation coalitions, and as degraded ritual (Piton) from the perspective of legacy testing regimes. The tactical units see maximum extraction because they bear combat effectiveness costs with no exit. The incumbents see coordination because the constraint protects their market position. The bureaucracy sees mixed benefits and costs because it genuinely coordinates complex requirements while also extracting institutional rents. The coalitions see a sunset because alternative pathways are emerging. The testing regimes see their own degradation because certification has become performative. The analytical observer sees irreducible mix because some procurement overhead is genuine coordination cost while some is extractive institutional protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense industrial incumbents are primary beneficiaries with arbitrage exit options — they benefit from procurement timelines that protect market share and can shift to commercial markets if defense margins compress. Derived d ≈ 0.05 (full beneficiary). Procurement bureaucracy is a mixed actor: benefits from institutional authority but also recognizes tactical costs; constrained exit (institutional reform is possible but costly). Derived d ≈ 0.35 (beneficiary-leaning symmetric). NATO tactical units are primary victims with trapped exit — locked into institutional acquisition pathways with no alternative procurement authority. Derived d ≈ 0.95 (full target). Allied force planners are secondary victims with constrained exit — face institutional barriers but have some emergency procurement authority. Derived d ≈ 0.85 (victim with limited agency). Rapid innovation coalitions are organized actors with mobile exit — building alternative pathways and have institutional backing for reform. Derived d ≈ 0.45 (slight victim but with agency). Legacy testing regimes are institutional actors with arbitrage exit — the testing bureaucracy persists through inertia but could pivot to other validation roles. Derived d ≈ 0.15 (beneficiary of institutional continuity). The analytical observer uses canonical d ≈ 0.72 for analytical context.
 *
 * MANDATROPHY ANALYSIS:
 *   PARTIAL RESOLUTION: The constraint exhibits both genuine coordination functions (safety validation, interoperability testing, lifecycle support) and asymmetric extraction (incumbent market protection, bureaucratic rents, tactical readiness costs). The mandatrophy question is not 'pure coordination or pure extraction?' but 'what proportion is each, and can the coordination function be preserved while reducing extraction?' The omega variables identify the empirical questions that would resolve this: (1) What proportion of procurement timeline is genuine safety overhead vs extractive process? (2) Does combat data provide equivalent validation to developmental testing? (3) Are timelines designed to protect incumbents or emergent from system complexity? The Hegseth directive represents an institutional hypothesis that the coordination function can be preserved at much shorter timelines (2026 target for 2022 Ukrainian baseline), but implementation uncertainty prevents full mandatrophy resolution. If the directive succeeds, the constraint shifts toward Tangled Rope or Scaffold (coordination with reducible extraction). If it fails or becomes performative, the constraint remains Snare (extraction naturalized as necessary process).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_overhead_threshold,
    'What proportion of the 5-7 year NATO procurement timeline represents genuine safety and interoperability validation vs extractive process overhead?',
    'Comparative analysis of Ukrainian rapid-cycle safety record vs NATO developmental testing outcomes; identification of which testing phases catch real failures vs which serve as bureaucratic gates',
    'If safety overhead < 30%: procurement timeline is primarily extractive (Snare from more perspectives). If safety overhead > 60%: timeline is primarily coordination (Tangled Rope or Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_overhead_threshold, empirical, 'Proportion of procurement timeline representing genuine safety validation').

omega_variable(
    combat_data_substitution,
    'Does Ukrainian combat data provide validation equivalent to or superior to controlled developmental testing for counter-drone and precision-mass systems?',
    'Longitudinal tracking of systems validated via combat data vs developmental testing; failure rate comparison; adaptation speed to adversary countermeasures',
    'If combat data superior: legacy testing is pure theater (Piton confirmed). If developmental testing catches failures combat data misses: testing overhead is coordination cost (Tangled Rope shifts toward Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(combat_data_substitution, empirical, 'Whether combat data provides equivalent or superior validation to developmental testing').

omega_variable(
    incumbent_capture_mechanism,
    'Do procurement timelines protect incumbent market share through institutional design, or do they emerge from genuine complexity in military system integration?',
    'Historical analysis of procurement reform attempts; correlation between timeline reduction efforts and incumbent lobbying; identification of which requirements are technically necessary vs which serve as barriers to entry',
    'If institutional design: constraint is Snare from more perspectives (deliberate extraction). If emergent complexity: constraint is Tangled Rope (coordination with extractive side effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_mechanism, conceptual, 'Whether procurement timelines are designed to protect incumbents or emerge from system complexity').

omega_variable(
    hegseth_directive_sunset,
    'Will the 2026 target for Ukrainian baseline adoption actually materialize, or will institutional resistance preserve the procurement gap?',
    'Tracking of Hegseth directive implementation milestones; identification of bureaucratic compliance vs substantive timeline reduction; measurement of actual time-to-deployment for systems adopted post-directive',
    'If directive succeeds: Scaffold perspective confirmed (real sunset). If directive fails or becomes performative: Scaffold collapses to Piton (aspirational reform becomes theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hegseth_directive_sunset, empirical, 'Whether institutional reform directive will achieve substantive timeline reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doctrine_export_asymmetry, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_2014, doctrine_export_asymmetry, theater_ratio, 0, 0.5).
narrative_ontology:measurement(theater_2018, doctrine_export_asymmetry, theater_ratio, 4, 0.58).
narrative_ontology:measurement(theater_2022, doctrine_export_asymmetry, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(extract_2014, doctrine_export_asymmetry, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(extract_2018, doctrine_export_asymmetry, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(extract_2022, doctrine_export_asymmetry, base_extractiveness, 8, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doctrine_export_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(doctrine_export_asymmetry, precision_mass_economics).
narrative_ontology:affects_constraint(doctrine_export_asymmetry, infrastructure_as_force_multiplier).

% DUAL FORMULATION NOTE:
% The doctrine export asymmetry is downstream of specific tactical innovations (precision-mass economics, infrastructure targeting) but represents a distinct institutional constraint. The upstream constraints have their own extractiveness values reflecting the tactical and strategic dynamics of the specific methods; the doctrine export asymmetry has its own extractiveness reflecting the procurement timeline gap and institutional barriers to adoption. Ukrainian rapid-cycle innovation demonstrates that the tactical methods are transferable — the constraint is in Western institutional acquisition pathways, not in the tactics themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doctrine_export_asymmetry, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
