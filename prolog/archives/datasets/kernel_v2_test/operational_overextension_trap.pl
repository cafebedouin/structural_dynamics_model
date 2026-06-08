% ============================================================================
% CONSTRAINT STORY: operational_overextension_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_operational_overextension_trap, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: operational_overextension_trap
 *   human_readable: Operational Overextension Trap in Russian Military Operations
 *   domain: military_operations/information_warfare/institutional_dysfunction
 *
 * SUMMARY:
 *   The operational overextension trap emerges from the interaction between
 *   ambitious operational objectives set by senior command and the tactical
 *   reality faced by frontline units. Russian forces are assigned objectives
 *   (Shevchenkove) that require first achieving unmet prerequisites
 *   (Kupyansk), creating systematic force dispersion. Units lack adequate
 *   reserves to respond to Ukrainian counterattacks because forces are
 *   committed to multiple simultaneous axes of advance. The constraint is
 *   downstream of the beautiful reports feedback loop: senior command
 *   receives filtered reports that understate tactical difficulty, enabling
 *   them to set impossible objectives without immediate consequences.
 *   Milblogger warnings about 'difficult situations' provide oblique evidence
 *   of the trap but cannot directly challenge operational planning. The
 *   constraint extracts from tactical units (casualties, exhaustion, mission
 *   failure) to benefit senior command (appearance of progress, political
 *   favor) and political leadership (narrative of offensive success).
 *
 * KEY AGENTS:
 *   - Russian Tactical Units: Primary victim (powerless/trapped) — bear full cost of dispersion and impossible missions; no exit option
 *   - Frontline Commanders: Secondary victim (moderate/constrained) — see the impossibility but cannot refuse orders without career consequences
 *   - Senior Command Structure: Primary beneficiary (institutional/arbitrage) — benefits from ambitious objectives; failures blamed downward
 *   - Political Leadership: Primary beneficiary (institutional/arbitrage) — receives progress reports; insulated from tactical reality
 *   - Milblogger Community: Mixed position (moderate/constrained) — benefits from information access; constrained by censorship risk
 *   - Analytical Observer: External view (analytical/analytical) — sees coordination kernel overlaid with extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(operational_overextension_trap, 0.68).
domain_priors:suppression_score(operational_overextension_trap, 0.82).
domain_priors:theater_ratio(operational_overextension_trap, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(operational_overextension_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(operational_overextension_trap, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(operational_overextension_trap, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(operational_overextension_trap, snare).
narrative_ontology:human_readable(operational_overextension_trap, "Operational Overextension Trap in Russian Military Operations").
narrative_ontology:topic_domain(operational_overextension_trap, "military_operations/information_warfare/institutional_dysfunction").

domain_priors:requires_active_enforcement(operational_overextension_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(operational_overextension_trap, senior_command_structure).
narrative_ontology:constraint_beneficiary(operational_overextension_trap, political_leadership).
narrative_ontology:constraint_victim(operational_overextension_trap, russian_tactical_units).
narrative_ontology:constraint_victim(operational_overextension_trap, frontline_commanders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN TACTICAL UNITS (SNARE) — Trapped in impossible operational geometry. Cannot refuse orders to advance toward Shevchenkove despite lacking control of prerequisite objectives. Bear full cost of dispersion: inadequate reserves, inability to respond to Ukrainian counterattacks, mounting casualties. No exit option — desertion is punished, rotation is denied, and the command structure does not accept tactical reality as grounds for mission refusal.
constraint_indexing:constraint_classification(operational_overextension_trap, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FRONTLINE COMMANDERS (SNARE) — Constrained by career consequences of reporting failure. Can see the operational impossibility but cannot refuse orders or revise objectives without being relieved. Face biographical consequences: relief for 'defeatism,' blocked promotion, or worse. The constraint extracts compliance through career threat while providing no coordination benefit — commanders know the orders are unexecutable but must attempt them anyway.
constraint_indexing:constraint_classification(operational_overextension_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SENIOR COMMAND STRUCTURE (ROPE) — Benefits from the overextension dynamic. Ambitious objectives satisfy political leadership's demand for progress. Failures are blamed on tactical execution, not operational planning. The command structure experiences this as coordination: they are transmitting political intent into military action. Extraction flows toward them (career advancement, political favor) rather than away.
constraint_indexing:constraint_classification(operational_overextension_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL LEADERSHIP (ROPE) — Primary beneficiary. Receives reports of ambitious operations and forward movement. Insulated from tactical reality by the beautiful reports feedback loop. Experiences the constraint as coordination: the military is executing the political vision. Can exit or revise strategy at will but chooses not to because the reporting structure shields them from consequences.
constraint_indexing:constraint_classification(operational_overextension_trap, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MILBLOGGER COMMUNITY (TANGLED ROPE) — Mixed position. Benefits from access to tactical information and audience growth through crisis reporting. Also constrained by censorship risk and pressure to maintain morale narratives. Experiences both coordination (information sharing within constraints) and extraction (cannot report full scope of dysfunction without consequences). Warnings about 'difficult situations' are calibrated to avoid crossing red lines.
constraint_indexing:constraint_classification(operational_overextension_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine operational coordination problem (how to sequence objectives given finite forces) and the extractive overlay (command structure benefits from impossible objectives because failures are blamed downward). The constraint has a coordination kernel — military operations require objective sequencing — but the implementation extracts from tactical units to benefit senior command. Tangled rope classification reflects this hybrid structure.
constraint_indexing:constraint_classification(operational_overextension_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(operational_overextension_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(operational_overextension_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(operational_overextension_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(operational_overextension_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(operational_overextension_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts heavily from tactical units through impossible mission assignments, force dispersion, and inability to respond to threats. Senior command and political leadership benefit through career advancement and narrative control. The extraction is not total (0.9+) because some tactical units do achieve local objectives, and the system occasionally revises plans when failures become undeniable. Theater ratio (0.45): Moderate. Operational planning has genuine functional content — objectives are chosen for strategic reasons, not purely for show. But a substantial portion is performative: objectives are set to satisfy political demands for progress rather than tactical feasibility. The theater component has increased over the interval as the gap between reported and actual progress has widened. Suppression (0.82): Very high. Tactical units cannot refuse orders. Frontline commanders cannot revise objectives without relief. Desertion is punished. Rotation is denied. The reporting structure suppresses accurate tactical assessment from reaching decision-makers. Suppression has increased over the interval as enforcement mechanisms have hardened in response to mounting failures.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark beneficiary-victim gap. Tactical units and frontline commanders experience pure extraction (Snare) — trapped in impossible operational geometry with no exit and mounting costs. Senior command and political leadership experience coordination (Rope) — they are transmitting intent and receiving reports of execution. The analytical observer sees the hybrid structure (Tangled Rope) — a genuine operational coordination problem (how to sequence objectives) overlaid with an extraction mechanism (impossible objectives benefit senior command by blaming failures downward). The milblogger community occupies a mixed position (Tangled Rope) — benefiting from information access while constrained by censorship. The perspectival gap is not a disagreement about facts but a structural difference in who bears costs and who captures benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Tactical units are full targets (d → 1.0): they bear the costs of dispersion, inadequate reserves, and mission failure. Frontline commanders are high-d targets (d → 0.8): constrained by career consequences but with slightly more agency than tactical units. Senior command and political leadership are beneficiaries (d → 0.1-0.2): extraction flows toward them through career advancement and narrative control. The milblogger community has intermediate directionality (d → 0.4-0.5): mixed costs and benefits. The analytical observer's directionality is neutral (d → 0.5) by definition. The engine computes effective extraction (chi) from these directionality values, power levels, and exit options. Trapped agents with high d experience maximum chi; beneficiaries with arbitrage exit experience low or negative chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the snare classification is perspectival, not absolute. From the tactical units' position, this is pure extraction — impossible missions with no exit. From senior command's position, this is coordination — transmitting political intent into military action. The analytical observer sees both: a coordination problem (objective sequencing) overlaid with extraction (failures blamed downward). The mandate (achieve operational objectives) has not outlived its function from senior command's perspective — it continues to serve their career interests. But from the tactical units' perspective, the mandate is mandatrophic — it demands the impossible and punishes failure. The constraint is not 'really' a snare or 'really' a rope; it is both, depending on where you sit in the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prerequisite_definition_ambiguity,
    'What constitutes ''control'' of a prerequisite objective sufficient to enable the next operation? Is Kupyansk a prerequisite for Shevchenkove, or can forces bypass it?',
    'Doctrinal analysis of Russian operational planning; historical case studies of successful vs failed bypass operations; assessment of Ukrainian interdiction capacity from Kupyansk salient',
    'If bypass is doctrinally sound: overextension is tactical failure, not structural trap. If prerequisite control is required: overextension is command-imposed impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prerequisite_definition_ambiguity, empirical, 'Whether prerequisite control is operationally necessary or command preference').

omega_variable(
    reporting_accuracy_threshold,
    'At what point does the beautiful reports feedback loop break? How severe must tactical failure become before it penetrates upward to revise operational objectives?',
    'Historical analysis of Russian command decision-making under stress; identification of past cases where tactical reality forced operational revision; assessment of current political constraints on acknowledging failure',
    'If threshold is low: overextension is self-correcting coordination problem. If threshold is high or nonexistent: overextension is stable extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reporting_accuracy_threshold, empirical, 'Severity threshold for tactical reality to force operational revision').

omega_variable(
    force_generation_sustainability,
    'Can Russian force generation sustain the current operational tempo and dispersion indefinitely, or is there a collapse point?',
    'Mobilization capacity analysis; equipment production and refurbishment rates; casualty and desertion trends; comparison to historical sustainment limits',
    'If sustainable: overextension is chronic stable state. If unsustainable: overextension has a natural sunset (collapse or forced operational pause).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(force_generation_sustainability, empirical, 'Whether force generation can sustain current operational overextension').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(operational_overextension_trap, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(overext_theater_t0, operational_overextension_trap, theater_ratio, 0, 0.3).
narrative_ontology:measurement(overext_theater_t3, operational_overextension_trap, theater_ratio, 3, 0.38).
narrative_ontology:measurement(overext_theater_t6, operational_overextension_trap, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(overext_extract_t0, operational_overextension_trap, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(overext_extract_t3, operational_overextension_trap, base_extractiveness, 3, 0.61).
narrative_ontology:measurement(overext_extract_t6, operational_overextension_trap, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(overext_suppress_t0, operational_overextension_trap, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(overext_suppress_t3, operational_overextension_trap, suppression_requirement, 3, 0.76).
narrative_ontology:measurement(overext_suppress_t6, operational_overextension_trap, suppression_requirement, 6, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(operational_overextension_trap, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The operational overextension trap is downstream of the beautiful reports feedback loop. The feedback loop creates the information asymmetry that enables impossible objective-setting; the overextension trap is the tactical manifestation of that asymmetry. Both constraints are required to model the full system: the feedback loop explains why senior command sets impossible objectives; the overextension trap explains how tactical units experience those objectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
