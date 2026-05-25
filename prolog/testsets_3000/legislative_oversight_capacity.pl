% ============================================================================
% CONSTRAINT STORY: legislative_oversight_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legislative_oversight_capacity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legislative_oversight_capacity
 *   human_readable: Legislative Oversight Capacity Constraint
 *   domain: political/governmental
 *
 * SUMMARY:
 *   Legislative oversight capacity is the structural ability of a legislature
 *   to investigate, monitor, and enforce constraints on executive and
 *   regulatory agency behavior. The constraint emerges when executive
 *   complexity outpaces legislative technical capacity and staffing
 *   resources. This creates a characteristic extractive dynamic: agencies
 *   expand their scope beyond legislative intent, legislatures perform
 *   oversight rituals without enforcement capacity, and constituencies bear
 *   the cost of regulatory drift unchecked by accountability. The constraint
 *   exhibits all six DR types from different perspectives, making it
 *   diagnostic for how institutional capacity mismatches generate extraction
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Constituency: Primary victim (powerless/trapped) — cannot exit jurisdiction, faces information asymmetry, bears cost of regulatory overreach
 *   - Legislative Body: Moderate victim (moderate/constrained) — understaffed committees, expertise gaps, constrained by agenda pressure and resource limitations
 *   - Executive Branch: Primary beneficiary (institutional/arbitrage) — expands regulatory scope with minimal legislative constraint; can exit through policy expansion
 *   - Regulatory Agencies: Secondary beneficiary (institutional/arbitrage) — drift away from statutory intent unchecked by legislative scrutiny
 *   - Legislative Reform Coalition: Organized agent (organized/constrained) — transparency advocates, staff improvement initiatives building alternative oversight pathways with sunset logic
 *   - Committee Hearing System: Institutional ritual (institutional/arbitrage) — maintains performative theater despite minimal functional verification capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent resource allocation as inherent complexity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legislative_oversight_capacity, 0.58).
domain_priors:suppression_score(legislative_oversight_capacity, 0.62).
domain_priors:theater_ratio(legislative_oversight_capacity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legislative_oversight_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(legislative_oversight_capacity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legislative_oversight_capacity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legislative_oversight_capacity, tangled_rope).
narrative_ontology:human_readable(legislative_oversight_capacity, "Legislative Oversight Capacity Constraint").
narrative_ontology:topic_domain(legislative_oversight_capacity, "political/governmental").

domain_priors:requires_active_enforcement(legislative_oversight_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legislative_oversight_capacity, executive_branch).
narrative_ontology:constraint_beneficiary(legislative_oversight_capacity, administrative_agencies).
narrative_ontology:constraint_beneficiary(legislative_oversight_capacity, regulatory_apparatus).
narrative_ontology:constraint_victim(legislative_oversight_capacity, legislative_body).
narrative_ontology:constraint_victim(legislative_oversight_capacity, constituency_accountability).
narrative_ontology:constraint_victim(legislative_oversight_capacity, public_interest).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONSTITUENCY (SNARE) — Citizens cannot exit their jurisdiction and face structural capture through information asymmetry. Elected representatives lack capacity to investigate executive actions; constituents bear the cost of regulatory overreach with no mechanism to compel legislative action. Maximum extraction from a trapped, powerless position.
constraint_indexing:constraint_classification(legislative_oversight_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE LEGISLATIVE BODY (TANGLED ROPE) — Constrained by staff limitations, technical expertise gaps, and agenda pressure. Genuine coordination function: legislatures must review executive operations and pass laws. Simultaneously experiences extraction: executive agencies and lobbyists exploit oversight capacity limits to operate with minimal transparency. High theater in committee hearings that appear to investigate but lack follow-up capacity.
constraint_indexing:constraint_classification(legislative_oversight_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EXECUTIVE BRANCH (ROPE) — Experiences the constraint as pure coordination: legislative oversight provides legitimate accountability signaling and policy constraints. Benefits from oversight bottleneck (underfunded committees cannot track regulatory drift). Can exit through policy expansion (regulatory reach) or information control. Net beneficiary position.
constraint_indexing:constraint_classification(legislative_oversight_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCIES (ROPE) — Genuinely coordinate with legislative intent through statutory mandates and appropriations. Oversight bottleneck benefits agencies: limited legislative scrutiny allows regulatory drift toward agency preferences without triggering reform. Arbitrage: agencies can shift emphasis within statutory authorization without detection.
constraint_indexing:constraint_classification(legislative_oversight_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE REFORM COALITION (SCAFFOLD) — Organized actors (good-government groups, transparency advocates, staff improvement initiatives) see oversight capacity as a temporary coordination failure with sunset potential. Congressional budget increases, specialized committees, inspector general expansion, and transparency mandates are building alternative pathways. Theater high (reform is often symbolic) but has sunset logic if implementation succeeds.
constraint_indexing:constraint_classification(legislative_oversight_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE COMMITTEE HEARING RITUAL (PITON) — Traditional congressional hearings where agencies respond to questions from legislators have degraded from functional oversight to performative theater. Hearings proceed; no follow-up investigation; no enforcement of testimony accuracy; no legislative action based on findings. The ritual persists through institutional inertia despite minimal verification capacity. Theater ratio very high (0.75+) — the activity appears to constitute oversight but produces minimal accountability.
constraint_indexing:constraint_classification(legislative_oversight_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INSTITUTIONAL COMPLEXITY VIEW (MOUNTAIN) — From a civilizational perspective, some capacity mismatch is inherent to democratic governance: the executive manages complexity that legislatures cannot fully track, and this gap is a structural feature of modern state administration. This perspective naturalizes the constraint as an immutable limit of democratic institutions. However, structural data contradicts the mountain classification — the engine will identify this as false summit, revealing that 'inherent to complexity' naturalizes what is actually a contingent resource allocation choice.
constraint_indexing:constraint_classification(legislative_oversight_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legislative_oversight_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legislative_oversight_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legislative_oversight_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legislative_oversight_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legislative_oversight_capacity, TR),
    TR >= 0.70.

:- end_tests(legislative_oversight_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint represents an asymmetric capacity mismatch where executive agencies operate under minimal legislative scrutiny. Initial extractiveness (0.35) reflected pre-1970s period when executive scope was narrower and legislative capacity closer to adequate. Current value (0.58) reflects regulatory expansion outpacing staff growth, particularly in specialized domains (environmental, financial, health regulation) where legislative committees lack technical expertise. The trajectory upward (0.35 → 0.48 → 0.58 over 20 years) reflects the accumulating gap between regulatory complexity and oversight resources. Suppression (0.62): Moderate-high. Barriers to effective oversight include: (a) staff limitations (most legislative committees have 3-5 staff; regulatory agencies have hundreds), (b) expertise asymmetry (agencies employ subject-matter experts; legislators do not), (c) information asymmetry (agencies control data presentation), (d) agenda pressure (legislatures juggle hundreds of issues; agencies focus on one domain), (e) retaliation risk (agencies can slow or redirect constituent services if overseen too aggressively). Theater ratio (0.68): High and increasing. Congressional hearings where agency officials testify appear to constitute oversight but rarely produce follow-up investigation, testimony verification, or enforcement. Theater has increased (0.45 → 0.60 → 0.68) as gap between hearing activity and actual investigative capacity has widened.
 *
 * PERSPECTIVAL GAP:
 *   The legislative body and executive/agencies perceive the constraint completely differently. From the legislative perspective, the constraint is mixed coordination-extraction (tangled rope) — the capacity limit is real, extractive, and asymmetric. From the executive perspective, the constraint is coordination (rope) — legislative oversight provides legitimate accountability feedback and policy constraints. The gap reveals that the constraint's extractive character depends on whether the agent has the capacity to exercise exit. The executive has arbitrage (can operate despite oversight); the legislature does not (cannot close the information gap without resources it lacks). The tanglement is real: genuine coordination function (agencies do implement legislative intent, mostly) alongside genuine extraction (agencies drift beyond intent and legislatures cannot detect or correct this).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position and power. Constituencies are trapped with no exit (d near 1.0) — maximum extraction. Legislatures are constrained but have some agency through budget power and statutory authority (d ~0.65). Executive agencies are beneficiaries with arbitrage options (expand scope, control information, shift interpretation) (d ~0.20). The asymmetry is structural: the agency can operate despite legislative constraint because the legislature lacks capacity to monitor agency operations in real time. The extraction flow is not from legislator coercion but from capacity asymmetry that allows agencies to drift beyond legislative intent. The oversight beneficiary (executive) derives d from arbitrage exit — the agency can always claim implementation discretion, budget limitations, or changed circumstances to justify policy drift without legislative detection. This produces effective extraction despite formally legitimate executive authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled rope at the moderate agent (legislative) perspective because it exhibits both genuine coordination (legislative oversight enables policy implementation and constraint) and genuine extraction (capacity asymmetry allows regulatory drift beyond legislative intent). The mandatrophy resolves by recognizing that this is not pure extraction masquerading as coordination (snare) nor pure coordination (rope) — it is genuinely hybrid. The legislative body benefits from oversight capacity in principle (it enables them to fulfill constitutional function) while simultaneously bearing extraction costs from the capacity gap (it prevents them from detecting drift). The constraint would become rope if capacity were adequate; it would become snare if legislatures had zero ability to constrain agencies even in principle. The current state is the hybrid tangled rope: coordination function is real (statutes do constrain agencies) but extracted by capacity asymmetry (agencies exploit the fact that drift is undetectable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oversight_capacity_measurement,
    'What constitutes ''adequate'' legislative oversight capacity? Is the standard enforcement capacity, information access, or deterrence outcome?',
    'Empirical comparison of enforcement rates, regulatory challenge success rates, and agency compliance with legislative intent across jurisdictions with different oversight funding levels',
    'If standard is enforcement: current capacity is severely deficient (extractiveness likely > 0.70). If standard is information access: current capacity is moderate (current 0.58 is accurate). If standard is deterrence outcome: capacity may be sufficient for agencies to internalize legislative intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oversight_capacity_measurement, empirical, 'Definition and measurability of adequate oversight capacity').

omega_variable(
    agency_autonomy_necessity,
    'What portion of executive agency autonomy is structurally necessary for effective governance versus rent-seeking extraction by agencies?',
    'Cross-national comparison of delegation patterns, audit rates, and policy stability in different oversight regimes; analysis of which agency decisions produce better outcomes with vs without legislative scrutiny',
    'If agency autonomy is mostly necessary: constraint is primarily coordination (more rope, less snare). If autonomy is mostly extraction: constraint is asymmetric exploitation of oversight gaps (more snare, less rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_autonomy_necessity, empirical, 'Whether executive agency autonomy is necessary or extractive').

omega_variable(
    information_technology_disruption,
    'Can digital platforms, automated compliance tracking, and distributed citizen review significantly reduce the cost of legislative oversight without formal capacity expansion?',
    'Pilot programs with legislative IT platforms, automated regulatory monitoring, and crowdsourced compliance review; measurement of oversight productivity per dollar spent and per staffer hour',
    'If technology succeeds: sunset is real — oversight capacity constraint becomes solvable without legislative budget expansion, scaffold trajectory confirmed. If technology fails: structural barriers (expertise requirements, political pressure) are more binding than information access, and capacity constraint remains structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(information_technology_disruption, empirical, 'Whether technology can reduce oversight cost without formal capacity expansion').

omega_variable(
    partisan_incentive_misalignment,
    'To what degree does partisan control of legislature reduce oversight of same-party executive, independent of objective capacity constraints?',
    'Time-series analysis of legislative investigation rates, appropriations scrutiny, and agency accountability across periods of unified vs divided government and partisan alignment',
    'If partisan incentive dominates: capacity constraints are secondary — increasing staff alone will not improve oversight (theater remains high). If capacity is primary constraint: oversight will improve with funding regardless of partisanship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partisan_incentive_misalignment, empirical, 'Whether partisan alignment reduces oversight independent of capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legislative_oversight_capacity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(leg_oversight_tr_t0, legislative_oversight_capacity, theater_ratio, 0, 0.45).
narrative_ontology:measurement(leg_oversight_tr_t10, legislative_oversight_capacity, theater_ratio, 10, 0.6).
narrative_ontology:measurement(leg_oversight_tr_t20, legislative_oversight_capacity, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(leg_oversight_be_t0, legislative_oversight_capacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(leg_oversight_be_t10, legislative_oversight_capacity, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(leg_oversight_be_t20, legislative_oversight_capacity, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legislative_oversight_capacity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legislative_oversight_capacity, 0.12).
narrative_ontology:affects_constraint(legislative_oversight_capacity, regulatory_capture).
narrative_ontology:affects_constraint(legislative_oversight_capacity, administrative_state_expansion).
narrative_ontology:affects_constraint(legislative_oversight_capacity, budget_process_dysfunction).

% DUAL FORMULATION NOTE:
% Legislative oversight capacity is upstream of regulatory capture — the capacity bottleneck enables agencies to be captured by regulated industries without legislative detection. The administrative state expansion is downstream — agency autonomy expands into the oversight gap. Decomposition: write separate stories for structural capacity limits (this story) vs political incentive failure (partisan oversight reduction) vs institutional degradation (committee hearing theater) if those components show materially different ε values under different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legislative_oversight_capacity, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
