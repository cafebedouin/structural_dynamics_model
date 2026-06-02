% ============================================================================
% CONSTRAINT STORY: international_dispute_resolution_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_dispute_resolution_capacity, []).

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
 *   constraint_id: international_dispute_resolution_capacity
 *   human_readable: International Dispute Resolution Capacity Constraint
 *   domain: international_relations/institutional_governance
 *
 * SUMMARY:
 *   International dispute resolution capacity represents a structural
 *   constraint operating at the intersection of institutional coordination
 *   and power asymmetry. States require mechanisms for resolving cross-border
 *   disputes (treaties, trade, maritime boundaries, environmental damage)
 *   without resorting to force. International institutions (ICJ, arbitration
 *   forums, WTO dispute settlement) provide these mechanisms but operate
 *   under conditions of profound power imbalance: powerful states can afford
 *   extended proceedings and can exit to alternative mechanisms; weak states
 *   depend on institutional access and face resource constraints that limit
 *   their ability to participate effectively. The constraint exhibits the
 *   full spectrum of DR classifications depending on observer position. It
 *   functions as genuine coordination (enabling disputes to be resolved
 *   peacefully through established forums) while simultaneously extracting
 *   from participants (through cost allocation, procedural complexity, and
 *   power-asymmetric outcomes). The rising theater_ratio indicates that
 *   procedural elaboration is increasing faster than dispute resolution
 *   efficacy, suggesting early-stage institutional degradation consistent
 *   with piton dynamics.
 *
 * KEY AGENTS:
 *   - Weak States: Primary victims (powerless/trapped) — lack resources for extended proceedings, lack political leverage to influence institutional outcomes, face unresolved grievances as the cost of exit
 *   - Powerful States: Primary beneficiaries (institutional/arbitrage) — can afford extended proceedings, can influence institutional interpretation through political pressure, can exit to alternative mechanisms (bilateral negotiation, economic coercion)
 *   - International Dispute Resolution Institutions: Institutional actors (institutional/arbitrage) — benefit from legitimacy, workload authority, and prestige; coordinate dispute resolution while allocating costs asymmetrically
 *   - Regional Dispute Resolution Coalitions: Organized secondary actors (organized/constrained) — benefit from coordinated capacity building while extracting through membership gatekeeping and selective enforcement
 *   - Mid-Tier States: Moderate secondary actors (moderate/constrained) — can participate but lack power to shape outcomes; benefit from formal mechanisms while bearing participation costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_dispute_resolution_capacity, 0.55).
domain_priors:suppression_score(international_dispute_resolution_capacity, 0.52).
domain_priors:theater_ratio(international_dispute_resolution_capacity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_dispute_resolution_capacity, extractiveness, 0.55).
narrative_ontology:constraint_metric(international_dispute_resolution_capacity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(international_dispute_resolution_capacity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_dispute_resolution_capacity, tangled_rope).
narrative_ontology:human_readable(international_dispute_resolution_capacity, "International Dispute Resolution Capacity Constraint").
narrative_ontology:topic_domain(international_dispute_resolution_capacity, "international_relations/institutional_governance").

domain_priors:requires_active_enforcement(international_dispute_resolution_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_dispute_resolution_capacity, powerful_states).
narrative_ontology:constraint_beneficiary(international_dispute_resolution_capacity, institutional_dispute_resolvers).
narrative_ontology:constraint_victim(international_dispute_resolution_capacity, weak_states).
narrative_ontology:constraint_victim(international_dispute_resolution_capacity, dispute_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAK STATE (SNARE) — Faces a dispute requiring international resolution but lacks resources for extended proceedings, expert legal representation, or political leverage to influence outcomes. Exit from the constraint system means accepting unresolved grievance or unilateral adverse settlement. The system extracts through cost burden (expertise, time, financial resources) while offering minimal guarantee of favorable resolution. No alternative dispute mechanisms are accessible.
constraint_indexing:constraint_classification(international_dispute_resolution_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL COALITIONS (TANGLED ROPE) — Organized groups of states benefit from shared dispute resolution capacity (coordination function: pooling legal expertise, establishing precedent, reducing transaction costs for members). Simultaneously extract from non-member states through capacity gatekeeping and selective enforcement. Member states gain access; non-members bear disproportionate burden. Active enforcement through institutional commitment and network effects.
constraint_indexing:constraint_classification(international_dispute_resolution_capacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RESOLUTION INSTITUTIONS (ROPE) — ICJ, UNCLOS arbitration, WTO dispute settlement function as pure coordination mechanisms from the institutional perspective. They solve the collective action problem of dispute resolution by establishing neutral forums, reducing verification costs, and enabling reciprocal commitment. Institutions themselves benefit from legitimacy and workload (arbitrage exit: can shift dispute-handling capacity across domains). Net beneficiary position through institutional prestige and resource allocation authority.
constraint_indexing:constraint_classification(international_dispute_resolution_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POWERFUL STATES (SCAFFOLD) — Can exit the international resolution system and resort to bilateral negotiation, military pressure, or economic coercion. The formal dispute resolution constraint is tolerated because it provides limited benefits (international legitimacy, rhetorical cover) at low cost (powerful states can afford extended proceedings and control interpretation through institutional influence). This perspective sees the constraint as temporary scaffolding — as power imbalances persist, powerful states gradually substitute direct mechanisms, eroding the need for formal resolution.
constraint_indexing:constraint_classification(international_dispute_resolution_capacity, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER — INSTITUTIONAL DEGRADATION (PITON) — From a civilizational horizon, the international dispute resolution system shows signs of degradation. Theater has risen (increased emphasis on institutional legitimacy and procedural theater rather than actual dispute resolution efficacy). The functional capacity of institutions has not kept pace with dispute volume and complexity. Powerful states increasingly bypass formal mechanisms. The institutions persist through inertia and formal legitimacy rather than because they are the most effective resolution pathway. Theater ratio indicates that institutional performance involves substantial performative activity (ceremonial procedures, rhetorical framing) masking limited practical dispute resolution.
constraint_indexing:constraint_classification(international_dispute_resolution_capacity, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: MID-TIER STATES (TANGLED ROPE) — Face moderate barriers to dispute resolution access. Can afford some legal expertise and institutional participation but lack the power to shape institutional outcomes. Benefit from the existence of formal resolution mechanisms (avoiding unilateral powerful-state dominance) while bearing costs of time, legal resources, and uncertainty. The system constrains outcomes while enabling dispute participation. Active enforcement through institutional procedures and power dynamics within forums.
constraint_indexing:constraint_classification(international_dispute_resolution_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_dispute_resolution_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_dispute_resolution_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_dispute_resolution_capacity, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_dispute_resolution_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_dispute_resolution_capacity, TR),
    TR >= 0.70.

:- end_tests(international_dispute_resolution_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The constraint extracts from weak and mid-tier states through cost allocation (legal expertise, proceeding duration, institutional participation) while distributing benefits toward powerful states and institutions. However, extractiveness is not maximal (0.75+) because the system does provide genuine value: weak states can bring disputes without military capacity, and the existence of formal mechanisms constrains powerful state unilateralism. The extraction is embedded in coordination rather than pure predation. Suppression (0.52): Moderate-high. Barriers to access include financial constraints, expertise requirements, procedural complexity, and power asymmetries in institutional decision-making. Weak states face significant suppression of their dispute-filing capacity. Suppression is not total (0.80+) because states can theoretically access institutions, but actual access faces severe practical barriers. Theater ratio (0.58): Moderate. International dispute resolution institutions have elevated procedural complexity (increasing theater) as they have developed sophisticated legal frameworks. However, genuine coordination function remains — procedures are not purely performative. The rising trajectory (0.42 → 0.58 over 45 years) indicates increasing theatrical component relative to functional dispute resolution, consistent with institutional aging.
 *
 * PERSPECTIVAL GAP:
 *   Weak states perceive maximum extraction (snare) because they experience the full cost-burden without exit option. Powerful states perceive pure coordination (rope/scaffold) because they benefit from institutional legitimacy while maintaining exit capacity. Institutions perceive pure coordination (rope) because they solve the genuine problem of establishing neutral dispute forums. The analytical observer perceives institutional degradation (piton) because theater is rising faster than efficacy. This gap is not a measurement error — it reflects structural reality. The same institutional system solves a genuine problem (enabling dispute resolution without military escalation) while simultaneously extracting from those with limited alternatives. The constraint is both rope and snare, depending on the observer's structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural relationship to the extraction flow and their power to escape it. Weak states (powerless/trapped) have d ≈ 0.95 — maximum target position because they cannot exit and must bear costs. Powerful states (institutional/arbitrage) have d ≈ 0.10 — low target position because they can exit and benefit from institutional legitimacy while externalizing costs. International institutions (institutional/arbitrage) have d ≈ 0.05 — full beneficiary position because they allocate costs and capture legitimacy value. Regional coalitions (organized/constrained) have d ≈ 0.45 — moderate position because membership provides benefits but non-membership ensures extraction. Mid-tier states (moderate/constrained) have d ≈ 0.55 — slightly above-center because participation is necessary but costly and outcomes are constrained by power asymmetry. The directionality values confirm the tangled-rope classification: the constraint provides genuine coordination (low d for beneficiaries, institutions) while extracting from those without exit capacity (high d for weak states).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by demonstrating that tangled rope is the correct classification: genuine coordination function coexists with asymmetric extraction. The mistake would be to classify this purely as rope (ignoring the extraction of weak states) or purely as snare (ignoring the real benefit of having a dispute resolution mechanism). The tangled-rope classification captures that the system provides legitimate coordination while extracting value through cost allocation and power asymmetry. The presence of beneficiaries (powerful states, institutions), victims (weak states), and active enforcement (institutional procedures, power dynamics) confirms tangled-rope gates are satisfied. The rising theater ratio indicates early-stage institutional degradation, which is consistent with long-term piton drift but does not change current classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_willingness_distinction,
    'Is the constraint primarily a lack of dispute resolution capacity (insufficient institutional resources, expertise, procedures) or a lack of state willingness to use available mechanisms?',
    'Comparative analysis of dispute caseload growth vs institutional resource growth; empirical measurement of utilization rates vs stated capacity; interviews with state representatives about barriers to dispute filing',
    'If capacity-constrained: institutional investment and procedural reform can address the bottleneck (Scaffold/Rope classification appropriate). If willingness-constrained: the constraint is fundamentally about power asymmetry and institutional legitimacy gaps (Snare/Tangled Rope classification more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_willingness_distinction, empirical, 'Whether bottleneck is institutional capacity or state willingness').

omega_variable(
    powerful_state_exit_sustainability,
    'Can powerful states sustain indefinite exit from formal international dispute resolution without destabilizing the system for weak states?',
    'Game-theoretic analysis of reciprocal non-cooperation; historical analysis of system collapse scenarios (League of Nations, pre-WTO trade disputes); measurement of powerful-state participation rates over time',
    'If sustainable: powerful states will continue to erode the system, shortening the sunset horizon and accelerating piton degradation (Scaffold becomes Piton faster). If unsustainable: powerful states must maintain formal participation, stabilizing the constraint as Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(powerful_state_exit_sustainability, conceptual, 'Sustainability of powerful-state exit from dispute resolution').

omega_variable(
    alternative_resolution_mechanism_emergence,
    'Are regional and bilateral dispute resolution mechanisms emerging as genuine alternatives that will replace international institutional mechanisms?',
    'Comparative case analysis of dispute resolution outcomes: bilateral vs institutional vs regional forums; measurement of state preferences revealed through dispute forum selection; institutional market share over time',
    'If genuine alternatives emerging: international institutions are degrading into pitons faster (scaffold sunset timeline shortens). If alternatives remain supplementary: formal institutions maintain tangled-rope classification with longer-term stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_resolution_mechanism_emergence, empirical, 'Whether alternative dispute mechanisms are replacing international institutions').

omega_variable(
    legitimacy_vs_efficacy_decoupling,
    'Are international dispute resolution institutions maintaining legitimacy through procedural theater while actual dispute resolution efficacy (settlement rates, implementation compliance) declines?',
    'Longitudinal analysis of dispute settlement rates, implementation compliance rates, duration of proceedings; correlation analysis between institutional procedural elaboration and efficacy metrics; analysis of theater components vs functional necessity',
    'If decoupled: theater_ratio will continue rising, accelerating piton classification. If coupled: theater reflects genuine functional necessity, and institutions remain Tangled Rope or Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_vs_efficacy_decoupling, empirical, 'Decoupling of institutional legitimacy from dispute resolution efficacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_dispute_resolution_capacity, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intl_disp_tr_t0, international_dispute_resolution_capacity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(intl_disp_tr_t15, international_dispute_resolution_capacity, theater_ratio, 15, 0.5).
narrative_ontology:measurement(intl_disp_tr_t30, international_dispute_resolution_capacity, theater_ratio, 30, 0.58).
narrative_ontology:measurement(intl_disp_tr_t45, international_dispute_resolution_capacity, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(intl_disp_be_t0, international_dispute_resolution_capacity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(intl_disp_be_t15, international_dispute_resolution_capacity, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(intl_disp_be_t30, international_dispute_resolution_capacity, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(intl_disp_be_t45, international_dispute_resolution_capacity, base_extractiveness, 45, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_dispute_resolution_capacity, enforcement_mechanism).
narrative_ontology:affects_constraint(international_dispute_resolution_capacity, maritime_boundary_dispute_resolution).
narrative_ontology:affects_constraint(international_dispute_resolution_capacity, trade_dispute_settlement_capacity).
narrative_ontology:affects_constraint(international_dispute_resolution_capacity, environmental_damage_liability_determination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_dispute_resolution_capacity, powerful, 0.12).
constraint_indexing:directionality_override(international_dispute_resolution_capacity, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
