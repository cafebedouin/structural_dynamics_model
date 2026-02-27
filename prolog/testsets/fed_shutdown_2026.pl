% ============================================================================
% CONSTRAINT STORY: fed_shutdown_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fed_shutdown_2026, []).

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
 *   constraint_id: fed_shutdown_2026
 *   human_readable: The 2026 DHS/ICE Funding Standoff
 *   domain: political/economic
 *
 * SUMMARY:
 *   The 2026 DHS/ICE funding standoff represents a structural clash between
 *   executive enforcement autonomy and legislative oversight authority,
 *   triggered by deaths of detainees in ICE custody. The standoff creates a
 *   classic federal shutdown dynamic in which two groups — federal workers
 *   without payment and citizens dependent on federal services — bear the
 *   costs of a dispute to which they are not parties. The constraint exhibits
 *   high theater ratio (0.81) because both parties gain electoral positioning
 *   regardless of policy outcome: Republicans demonstrate enforcement
 *   resolve, Democrats demonstrate oversight commitment, and neither party
 *   faces direct costs. Federal workers and service dependents face the
 *   extraction with no exit option (trapped). ICE leadership faces the
 *   constraint as a coordination mechanism that forces institutional
 *   visibility and may yield minor oversight concessions while preserving
 *   enforcement autonomy (rope). The analytical observer sees the shutdown as
 *   evidence of system dysfunction — the budgeting process has collapsed as a
 *   mechanism for resolving disputes over constitutional authority
 *   allocation.
 *
 * KEY AGENTS:
 *   - Federal Workers: Primary victims (powerless/trapped) — lose wages and benefits; cannot opt out of employment dependency
 *   - Service Dependents: Primary victims (powerless/trapped) — lose access to Medicare processing, VA care, food safety oversight; cannot bypass federal services
 *   - Detained Immigrants: Secondary victims (powerless/trapped) — face delayed processing and reduced services; no exit from detention
 *   - Congressional Oversight Coalition: Organized advocates (organized/constrained) — benefit from constraint as visibility mechanism but face shutdown extension costs
 *   - ICE Enforcement Leadership: Primary beneficiary (institutional/arbitrage) — controls outcome through operational continuity; can concede minor oversight while retaining autonomy
 *   - Partisan Political Ecosystem: Theater beneficiary (powerful/mobile) — both parties gain electoral positioning from standoff; can exit through negotiated settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fed_shutdown_2026, 0.58).
domain_priors:suppression_score(fed_shutdown_2026, 0.68).
domain_priors:theater_ratio(fed_shutdown_2026, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fed_shutdown_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(fed_shutdown_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fed_shutdown_2026, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fed_shutdown_2026, snare).
narrative_ontology:human_readable(fed_shutdown_2026, "The 2026 DHS/ICE Funding Standoff").
narrative_ontology:topic_domain(fed_shutdown_2026, "political/economic").

domain_priors:requires_active_enforcement(fed_shutdown_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fed_shutdown_2026, executive_immigration_enforcement_capacity).
narrative_ontology:constraint_victim(fed_shutdown_2026, federal_workers_without_payment).
narrative_ontology:constraint_victim(fed_shutdown_2026, citizens_dependent_on_federal_services).
narrative_ontology:constraint_victim(fed_shutdown_2026, immigrants_in_detention_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL WORKERS & SERVICE DEPENDENTS (SNARE) — Frontline federal workers and citizens dependent on federal services (Medicare processing, VA care, food safety inspections, immigration adjudication) experience the shutdown as pure extraction without coordination benefit. No exit available; cannot opt out of dependency on federal systems. Maximum experienced extraction through loss of wages and services.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DETAINED IMMIGRANTS (SNARE) — Individuals in ICE detention during shutdown experience the standoff as pure constraint with no exit. Detention operations continue but with reduced services, medical care delays, and administrative processing halts. Structurally trapped; no coordination benefit; maximum suppression through absence of alternatives.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL OVERSIGHT COALITION (TANGLED ROPE) — Democratic and moderate Republican legislators advocating for ICE oversight reforms experience both coordination and extraction. The constraint performs a coordination function: it forces reckoning with deaths in custody and demands for transparency. But it also extracts through uncertainty costs, reputational risk, and the threat of shutdown extension. Constrained exit because backing down looks like abandonment of oversight; pressing forward risks shutdown extension. Mixed benefit and cost.
constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ICE ENFORCEMENT LEADERSHIP (ROPE) — ICE administration experiences the standoff as pure coordination: the shutdown funding constraint forces clarification of its operational mandate and forces visibility of custody practices. Leadership benefits from either outcome — if they capitulate, they retain operational authority; if they resist and force concessions on oversight, they preserve enforcement capacity while appearing responsive. Arbitrage exit: they can walk away through minimal concessions that leave enforcement largely intact. Net beneficiary of the constraint as a coordination mechanism.
constraint_indexing:constraint_classification(fed_shutdown_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PARTISAN BUDGET THEATER (PITON) — The shutdown standoff as pure political theater: both parties benefit electorally from performative stances (Republicans defending enforcement; Democrats demanding oversight) while the actual operational consequences are minimal — ICE continues detention operations, essential services continue, and the primary impact is wage delays for 200K+ federal workers. The theater ratio (0.81) reflects that political positioning dominates actual policy outcomes. This constraint persists through partisan inertia, not because it solves a real coordination problem. Degraded function: the shutdown mechanism was designed to force budget compromise; it now primarily generates electoral positioning for the next cycle.
constraint_indexing:constraint_classification(fed_shutdown_2026, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SYSTEM DYSFUNCTION (SNARE) — From a systems perspective, the shutdown constraint reveals that the federal budgeting process has collapsed as a coordination mechanism. The shutdown hostage-taking (using payment to 200K+ federal workers and access to critical services as leverage) represents the system's failure to resolve disputes over immigration enforcement and oversight through normal legislative process. The constraint persists because neither party can afford to back down without appearing weak to their base. This perspective identifies the constraint as structural dysfunction, not a feature.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fed_shutdown_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fed_shutdown_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fed_shutdown_2026, TR),
    TR >= 0.70.

:- end_tests(fed_shutdown_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from federal workers and service dependents through wage loss and service disruption, but the extraction is not maximal because: (1) ICE operations continue largely unchanged, (2) essential federal services (Social Security, Medicare) continue, and (3) the shutdown duration is typically 1-3 weeks, not months. The extractiveness reflects the real costs to the powerless without overstating system-wide paralysis. Suppression (0.68): High. Federal workers and service dependents face near-total suppression of alternatives — they cannot opt out of federal employment or federal services, they cannot negotiate individual exemptions, and the shutdown mechanism itself is coercive (threat of continuing payment loss). Congressional advocates face constrained alternatives (can negotiate concessions or back down; cannot force resolution). Suppression is not maximal (0.99) because congressional actors retain some negotiating power. Theater ratio (0.81): High and rising. The shutdown constraint generates pure political theater in the partisan positioning dimension (both parties benefit electorally from their stances) while the actual policy outcomes are minimal. Initial theater ratio (0.62) reflects the genuine underlying dispute about ICE oversight. Rising theater ratio (0.81 by day 7) reflects that partisan positioning increasingly dominates the public narrative while actual policy negotiations stall. The increase from 0.62 to 0.81 over the interval models Goodhart drift: the constraint's original function (force budget agreement on ICE oversight) is being replaced by its performative function (generate electoral narrative).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the federal worker (snare) and ICE leadership (rope) is maximal. The worker experiences pure extraction with no exit and no coordination benefit — the shutdown constraint extracts their wages and livelihood to force a dispute they do not participate in. ICE leadership experiences the constraint as coordination that forces institutional transparency and may yield minor concessions while they retain operational autonomy. Both are looking at the same structural reality: the shutdown mechanism ties ICE funding to budget negotiations. The worker's structural position makes it a snare; the leadership's structural position makes it a rope. The piton perspective identifies the constraint as degraded theater — the shutdown mechanism's original function (force timely budget agreement) has atrophied, replaced by its performative function (generate partisan electoral narrative). Federal workers bear the costs of this degradation; partisan actors benefit from it.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is derived from their structural position relative to the extraction flow. Federal workers and service dependents are powerless agents with trapped exit options — they experience maximum directionality (d ≈ 0.95) because they bear costs they did not create and cannot escape. ICE leadership is an institutional beneficiary with arbitrage exit options — they experience low directionality (d ≈ 0.15) because they control the outcome and face minimal personal cost. Congressional oversight advocates are organized agents with constrained exit options — they experience moderate-high directionality (d ≈ 0.55) because they created the constraint (through oversight demands) but cannot easily escape the shutdown dynamics they triggered. The partisan political ecosystem experiences low directionality (d ≈ 0.20) because both parties can exit through settlement at any point; they choose to persist through the shutdown for electoral benefit. The piton classification derives from the theater gate: the constraint's primary function (forcing ICE budget agreement) is degraded, replaced by performative positioning, yet the constraint persists through institutional inertia (the shutdown mechanism is structurally embedded in the budgeting process).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE vs. PITON RESOLUTION: The constraint is primarily a Snare (federal workers and service dependents experience maximum extraction without exit). The piton classification captures an important secondary insight: the shutdown mechanism itself has degraded from a coordination tool (force timely budget agreement) to a performative tool (generate partisan positioning). The mandatrophy is resolved by recognizing that both classifications are correct from different temporal perspectives. Short-term (immediate perspective): Snare. Federal workers face wage loss and service dependents face service disruption as immediate extraction. Long-term (civilizational perspective): Piton. The shutdown mechanism's function has decayed because neither party faces sufficient cost to force capitulation — the constraint persists through institutional inertia (it is embedded in the budget process) not because it works (it generates electoral benefit regardless of outcome). The rising theater ratio (0.62 to 0.81) models this decay: the functional component (actual policy negotiation on ICE oversight) is being replaced by the performative component (partisan positioning). Federal workers pay the cost of this institutional degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oversight_demand_legitimacy,
    'Are the deaths in ICE custody that triggered the standoff attributable to systemic failures requiring institutional reform, or to individual incidents that do not warrant broader operational constraints?',
    'Forensic review of custody protocols; comparison of ICE death rates to comparable detention systems; analysis of whether deaths were preventable through existing operational changes',
    'If systemic: oversight demands are structural and the shutdown constraint will persist until resolved. If individual: oversight demands lack empirical grounding and the constraint becomes pure political theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oversight_demand_legitimacy, empirical, 'Whether ICE custody deaths reflect systemic failures or individual incidents').

omega_variable(
    enforcement_capacity_vulnerability,
    'Does ICE operations actually depend on immediate DHS appropriations, or can the agency continue enforcement-critical functions indefinitely through continuing resolutions and carryover authority?',
    'Analysis of ICE budget structure; assessment of which functions are immediately halted vs. deferred during shutdowns; historical data on detention operations continuity across prior shutdowns',
    'If dependent: shutdown pressure is real and agencies must negotiate. If autonomous: the shutdown mechanism has zero coercive power and both parties are engaged in pure theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_vulnerability, empirical, 'Whether ICE enforcement capacity depends on immediate appropriations').

omega_variable(
    congressional_negotiating_resolve,
    'Will congressional oversight advocates maintain their funding demands through an extended shutdown (2+ weeks), or will they capitulate under pressure from federal workers'' unions and service disruption complaints?',
    'Tracking of congressional voting positions; union pressure campaigns; polling of constituent impact sentiment; historical comparison to prior shutdown endurance patterns',
    'If maintain: constraint will force policy concessions on ICE oversight. If capitulate: constraint collapses into pure theater and ICE retains operational autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_negotiating_resolve, empirical, 'Congressional resolve to maintain ICE oversight demands through extended shutdown').

omega_variable(
    alternative_oversight_mechanisms,
    'Could ICE oversight be achieved through executive order, inspector general investigation, or legislative rider without leveraging the shutdown mechanism?',
    'Legal analysis of executive authority over ICE operations; IG investigative scope; procedural analysis of legislative riders; comparison to prior oversight victories not requiring shutdowns',
    'If alternative mechanisms exist: the shutdown constraint is chosen, not imposed — meaning both parties prefer the theater to actual reform. If alternatives are exhausted: the shutdown is a genuine last-resort mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_oversight_mechanisms, conceptual, 'Whether ICE oversight could be achieved through non-shutdown mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fed_shutdown_2026, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedshut_tr_t0, fed_shutdown_2026, theater_ratio, 0, 0.62).
narrative_ontology:measurement(fedshut_tr_t3, fed_shutdown_2026, theater_ratio, 3, 0.71).
narrative_ontology:measurement(fedshut_tr_t7, fed_shutdown_2026, theater_ratio, 7, 0.81).

% Extraction over time
narrative_ontology:measurement(fedshut_be_t0, fed_shutdown_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fedshut_be_t3, fed_shutdown_2026, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(fedshut_be_t7, fed_shutdown_2026, base_extractiveness, 7, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fed_shutdown_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(fed_shutdown_2026, executive_enforcement_congressional_oversight).
narrative_ontology:affects_constraint(fed_shutdown_2026, federal_budgeting_mechanism_failure).

% DUAL FORMULATION NOTE:
% The 2026 standoff is downstream of the underlying constitutional tension between executive immigration enforcement authority and congressional appropriation power. That structural constraint (executive_enforcement_congressional_oversight) has ε ≈ 0.35 (tangled rope at civilizational scale). The funding standoff (fed_shutdown_2026) has higher ε (0.58) because it operates at immediate timescale and adds coercive extraction through wage loss. The budgeting mechanism failure (federal_budgeting_mechanism_failure) is a separate constraint with its own ε ≈ 0.52 reflecting the general collapse of the budget process as a coordination tool. The standoff leverages mechanism failure to apply coercion in the underlying institutional dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fed_shutdown_2026, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
