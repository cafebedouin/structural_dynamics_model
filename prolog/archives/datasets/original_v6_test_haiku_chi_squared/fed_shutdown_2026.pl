% ============================================================================
% CONSTRAINT STORY: fed_shutdown_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The 2026 DHS/ICE funding standoff represents a structural constraint
 *   where federal appropriations authority is weaponized to extract policy
 *   concessions outside the legislative process. Following deaths in ICE
 *   detention facilities under disputed circumstances, Congress and the
 *   Executive branch deadlock over whether ICE operations should continue
 *   under existing oversight mechanisms. A hardline enforcement coalition
 *   (House Republican leadership, anti-immigration advocacy groups, Executive
 *   ICE leadership) demands suspension of specific oversight functions — OIG
 *   audits, detention facility inspections, funding condition checks — as a
 *   condition for passing funding bills. This demand is embedded in must-pass
 *   appropriations legislation, creating a constraint: federal employees lack
 *   wages, vulnerable populations lack protection mechanisms, and service
 *   users lack guarantees of government function. The shutdown persists
 *   because the demand structure cannot be resolved through normal
 *   legislative negotiation — it is presented as non-negotiable operational
 *   necessity, not as a policy choice. The constraint exhibits all
 *   characteristics of a snare: high suppression (federal employees and
 *   immigrants cannot exit the consequences), high extractiveness (hardline
 *   coalition gains policy concessions worth billions in operational
 *   freedom), and rising theater ratio (performative debate about 'safety'
 *   and 'operational necessity' masks that oversight removal is the primary
 *   demand). The analytical frame risks naturalizing the coercive mechanism
 *   as inherent to separated powers governance, when it is actually a
 *   degraded appropriations process instrumentalized for policy extraction.
 *
 * KEY AGENTS:
 *   - Hardline Immigration Enforcement Coalition: Primary beneficiary (powerful/arbitrage) — seeks removal of ICE oversight mechanisms; has veto power through House Republican leadership; gains operational freedom and policy concessions worth billions annually
 *   - Federal Employees (Unpaid): Primary victim (powerless/trapped) — denied wages during shutdown; pressured to work unpaid or face employment termination; unable to negotiate or exit
 *   - Vulnerable Immigrant Populations: Primary victim (powerless/trapped) — subject to ICE enforcement with suspended oversight; detention facility conditions deteriorate; no representation in shutdown negotiations
 *   - Congressional Democratic Caucus: Secondary victim (moderate/constrained) — must choose between capitulating to demands or extending shutdown; both options extract political costs
 *   - Essential Service Users (Healthcare, Food Safety, etc.): Secondary victim (powerless/constrained) — receive degraded services during shutdown; no control over shutdown resolution
 *   - Congressional Appropriations Process: Institutional actor (institutional/arbitrage) — nominally neutral coordination mechanism; degraded into policy extraction instrument; persists through inertia
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

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fed_shutdown_2026, hardline_immigration_enforcement_coalition).
narrative_ontology:constraint_beneficiary(fed_shutdown_2026, executive_agencies_discretion).
narrative_ontology:constraint_victim(fed_shutdown_2026, federal_employees).
narrative_ontology:constraint_victim(fed_shutdown_2026, vulnerable_immigrant_populations).
narrative_ontology:constraint_victim(fed_shutdown_2026, essential_service_users).
narrative_ontology:constraint_victim(fed_shutdown_2026, appropriations_process_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPAID FEDERAL EMPLOYEES (SNARE) — Trapped in shutdown with no income, no negotiating power, no ability to exit the federal system without career loss. Forced to continue essential work unpaid or face job termination. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. Pure extraction with maximum coercion.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VULNERABLE IMMIGRANT POPULATIONS (SNARE) — Trapped by immigration enforcement policies weaponized during shutdown. ICE operations continue while oversight mechanisms are suspended. No exit from enforcement apparatus; no representation in shutdown negotiations. d≈0.98, f(d)≈1.42, σ=1.0 → χ≈0.82. Maximum extraction through asymmetric vulnerability.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL DEMOCRATS (TANGLED ROPE) — Constrained by need to pass appropriations (essential coordination function) but also extracted from by majority-party veto power. Must choose between capitulating to ICE enforcement demands or extending shutdown (political extraction). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55. Mixed: genuine coordination requirement paired with asymmetric extraction.
constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HARDLINE ENFORCEMENT COALITION (ROPE) — Primary beneficiaries with exit option (compromise bill language, phased negotiations). Experience constraint as coordination: forcing oversight suspension into budget language solves their problem of operational constraints. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary; sees mechanism as functional coordination.
constraint_indexing:constraint_classification(fed_shutdown_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESSIONAL APPROPRIATIONS PROCESS (PITON) — The shutdown mechanism is a degraded institutional form: it once enforced fiscal discipline, but now functions primarily as a theater for policy coercion. The performative threat of shutdown has replaced actual budget discipline. theater_ratio≈0.81 (negotiations theater, policy riders conflated with spending authority). The process persists through inertia despite low functional value as a fiscal mechanism.
constraint_indexing:constraint_classification(fed_shutdown_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational rule-of-law perspective, the shutdown instrumentalizes the appropriations process to extract policy concessions outside legislative debate. The constraint violates the principle that enforcement policy should be debated in its own committees, not hidden in must-pass budget bills. This is extraction normalized as governance. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(fed_shutdown_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fed_shutdown_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): The hardline coalition extracts policy concessions (ICE oversight suspension) worth billions in operational freedom annually. The extraction is not total because it requires legislative negotiation and cannot be sustained indefinitely (essential services failures force resolution). The rising trajectory (0.38→0.58 over interval) reflects escalation of demands and hardening of positions. Suppression (0.68): Federal employees have zero exit options — they cannot quit without losing careers, and they are pressured to work unpaid. Immigrants have zero exit options — they cannot escape enforcement apparatus. Congressional Democrats have some exit option (compromise) but it is politically costly. Suppression is high but not maximal because federal employees can eventually refuse to work (though with career consequences) and political actors retain legislative tools. Theater ratio (0.81): Exceptionally high. The public debate centers on claims about 'operational necessity,' 'safety,' and 'fiscal discipline,' but the actual negotiation is purely about whether oversight mechanisms continue. The performative layer (debate about what's 'necessary') is almost as large as the functional layer (actual oversight suspension). Claimed type (Snare): Justified by high suppression and high extractiveness. Federal employees and immigrants cannot exit; the hardline coalition captures the primary benefit.
 *
 * PERSPECTIVAL GAP:
 *   The hardline coalition sees this as a Rope (coordination problem: funding bills require resolution, and ICE operational constraints must be addressed). Federal employees and immigrants see a Snare (coercion with no exit). Democrats see a Tangled Rope (genuine appropriations coordination requirement paired with extraction through veto power). The appropriations process itself is a Piton — it once functioned as a budgetary discipline mechanism but now primarily serves as a vehicle for policy coercion through theater. The analytical observer sees the constraint as a Snare that risks being naturalized as inherent to separated powers (false summit). The gap reveals that beneficiaries experience the constraint as functional (coordination), while victims experience it as extraction, and institutional actors see their own degradation but cannot escape it.
 *
 * DIRECTIONALITY LOGIC:
 *   Hardline enforcement coalition: Beneficiary + arbitrage exit (can negotiate compromise, can escalate, has legislative veto power) → d≈0.08, f(d)≈-0.10. Net beneficiary; effectively subsidized by others' suffering. Federal employees: Victim + trapped exit (cannot quit without career loss, pressured to work unpaid) → d≈0.95, f(d)≈1.42. Maximum extraction. Vulnerable immigrants: Victim + trapped exit (cannot escape enforcement apparatus, no political voice) → d≈0.98, f(d)≈1.42. Absolute extraction. Democrats: Victim of extraction + constrained exit (can compromise but at political cost, cannot force resolution without Republican support) → d≈0.65, f(d)≈0.95. Significant extraction. Appropriations process: Institutional actor + arbitrage exit (can theoretically change rules, but inertia prevents it) → d≈0.05, f(d)≈-0.10. Piton classification comes from theater_ratio ≥ 0.70, not from high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT resolve mandatrophy, and this is analytically significant. The snare classification requires that we accept: (1) the hardline coalition genuinely benefits from oversight suspension as an operational necessity, AND (2) federal employees and immigrants genuinely bear costs with no reciprocal benefit. However, the justification layer (debate about 'safety' and 'operational necessity') suggests that the framing itself is contested. If oversight suspension is genuinely necessary for ICE to function safely, the snare classification is justified. If oversight suspension is primarily about operational freedom and the safety claims are post-hoc justification, the snare classification is confirmed — the extraction includes the cost of maintaining its own legitimating narrative. The unresolved omega variables suggest that mandatrophy persists: we cannot definitively separate genuine coordination needs from pure extraction without empirical data on oversight effectiveness. This is the characteristic feature of snares at the boundary with tangled ropes — the beneficiary's narrative includes a kernel of operational truth, but the extraction is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oversight_effectiveness_empirical,
    'Do the suspended ICE oversight mechanisms (OIG audits, detention facility inspections, funding condition checks) actually reduce harms, or are they performative compliance theater?',
    'Comparative analysis of ICE detention harm rates during periods with vs. without active oversight; tracking of corrective actions from OIG reports; correlation between oversight findings and operational changes',
    'If oversight is effective: suppression value (0.68) understates the extraction — coercion includes removal of functional safeguards. If oversight is performative: snare classification is justified but political framing of ''safety concerns'' is exposed as justification layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oversight_effectiveness_empirical, empirical, 'Whether suspended ICE oversight mechanisms reduce actual harms').

omega_variable(
    shutdown_duration_viability_boundary,
    'What shutdown duration triggers cascading failures in essential services (healthcare, food safety, disaster response) that create political pressure independent of actors'' negotiating positions?',
    'Historical shutdown data on service degradation timelines; modeling of critical infrastructure failure cascades; analysis of when political pressure forces settlement despite negotiating stances',
    'If < 2 weeks: one side can endure shutdown costs longer than other. If > 2 weeks: technical constraints force settlement regardless of politics. Duration affects whether snare classification is stable or breaks into negotiated compromise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(shutdown_duration_viability_boundary, empirical, 'Shutdown duration threshold for cascading service failures').

omega_variable(
    immigration_enforcement_demand_coherence,
    'Is the hardline coalition''s demand for ICE oversight removal coherent with stated public safety rationale, or does the demand structure reveal extraction that incompletely justifies itself?',
    'Analysis of policy proposals: do they target specific oversight mechanisms that demonstrably impede safety functions, or do they target accountability mechanisms broadly? Comparison with other law enforcement agencies'' oversight.',
    'If coherent: the snare classification is partially justified as protection of public safety infrastructure. If incoherent: snare classification is confirmed — demand is pure extraction disguised as operational necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immigration_enforcement_demand_coherence, conceptual, 'Coherence of justification for ICE oversight removal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fed_shutdown_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedshutdown_tr_t0, fed_shutdown_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(fedshutdown_tr_t5, fed_shutdown_2026, theater_ratio, 5, 0.72).
narrative_ontology:measurement(fedshutdown_tr_t10, fed_shutdown_2026, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(fedshutdown_be_t0, fed_shutdown_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fedshutdown_be_t5, fed_shutdown_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(fedshutdown_be_t10, fed_shutdown_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fed_shutdown_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(fed_shutdown_2026, appropriations_process_hostage_taking).
narrative_ontology:affects_constraint(fed_shutdown_2026, ice_detention_facility_oversight).
narrative_ontology:affects_constraint(fed_shutdown_2026, executive_agency_discretion_boundary).

% DUAL FORMULATION NOTE:
% This constraint is structurally downstream of ICE detention facility oversight debates (which define what oversight removal would entail) and upstream of broader appropriations process hostage-taking (which reveals the generalized pattern of policy-rider extraction through must-pass bills).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fed_shutdown_2026, powerful, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
