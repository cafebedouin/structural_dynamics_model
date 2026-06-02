% ============================================================================
% CONSTRAINT STORY: deferred_maintenance_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferred_maintenance_cascade, []).

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
 *   constraint_id: deferred_maintenance_cascade
 *   human_readable: Deferred Maintenance Cascade
 *   domain: infrastructure/organizational_degradation
 *
 * SUMMARY:
 *   Deferred maintenance represents a structural constraint where current
 *   budget holders systematically shift maintenance costs to future periods,
 *   creating an extraction mechanism masked as a coordination problem. The
 *   constraint exhibits classic features of tangled rope: genuine
 *   coordination function (managing tight current budgets while sustaining
 *   essential services) coupled with asymmetric extraction (future agents and
 *   users bear escalating failure risk and emergency costs). The
 *   extractiveness grows over time (0.28 → 0.58) as deferred maintenance
 *   cascades and compound failure risks accelerate. Theater ratio rises (0.42
 *   → 0.65) as inspection rituals proliferate without corresponding repairs.
 *   The constraint appears as pure snare to users trapped in deteriorating
 *   infrastructure; as rope to short-term budget beneficiaries; as scaffold
 *   to infrastructure reformers; as piton to the degraded asset-management
 *   system; and risks appearing as natural law to analytical observers who
 *   naturalize fiscal scarcity and present bias.
 *
 * KEY AGENTS:
 *   - Current Administration/Budget Holders: Primary beneficiary (institutional/arbitrage) — captures budget savings by deferring maintenance; experiences as coordination problem requiring solution through temporal shifting
 *   - Future Users and Residents: Primary victim (powerless/trapped) — face escalating infrastructure failure risk, safety hazards, and emergency costs; cannot exit geographic dependency
 *   - Emergency Responders: Secondary victim (organized/constrained) — absorb costs of infrastructure-failure responses; experience constraint as operational burden, not budget benefit
 *   - Infrastructure Reliability (Collective Good): Tertiary victim (powerless/trapped) — abstract collective good; no agent advocates; bears risk of cascading system failures
 *   - Infrastructure Reform Coalition: Organized beneficiary (powerful/mobile) — federal grant programs, asset-management consultants, engineering societies building alternative pathways with dedicated funding streams
 *   - Asset Inspection and Maintenance System: Institutional actor (institutional/arbitrage) — maintains performative protocols; actual maintenance systematically deferred beneath ritual documentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferred_maintenance_cascade, 0.58).
domain_priors:suppression_score(deferred_maintenance_cascade, 0.62).
domain_priors:theater_ratio(deferred_maintenance_cascade, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferred_maintenance_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferred_maintenance_cascade, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(deferred_maintenance_cascade, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferred_maintenance_cascade, tangled_rope).
narrative_ontology:human_readable(deferred_maintenance_cascade, "Deferred Maintenance Cascade").
narrative_ontology:topic_domain(deferred_maintenance_cascade, "infrastructure/organizational_degradation").

domain_priors:requires_active_enforcement(deferred_maintenance_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferred_maintenance_cascade, short_term_budget_holders).
narrative_ontology:constraint_beneficiary(deferred_maintenance_cascade, current_administration).
narrative_ontology:constraint_victim(deferred_maintenance_cascade, future_users).
narrative_ontology:constraint_victim(deferred_maintenance_cascade, emergency_responders).
narrative_ontology:constraint_victim(deferred_maintenance_cascade, infrastructure_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE/CURRENT USERS (SNARE) — Cannot exit deteriorating infrastructure systems (roads, water, power grids). Trapped by geographic dependency. Bears escalating risk of catastrophic failure while current decision-makers capture budget savings. Maximum extraction with zero exit options.
constraint_indexing:constraint_classification(deferred_maintenance_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CURRENT ADMINISTRATION (TANGLED ROPE) — Coordinates essential services provision while systematically extracting from future period through deferred maintenance. Must balance budgets (coordination function) but achieves this through temporal shifting of costs (asymmetric extraction). Active enforcement of maintenance deferral through budgetary policy. Constrained by political cycle pressures and revenue limitations.
constraint_indexing:constraint_classification(deferred_maintenance_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SHORT-TERM BUDGET HOLDERS (ROPE) — Experiences constraint as pure coordination benefit. Budget savings enable other priority spending (healthcare, education, emergency services). Arbitrage opportunity: redirect maintenance funds to pressing current needs. Net beneficiary of the deferred cost structure.
constraint_indexing:constraint_classification(deferred_maintenance_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: INFRASTRUCTURE REFORM COALITION (SCAFFOLD) — Powerful actors (engineering societies, asset-management consultants, EPA, federal grant programs) recognize deferred maintenance as a solvable coordination problem with a sunset. Federal infrastructure investment programs (INFRA bill) are building alternative pathways: dedicated funding streams, asset management mandates, public-private partnerships. These create exit paths from the deferred-maintenance trap. High agency, visible sunset clause, decreasing suppression as alternatives mature.
constraint_indexing:constraint_classification(deferred_maintenance_cascade, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ASSET INSPECTION AND REPORTING SYSTEM (PITON) — Maintenance schedules, condition assessments, and asset-management protocols persist despite systematic non-compliance. Inspection theaters (paperwork, certifications, audits) substitute for actual maintenance. The ritual is maintained through institutional inertia and regulatory theater rather than function. Theater ratio high (0.65) reflects that the system performs documentation without enabling repair.
constraint_indexing:constraint_classification(deferred_maintenance_cascade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks seeing deferred maintenance as inevitable consequence of fiscal scarcity and human present bias. Under this naturalization, deferral is an immutable property of bounded rationality and budget constraints. The engine's false summit detector will flag this — the structural data reveals deferred maintenance as a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(deferred_maintenance_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferred_maintenance_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deferred_maintenance_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deferred_maintenance_cascade, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferred_maintenance_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deferred_maintenance_cascade, TR),
    TR >= 0.70.

:- end_tests(deferred_maintenance_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from future agents to benefit current budgets, but extraction is partial rather than complete. Some deferral is legitimate cost-shifting (spreading replacement costs across time); much is rent-seeking (capturing current budget benefits while offloading failure risk). The upward trajectory (0.28 → 0.58 over the interval) reflects accelerating extraction as compound degradation effects emerge. Suppression (0.62): Moderate-high. Barriers to maintenance include revenue constraints (genuine structural barrier), political cycles (institutional barrier), and information asymmetry (inspection theaters obscure actual asset conditions). Future users cannot exit. Suppression is not absolute — some maintenance occurs, and alternative funding mechanisms exist (federal grants, bonds), but accessing them requires overcoming substantial institutional friction. Theater ratio (0.65): Moderate-high. Asset management systems emphasize documentation and certification (inspection protocols, condition assessments) while actual maintenance is systematically deferred. The theater persists because it creates accountability rituals without requiring expenditure. As degradation accelerates, the gap between documented condition and actual state widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence driven by temporal position. The short-term budget holder sees rope: 'We're coordinating essential services within fiscal constraints.' The future user sees snare: 'I'm trapped in deteriorating infrastructure with no exit and escalating risk.' The reform coalition sees scaffold: 'This is a solvable coordination problem; federal investment is building exit pathways.' The asset-management system sees piton: 'We maintain inspection protocols; actual maintenance is deferred by budget decisions outside our control.' The analytical observer risks seeing mountain: 'Fiscal scarcity and present bias make deferral inevitable.' The perspectival gap is primarily temporal — the same physical infrastructure appears as a coordination benefit to present actors and as catastrophic extraction to future agents. The gap widens as compound degradation accelerates, shifting perspectives from rope/tangled_rope toward snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by: (1) structural position relative to the extraction flow (who benefits from deferred maintenance? who bears costs?), (2) temporal horizon (present vs future), and (3) exit capacity. Short-term budget holders are beneficiaries with arbitrage options (can redirect maintenance funds) → d ≈ 0.10 → low effective extraction. Future users are victims with no exit (trapped in geography + no alternative infrastructure) → d ≈ 0.92 → maximum effective extraction. Current administrators are beneficiaries but constrained by political pressure → d ≈ 0.35 → moderate extraction. Reform coalition actors are organized with mobile exit options (federal funding provides alternative pathways) → d ≈ 0.50 → symmetric constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The deferred maintenance cascade resolves mandatrophy by acknowledging that the same institutional arrangement is simultaneously a coordination mechanism AND an extraction mechanism. The coordination function is real: spreading replacement costs across time, prioritizing emergency services during budget crises, managing political constraints. The extraction is also real: future agents pay compound interest on deferred costs, failure risks concentrate on future users and marginalized populations without exit options, emergency costs dwarf planned maintenance savings. The constraint is NOT 'either coordination or extraction' — it is tangled rope: coordination for present actors embedded with asymmetric extraction against future actors. The mandatrophy resolves through temporal decomposition: the constraint coordinates present budgets while extracting from future periods. No single time horizon sees it as pure rope (only present administration) or pure snare (only future users). The analytical observer must avoid the trap of naturalizing this extraction as an inevitable consequence of fiscal scarcity — it is a contingent institutional choice to defer rather than refund, privatize rather than invest, or shift risk rather than distribute it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_threshold,
    'At what degradation level does catastrophic failure risk become imminent rather than theoretical?',
    'Infrastructure condition assessment databases; historical failure analysis correlating asset condition index to failure rates; engineering thresholds for safety-critical systems',
    'If threshold is crossed: classification shifts from tangled_rope to snare across all perspectives. Budget savings evaporate in emergency costs. Suppression increases to 0.85+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_threshold, empirical, 'Degradation threshold for catastrophic failure risk transition').

omega_variable(
    political_cycle_lock_in,
    'Does the political cycle (election frequency) structurally enforce deferred maintenance, or is deferral a contingent choice within each cycle?',
    'Cross-jurisdictional comparison of maintenance patterns; analysis of deferred maintenance rates before vs after multi-year budget stabilization initiatives',
    'If structurally enforced: constraint approaches mountain status (inevitable under democratic cycles). If contingent: constraint is purely institutional and remediable through policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_cycle_lock_in, conceptual, 'Whether political cycles structurally enforce maintenance deferral').

omega_variable(
    compound_interest_rate_of_degradation,
    'Does deferred maintenance compound (accelerating failure risk) or accumulate linearly (constant failure rate per year of deferral)?',
    'Longitudinal asset condition tracking; analysis of repair cost escalation as function of deferral time; modeling of interdependencies (e.g., water main failure causing road collapse)',
    'If compound: extraction mechanism is self-reinforcing and snare classification is inevitable. If linear: scaffold perspective becomes more credible (maintenance deferral is a temporary loan from future, not a debt spiral).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compound_interest_rate_of_degradation, empirical, 'Whether degradation compounds or accumulates linearly').

omega_variable(
    replacement_cost_externality,
    'What fraction of eventual replacement cost is paid by the entity that deferred maintenance vs by users, insurers, or emergency systems that absorb failure cascades?',
    'Comparative cost analysis of planned maintenance vs emergency repair; tracking of cost allocation across budgets (e.g., whose balance sheet absorbs a main line break)',
    'If original deferor pays replacement cost: constraint is a zero-sum temporal shift (legitimate coordination). If deferor avoids cost: constraint is pure extraction mechanism with compounding interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replacement_cost_externality, empirical, 'Cost responsibility allocation across planned vs emergency maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferred_maintenance_cascade, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defmaint_tr_t0, deferred_maintenance_cascade, theater_ratio, 0, 0.42).
narrative_ontology:measurement(defmaint_tr_t3, deferred_maintenance_cascade, theater_ratio, 3, 0.52).
narrative_ontology:measurement(defmaint_tr_t6, deferred_maintenance_cascade, theater_ratio, 6, 0.61).
narrative_ontology:measurement(defmaint_tr_t10, deferred_maintenance_cascade, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(defmaint_be_t0, deferred_maintenance_cascade, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(defmaint_be_t3, deferred_maintenance_cascade, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(defmaint_be_t6, deferred_maintenance_cascade, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(defmaint_be_t10, deferred_maintenance_cascade, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferred_maintenance_cascade, resource_allocation).
narrative_ontology:affects_constraint(deferred_maintenance_cascade, infrastructure_collapse_cascades).
narrative_ontology:affects_constraint(deferred_maintenance_cascade, fiscal_straitjacket_municipal_budgets).
narrative_ontology:affects_constraint(deferred_maintenance_cascade, emergency_response_system_overload).

% DUAL FORMULATION NOTE:
% Deferred maintenance is the upstream constraint enabling downstream cascades: infrastructure collapse (ε=0.72, snare), fiscal straitjacket (ε=0.45, tangled_rope), and emergency response overload (ε=0.55, tangled_rope). The deferral mechanism creates conditions for all three. Decomposition: this story captures the maintenance-deferral choice; downstream stories capture failure consequences. All are linked by the extraction flow: present savings → future costs → crisis response → system overload.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferred_maintenance_cascade, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
