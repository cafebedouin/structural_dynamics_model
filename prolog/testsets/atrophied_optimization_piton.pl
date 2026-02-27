% ============================================================================
% CONSTRAINT STORY: atrophied_optimization_piton
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_atrophied_optimization_piton, []).

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
 *   constraint_id: atrophied_optimization_piton
 *   human_readable: The Ghost in the Habit: Atrophied Optimization Piton
 *   domain: technological/social
 *
 * SUMMARY:
 *   The atrophied optimization piton represents a class of technological debt
 *   where an AI or algorithmic optimization system has been deactivated, yet
 *   the human workflows, KPIs, performance metrics, and behavioral nudges it
 *   originally created remain mandatory and unevaluated. The 'ghost'
 *   persists: workers still follow procedures optimized for a feedback loop
 *   that no longer exists; managers still enforce metrics designed by
 *   algorithms that are no longer learning; the organization continues to
 *   justify workflows by reference to 'data-driven optimization' long after
 *   the data pipeline stopped running. This constraint exhibits the piton
 *   signature: high theater ratio (0.81) — the enforcement of procedures
 *   whose original functional purpose has evaporated — combined with low
 *   extractiveness (0.18) because there is no active beneficiary capturing
 *   value from the ghost, only institutional inertia maintaining it. The
 *   constraint creates friction without benefit: workers experience
 *   suppressed autonomy (0.52) through mandatory compliance with pointless
 *   procedures; operational efficiency declines because workflows that once
 *   optimized for machine-learning feedback now serve no coherent objective;
 *   the organization remains locked in a degraded structure. The theater
 *   ratio has risen over the interval (0.52 → 0.81) as the gap widens between
 *   the procedures' original justification and their present lack of function
 *   — the ghost becomes more obviously empty.
 *
 * KEY AGENTS:
 *   - Operational Workers: Primary victims (powerless/trapped) — bear the friction and time cost of mandatory workflows whose optimization function has been deactivated; cannot exit without performance evaluation penalties
 *   - Operations Manager: Secondary victim (moderate/constrained) — aware of the degraded state but constrained by switching costs and institutional relationships; maintains procedures through inertia
 *   - Legacy Systems Owner: Secondary beneficiary (institutional/arbitrage) — continues to extract licensing revenue by maintaining 'optimization platform' agreements; can exit anytime; benefits from client inertia
 *   - Organizational Modernization Initiative: Organized agent (organized/constrained) — tasked with replacing the atrophied system; experiences the constraint as a temporary problem with a defined sunset
 *   - Frontline Support Users: Tertiary victims (powerless/trapped) — external or internal users trapped in mandatory procedures that create friction without optimization benefit
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the ghost as an inevitable law of technological drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(atrophied_optimization_piton, 0.18).
domain_priors:suppression_score(atrophied_optimization_piton, 0.52).
domain_priors:theater_ratio(atrophied_optimization_piton, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(atrophied_optimization_piton, extractiveness, 0.18).
narrative_ontology:constraint_metric(atrophied_optimization_piton, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(atrophied_optimization_piton, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(atrophied_optimization_piton, piton).
narrative_ontology:human_readable(atrophied_optimization_piton, "The Ghost in the Habit: Atrophied Optimization Piton").
narrative_ontology:topic_domain(atrophied_optimization_piton, "technological/social").

% --- Structural relationships ---
narrative_ontology:constraint_victim(atrophied_optimization_piton, operational_workers).
narrative_ontology:constraint_victim(atrophied_optimization_piton, organizational_efficiency).
narrative_ontology:constraint_victim(atrophied_optimization_piton, user_experience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL WORKER (PITON) — Trapped in mandatory workflows originally designed by an active AI optimizer. The system still enforces KPIs, procedures, and daily nudges that made sense when the optimization loop was running and learning, but now persist through institutional inertia. The worker cannot exit these procedures without risking performance evaluation penalties. The constraint is experienced as pointless ritual — the ghost remains, the purpose has evaporated. Low extractiveness because there is no beneficiary capturing value from the now-defunct optimization; high suppression because the workflows are still enforced.
constraint_indexing:constraint_classification(atrophied_optimization_piton, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: OPERATIONS MANAGER (PITON) — Aware that the AI optimization system was deactivated years ago, but the workflows it configured remain embedded in systems, metrics dashboards, and performance contracts. Changing them requires expensive reengineering and potentially disrupts institutional relationships. The manager experiences the constraint as a degraded but entrenched structure: too costly to remove, too pointless to defend as functionally necessary. Theater ratio rises because the manager must justify maintaining procedures whose original logic (machine learning feedback) no longer exists.
constraint_indexing:constraint_classification(atrophied_optimization_piton, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGACY SYSTEMS OWNER (ROPE) — The organization that maintains the AI system (or its documentation and licensing agreements) benefits from continuing to invoice the client organization for 'optimization platform maintenance' even though the core loop has been deactivated. The beneficiary experiences the constraint as a coordination mechanism: the workflows are justified by reference to 'the platform' and 'data-driven optimization,' enabling continued revenue extraction with minimal active work. Arbitrage exit means the owner can walk away or renegotiate terms anytime; the client is more constrained.
constraint_indexing:constraint_classification(atrophied_optimization_piton, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERNIZATION INITIATIVE (SCAFFOLD) — A cross-functional team tasked with 'digital transformation' sees the atrophied optimization system as a temporary problem: legacy KPI structures will be replaced with new objectives as part of a platform migration. The initiative has a sunset clause — the old workflows are expected to be deprecated within 18-24 months as new systems go live. Theater is present (the initiative must justify why the old system persists) but is accepted as transitional. Suppression is moderate because the initiative has resources and organizational backing to define the path forward.
constraint_indexing:constraint_classification(atrophied_optimization_piton, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the persistence of deactivated optimization loops in human workflows might appear as an inevitable law of technological drift: all systems eventually outlive their original purpose, and removing them is always harder than deploying them. The constraint seems inherent to how institutional memory embeds dead systems. However, the structural data contradicts this — the 'ghost' is maintained by specific institutional choices (inertia, risk aversion, revenue incentives), not by physical or logical necessity. This perspective risks naturalizing institutional decay.
constraint_indexing:constraint_classification(atrophied_optimization_piton, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: FRONTLINE SUPPORT USER (SNARE) — End users of the system (internal or external) experience mandatory workflows that no longer optimize for anything — they simply consume time and create friction. The user cannot bypass the procedures; they persist because 'the system requires it.' The user has no recourse to request changes because the AI system has been deactivated and the workflows are now 'legacy.' Pure extraction: time spent, friction borne, no compensating optimization benefit. This perspective reveals the snare structure most clearly — the ghost persists to extract compliance without delivering value.
constraint_indexing:constraint_classification(atrophied_optimization_piton, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(atrophied_optimization_piton_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(atrophied_optimization_piton, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(atrophied_optimization_piton, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(atrophied_optimization_piton, TR),
    TR >= 0.70.

:- end_tests(atrophied_optimization_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint persists not because an active beneficiary is extracting value, but because institutional inertia and switching costs lock the organization into maintaining dead workflows. The legacy systems owner captures some rent through continued licensing, but this is passive extraction — the owner is not actively maintaining the optimization loop, merely continuing to invoice for infrastructure that was never deactivated. Without an active beneficiary, extractiveness remains low. Suppression (0.52): Moderate. Workers and managers are constrained in their ability to modify or abandon the workflows — performance contracts reference the old metrics, system architecture embeds the procedures, risk aversion discourages change. But suppression is not total: the organization could theoretically deactivate the workflows anytime, given sufficient political will and capital. Theater ratio (0.81): High and rising. The key signature of piton decay. Workers enforce procedures whose original function (algorithmic feedback) no longer exists; managers justify them by appeal to 'system requirements' rather than organizational benefit; the organization continues to deploy resources to maintain the ghost. As time passes and awareness grows that the optimization loop is genuinely dead, the theater becomes more visible — the procedures are increasingly recognized as empty ritual. Claimed type: piton. The high theater ratio is the defining feature.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (legacy systems owner) experiences the constraint as coordination (rope) — they justify continued licensing by reference to the 'optimization platform.' The workers experience it as pure degradation (snare or piton, depending on whether they see it as actively extractive or just pointless). The modernization initiative sees it as a temporary problem with a sunset (scaffold) — the old workflows will be replaced. The analytical observer risks seeing it as an inevitable law of technological drift (mountain), but the structural data reveals it as a chosen institutional arrangement (inertia + switching costs + revenue incentives). The perspectival gap reveals the constraint's true nature: not an immutable property of systems, but a contingent maintenance choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation proceeds from structural position. The operational worker is powerless/trapped — the system constrains them and they cannot exit. They have no beneficiary relationship to the constraint; they are pure victim. The legacy systems owner is institutional/arbitrage — they can walk away anytime, and the client is more constrained; they capture residual value through licensing. The modernization initiative is organized/constrained — they have agency and resources, but are constrained by switching costs and organizational politics. The analytical observer is analytical/analytical — they see the full structure and risk naturalizing it. Each perspective's directionality reflects these structural positions, producing different experienced extractiveness (chi) values even though base extractiveness (epsilon) is constant at 0.18.
 *
 * MANDATROPHY ANALYSIS:
 *   PITON-SPECIFIC ANALYSIS: The atrophied optimization piton does not produce mandatrophy because it does not claim to be a coordination mechanism anymore — its beneficiary (the legacy systems owner) has dropped the coordination justification and simply continues to invoice. The organization has implicitly accepted that the workflows are overhead, not optimization. However, a hidden mandatrophy lurks in the modernization initiative's perspective: if the scaffold's sunset claim is false — if the old system will never actually be replaced — then the organization is simply maintaining an extractive snare under the false banner of 'transition.' The modernization initiative's credibility is the resolution point. If the initiative delivers its promised timeline, the piton is temporary and the scaffold classification is correct. If the initiative delays indefinitely, the constraint degrades from piton (dead ritual) to snare (active extraction via false promises of change). The theater ratio will rise further in that scenario, potentially exceeding 0.90, at which point the constraint enters the mandatrophy zone: is it a legitimate temporary structure with sunset, or has the sunset become a performance to justify indefinite extraction?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimization_loop_resurrection_threshold,
    'Is the AI optimization system truly inactive, or is it still collecting data and training offline with a deactivated deployment pipeline?',
    'Audit of system activity logs; interviews with original system developers; inspection of code repositories and training infrastructure for signs of active learning',
    'If truly inactive: clear piton (ghost persists, purpose evaporated). If still learning: constraint may degrade further (snare) or improve (rope) when reactivated; implications for mandatrophy if beneficiaries are waiting for reactivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_loop_resurrection_threshold, empirical, 'Whether the optimization loop is truly deactivated or running offline').

omega_variable(
    workflow_value_residue,
    'What fraction of the remaining KPI-driven workflows still generate measurable business value, independent of AI optimization?',
    'Comparative analysis: measure organizational outcomes for 30 days with workflows active vs 30 days with workflows disabled; track secondary effects (employee morale, error rates, customer satisfaction, throughput)',
    'If > 40% value remains: constraint may be tangled_rope (hybrid coordination + extraction). If < 10% value remains: clear piton with negligible coordination function. Determines whether removal would be costless or require genuine reengineering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(workflow_value_residue, empirical, 'Residual business value of atrophied workflows independent of AI optimization').

omega_variable(
    institutional_switching_cost,
    'What is the true cost (engineering, contract renegotiation, training, risk) of deactivating the legacy workflows versus maintaining them indefinitely?',
    'Detailed cost-benefit analysis; interviews with IT, legal, and operations leadership; identification of hidden switching costs (regulatory compliance tied to audit trails of the old workflows, vendor contract terms, etc.)',
    'If switching cost < 6 months of operational drag: constraint is maintainable and will be removed. If switching cost > 18 months of drag: constraint will persist as piton. Determines whether the scaffold sunset is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_switching_cost, empirical, 'True cost of removing versus maintaining the atrophied optimization workflows').

omega_variable(
    legacy_system_owner_incentive_structure,
    'Is the legacy systems owner deliberately maintaining demand for the deactivated optimization loop to extract continued licensing revenue?',
    'Contract analysis; pricing history; competitive analysis of alternative vendors and migration costs; testimony from procurement and IT leadership about pressures to renew licensing agreements',
    'If deliberate: constraint is extractive snare (coordinated toward rent-seeking). If passive (benign abandonment): constraint is piton (dead inertia). If mixed: constraint becomes tangled_rope (hybrid extraction + unavoidable coordination overhead). Determines whether the beneficiary''s ''rope'' perspective is accurate or covers genuine predatory behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legacy_system_owner_incentive_structure, empirical, 'Whether legacy system owner deliberately maintains deactivated loop for revenue').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(atrophied_optimization_piton, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atrophy_tr_t0, atrophied_optimization_piton, theater_ratio, 0, 0.52).
narrative_ontology:measurement(atrophy_tr_t3, atrophied_optimization_piton, theater_ratio, 3, 0.68).
narrative_ontology:measurement(atrophy_tr_t6, atrophied_optimization_piton, theater_ratio, 6, 0.81).

% Extraction over time
narrative_ontology:measurement(atrophy_be_t0, atrophied_optimization_piton, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(atrophy_be_t3, atrophied_optimization_piton, base_extractiveness, 3, 0.17).
narrative_ontology:measurement(atrophy_be_t6, atrophied_optimization_piton, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(atrophied_optimization_piton, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is not part of a decomposed family. It stands alone as a single structural phenomenon: the ghost of a deactivated optimization loop persisting in human workflows through institutional inertia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
