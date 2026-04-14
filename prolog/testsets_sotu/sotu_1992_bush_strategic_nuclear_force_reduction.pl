% ============================================================================
% CONSTRAINT STORY: sotu_1992_bush_strategic_nuclear_force_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1992_bush_strategic_nuclear_force_reduction, []).

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
 *   constraint_id: sotu_1992_bush_strategic_nuclear_force_reduction
 *   human_readable: Unilateral U.S. Strategic Nuclear Force Reduction (1992)
 *   domain: military/geopolitical/budgetary
 *
 * SUMMARY:
 *   In February 1992, President George H. W. Bush announced a unilateral
 *   reduction in U.S. strategic nuclear forces: immediate halt of B-2 bomber
 *   production after 20 units (vs. planned 130+), termination of the Small
 *   ICBM (Midgetman) program, cessation of new warhead production for
 *   sea-launched cruise missiles, and halting of Peacekeeper ICBM
 *   procurement. This constraint represents a deliberate shift in nuclear
 *   strategy from Cold War forward-deployed readiness to reduced-stockpile
 *   stability, imposed through executive action and codified in the
 *   subsequent START II treaty framework negotiated at Camp David. The
 *   reduction creates a structural tension between fiscal benefit (savings of
 *   $50-80 billion over a decade) and employment/industrial capacity loss in
 *   defense manufacturing regions. It simultaneously solves a genuine
 *   coordination problem (mutual vulnerability reduction) and imposes
 *   asymmetric costs on workers and contractors whose economic specialization
 *   depended on Cold War production levels.
 *
 * KEY AGENTS:
 *   - U.S. Fiscal Budget / Taxpayers: Primary beneficiary (institutional/arbitrage) — saves $50-80 billion over 10 years; benefits from reduced existential risk and freed resources for domestic priorities
 *   - Aerospace Manufacturing Workers: Primary victim (powerless/trapped) — geographic and skill-locked employment in B-2 production (Palmdale, CA), Small ICBM (Malmstrom AFB region), Peacekeeper (Warren AFB region); no local retraining or alternative employment
 *   - Defense Contractors (Northrop Grumman, General Dynamics, Lockheed Martin): Secondary victim (powerful/constrained) — lose production volume and future contracts; retain institutional relationships and access to remaining procurement
 *   - Russian Federation: Secondary beneficiary (organized/mobile) — negotiating partner for Camp David reciprocal reductions; retains exit mobility but experiences constrained strategic choice
 *   - Cold War Alert Posture System: Tertiary victim (institutional/arbitrage) — 24/7 launch-on-warning readiness, continuous bomber patrol, SSBN deployment persist through institutional inertia despite evaporating strategic rationale
 *   - Existential Risk Reduction Advocates: Secondary beneficiary (institutional/arbitrage) — benefit from reduced warhead stockpile and mutual vulnerability decrease
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as immutable strategic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1992_bush_strategic_nuclear_force_reduction, 0.52).
domain_priors:suppression_score(sotu_1992_bush_strategic_nuclear_force_reduction, 0.48).
domain_priors:theater_ratio(sotu_1992_bush_strategic_nuclear_force_reduction, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1992_bush_strategic_nuclear_force_reduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1992_bush_strategic_nuclear_force_reduction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1992_bush_strategic_nuclear_force_reduction, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1992_bush_strategic_nuclear_force_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1992_bush_strategic_nuclear_force_reduction, "Unilateral U.S. Strategic Nuclear Force Reduction (1992)").
narrative_ontology:topic_domain(sotu_1992_bush_strategic_nuclear_force_reduction, "military/geopolitical/budgetary").

domain_priors:requires_active_enforcement(sotu_1992_bush_strategic_nuclear_force_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1992_bush_strategic_nuclear_force_reduction, fiscal_budget).
narrative_ontology:constraint_beneficiary(sotu_1992_bush_strategic_nuclear_force_reduction, existential_risk_reduction_advocates).
narrative_ontology:constraint_beneficiary(sotu_1992_bush_strategic_nuclear_force_reduction, u_s_taxpayers).
narrative_ontology:constraint_victim(sotu_1992_bush_strategic_nuclear_force_reduction, defense_contractors).
narrative_ontology:constraint_victim(sotu_1992_bush_strategic_nuclear_force_reduction, military_industrial_employment).
narrative_ontology:constraint_victim(sotu_1992_bush_strategic_nuclear_force_reduction, cold_war_deterrence_maintenance_posture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AEROSPACE WORKERS (SNARE) — Trapped by geographic specialization and skill-locked employment. B-2 production halt in Palmdale, Small ICBM cancellation (Minuteman basing), Peacekeeper termination, and cruise missile procurement cuts eliminate high-wage manufacturing jobs with no alternative local employment. Workers cannot exit geographically without demolishing family stability. Suppression is structural: specialized skills are not transferable to civilian sector; retraining programs are underfunded; unemployment in defense manufacturing regions persists. Maximum extraction experienced.
constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEFENSE CONTRACTORS (TANGLED ROPE) — Constrained exit: they benefited from Cold War procurement surge and coordinate with DoD on program transition. The reduction imposes real costs (production line shutdown, sunk R&D, reduced future contracts) but contractors retain institutional relationships and access to remaining procurement (strategic air command modernization, other platforms). They experience the constraint as both coordination (managed decline through Camp David reciprocal framework) and asymmetric extraction (their strategic choice architecture is narrowed; they lose margin on cancelled programs). Moderate-high extraction but with agency and benefit from coordination mechanism.
constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. FISCAL BUDGET / TAXPAYERS (ROPE) — Primary beneficiary. Strategic force reduction saves approximately $50-80 billion over 10 years (B-2 production cap at 20 units vs. planned 130+, Small ICBM termination, warhead production freeze, cruise missile halt). The constraint solves a genuine fiscal coordination problem: Cold War overproduction created unsustainable nuclear arsenal maintenance costs. Taxpayers experience the constraint as pure coordination — reducing existential risk while freeing resources for domestic priorities. No extraction experienced; significant benefit. Beneficiary with strong arbitrage options (could have continued spending but chose reduction).
constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXISTENTIAL RISK REDUCTION ADVOCATES (ROPE) — Strategic force reduction aligns with long-term risk mitigation: smaller arsenals reduce accidental launch probability, command-and-control failure risk, and proliferation pressure. The constraint coordinates a genuine existential coordination problem (mutual vulnerability and first-strike temptation). From a civilizational perspective, reducing deployed warheads from ~10,000 to target of ~3,500 (by post-agreement trajectory) solves a structural coordination failure. No extraction experienced; pure coordination benefit.
constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RUSSIAN FEDERATION / SOVIET SUCCESSOR STATE (TANGLED ROPE) — Unilateral U.S. reduction creates reciprocal negotiating pressure at Camp David, but Moscow retains exit mobility: they can refuse reciprocation and maintain Cold War arsenal or pursue alternative alliances. The U.S. reduction is both coordination (establishes framework for bilateral Arms Reduction Treaty / START II) and extraction (shifts relative conventional force balance; Russia loses deterrent parity that nuclear arsenal provided during conventional military transition). Russia experiences constrained choice but not trapped exit. The Camp David framework imposes active enforcement (treaty compliance monitoring) but provides genuine coordination benefits (reducing mutual existential risk, freeing resources for both sides). Moderate extraction with significant coordination function.
constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR ALERT READINESS POSTURE (PITON) — The constraint begins degrading the 24/7 alert readiness system (ICBM launch-on-warning, continuous bomber patrol, underwater ballistic missile deployment). Theater ratio is high: alert operations persisted through bureaucratic inertia even as strategic rationale (imminent Soviet first-strike threat) evaporated post-1989. The reduction caps warheads and halts production but does not immediately eliminate alert posture itself — it persists through institutional momentum. Theater increases as maintenance costs mount relative to strategic justification. The piton classification captures the degradation: the old deterrence system continues performing but its function has atrophied relative to the new negotiated framework.
constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, mutual nuclear deterrence appears as an immutable constraint of bipolar geopolitics: any nuclear-armed peer must maintain credible second-strike capability, which requires deployed forces and alert posture. The reduction looks like a temporary adjustment within structural limits. However, the base properties contradict this: the reduction is actively managed through bilateral negotiation, enforced through treaty compliance monitoring, and constrained by domestic political/budgetary choice — not by physics or logic. The mountain classification is a false summit, naturalizing what is a contingent institutional arrangement.
constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1992_bush_strategic_nuclear_force_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1992_bush_strategic_nuclear_force_reduction, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1992_bush_strategic_nuclear_force_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1992_bush_strategic_nuclear_force_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1992_bush_strategic_nuclear_force_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over the interval. At announcement (t=0), the reduction appears primarily as fiscal coordination (low extractiveness ≈0.35). Over the decade (t=0-10), the costs to aerospace workers and contractors materialize: production line shutdowns, job losses, regional dislocation. The rising extractiveness reflects that the coordination benefit (fiscal savings, existential risk reduction) concentrates on large diffuse beneficiaries (taxpayers, future generations) while extraction concentrates on specific identifiable victims (workers, contractors, military communities). The divergence between diffuse benefit and concentrated harm is the source of rising perceived asymmetry. Suppression (0.48): Moderate. Workers face genuine barriers — geographic relocation costs, skill non-transferability, regional unemployment — but suppression is not total. Some retraining occurs, some workers transition to other sectors, some regions diversify. The Camp David framework provides exit options for contractors (transition to other defense platforms, commercial aerospace) but at significant cost. Theater ratio (0.35, rising to 0.40): Low-moderate. The initial reduction is substantive and tracked (warhead counts are verified, production lines shut down), so theater is low. Theater rises modestly as the alert posture persists unchanged — 24/7 readiness persists through bureaucratic inertia even as strategic justification evaporates. The degradation creates performative maintenance costs (running alert operations for a threat that no longer exists), driving theater upward.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Taxpayers see pure coordination (fiscal savings + existential risk reduction). Aerospace workers see pure extraction (job loss + permanent regional dislocation). Contractors see mixed coordination-extraction (managed decline with institutional relationships preserved, but margin compression and future contract loss). Russia sees constrained negotiation (forced to match U.S. reduction or fall behind, but retaining exit options). The Cold War alert system sees inertial persistence (function atrophied but ritual maintained). The analytical observer risks false-summit naturalization (treating mutual deterrence as immutable constraint rather than contingent institutional choice). The gap between 'taxpayer-beneficiary' and 'aerospace worker-victim' perspectives on the same constraint reveals the mechanism: the constraint redistributes resources from cold-war-dependent sectors to general taxpayers, while creating concentrated costs in affected regions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives. Taxpayers (institutional/arbitrage) are pure beneficiaries (d≈0.05): they bear no specific cost and capture fiscal benefits. Workers (powerless/trapped) are pure targets (d≈0.95): they bear concentrated, unavoidable costs with no exit. Contractors (powerful/constrained) are moderate targets (d≈0.65): they bear real margin losses but retain institutional relationships and exit options (transition to other platforms). Russia (organized/mobile) experiences constrained but not trapping directionality (d≈0.55): they must match the U.S. reduction to avoid conventional military disadvantage, but retain exit mobility (refusing reciprocation, alternative alliances). The piton perspective (alert system) experiences institutional inertia rather than directionality: the system continues operating (d undefined) because it has no agency to exit. The analytical mountain perspective risks high d (viewing the constraint as imposing existential rules on all agents), but the false-summit detector reveals the 'd' as naturalization of a distributive choice.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids the mandatrophy (forced choice between 'is this Rope or is this Snare?') by recognizing that the answer is: both, from different positions. The constraint is genuinely Rope (solves mutual vulnerability coordination problem, saves fiscal resources) AND genuinely Snare (concentrates employment costs in specific regions, imposes permanent workforce transition). The mandatrophy resolves not by choosing one type, but by accepting that Tangled Rope is the structural truth: the constraint has a genuine coordination function (bilateral arms reduction framework) coupled with asymmetric extraction (worker/contractor losses funded by taxpayer gains). The false summit (mountain perspective) is the risk: naturalizing 'mutual deterrence requires sustained cold-war-scale forces' as an immutable law. The Camp David framework proves this is false — the coordination problem (mutual existential vulnerability) can be solved with vastly smaller arsenals. The mountain naturalization enables extraction to hide behind 'strategic necessity.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocal_enforcement_credibility,
    'Will unilateral U.S. reduction induce equivalent Russian reciprocation, or does it signal weakness and invite Russian non-compliance?',
    'Historical tracking: compare Russian warhead reductions to U.S. reductions under START II and successor treaties (2010-2026); assess whether Russian pause/expansion periods correlate with U.S. slowdowns or advances',
    'If reciprocation holds: constraint is genuine coordination (Rope dominates). If Russia freeloads or diverges: constraint becomes asymmetric extraction on U.S. side (Snare dominates for U.S. strategic position). Classification shifts to Snare from U.S. perspective if non-compliance confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocal_enforcement_credibility, empirical, 'Whether Russian reciprocation follows U.S. unilateral reduction').

omega_variable(
    worker_transition_success_rate,
    'Do retraining and economic transition programs successfully reemploy displaced defense manufacturing workers, or is the cost borne permanently by affected regions?',
    'Longitudinal employment tracking: unemployment rates in B-2 production regions (Palmdale, CA; St. Louis, MO; other contractor hubs) vs. national average 1992-2005; wage replacement rates for displaced workers; community economic indicators post-transition',
    'If successful transition (>70% reemployment at comparable wages): snare extraction is temporary, constraint reclassifies as Scaffold with sunset. If permanent dislocation (<40% reemployment, sustained wage loss): snare extraction is severe and persistent, constraint reclassifies as Snare from affected regions'' perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_transition_success_rate, empirical, 'Success rate of worker retraining and regional economic transition').

omega_variable(
    deterrence_stability_threshold,
    'Is there a minimum deployed warhead level below which second-strike credibility erodes and first-strike temptation re-emerges for either side?',
    'Strategic theory assessment (Schelling, Sagan, Waltz): modeling of command-and-control failure probability vs. force size; assessment of whether 3,500-warhead target maintains credible retaliation vs. new threat environments (Chinese arsenal growth, asymmetric conflict escalation, emerging peer competitors)',
    'If threshold is well above 3,500 (e.g., 5,000+): current reduction increases existential risk, constraint becomes extraction on humanity''s existential security (mountain). If threshold is below 3,500 or non-existent: reduction improves risk posture, constraint is pure coordination benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_threshold, conceptual, 'Minimum warhead level for maintaining deterrence stability').

omega_variable(
    domestic_political_reversibility,
    'Can future U.S. administrations unilaterally reverse these reductions (restart B-2 production, resume warhead fabrication, abandon treaty caps), or have institutional commitments made reversal politically costly?',
    'Legislative and treaty analysis: assess whether reductions are locked into binding treaties vs. executive orders; track domestic political debate around expansion attempts 1992-2026; measure congressional appetite for production restart',
    'If easily reversible: constraint is theatrical compliance rather than binding commitment (theater_ratio rises). If locked into long-term institutional commitments: constraint represents genuine strategic choice with high reversal cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_political_reversibility, empirical, 'Political and institutional reversibility of strategic force reductions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1992_bush_strategic_nuclear_force_reduction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_nucred_theater_t0, sotu_1992_bush_strategic_nuclear_force_reduction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sotu_nucred_theater_t5, sotu_1992_bush_strategic_nuclear_force_reduction, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sotu_nucred_theater_t10, sotu_1992_bush_strategic_nuclear_force_reduction, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(sotu_nucred_extract_t0, sotu_1992_bush_strategic_nuclear_force_reduction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_nucred_extract_t5, sotu_1992_bush_strategic_nuclear_force_reduction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sotu_nucred_extract_t10, sotu_1992_bush_strategic_nuclear_force_reduction, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1992_bush_strategic_nuclear_force_reduction, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1992_bush_strategic_nuclear_force_reduction, post_cold_war_conventional_force_balance).
narrative_ontology:affects_constraint(sotu_1992_bush_strategic_nuclear_force_reduction, russian_strategic_modernization_response).
narrative_ontology:affects_constraint(sotu_1992_bush_strategic_nuclear_force_reduction, u_s_defense_industrial_base_transition).

% DUAL FORMULATION NOTE:
% The strategic force reduction is a unilateral policy choice that establishes a negotiating framework (Camp David reciprocal reduction agreement). It could be decomposed into a fiscal coordination story (ε≈0.20, Rope) and a worker dislocation story (ε≈0.70, Snare), but the constraint's structural unity is the decision logic linking production halt to budget savings and strategic negotiation. The linked constraints in network.affects_constraints model the downstream effects: Russian response to U.S. reduction, conventional force balance shift, and industrial base transition dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1992_bush_strategic_nuclear_force_reduction, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
