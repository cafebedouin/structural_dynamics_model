% ============================================================================
% CONSTRAINT STORY: structural_extraction_without_actor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_extraction_without_actor, []).

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
 *   constraint_id: structural_extraction_without_actor
 *   human_readable: The Inertial Rent Siphon
 *   domain: economic/social
 *
 * SUMMARY:
 *   The inertial rent siphon is a structural constraint where extraction
 *   continues long after the institutional justification has eroded. A legacy
 *   fee structure, bureaucratic requirement, or service charge persists
 *   because no single actor has sufficient incentive or authority to
 *   dismantle it, even though all stakeholders would benefit from its
 *   removal. The original service provider (a specific institution,
 *   technology, or coordination function) has either automated away, been
 *   replaced, consolidated into a larger system, or simply become obsolete —
 *   yet the extraction mechanism survives through institutional inertia.
 *   Classic examples include: obsolete licensing fees for services no longer
 *   provided, legacy telecom surcharges that funded universal service
 *   infrastructure now funded separately, permit requirements for activities
 *   that no longer need regulation, settlement fees that persist after the
 *   original settlement infrastructure has been replaced, and intermediary
 *   commissions that survive the original information asymmetry they
 *   addressed. The constraint demonstrates how extraction can decouple
 *   completely from coordination benefit and persist in a degraded
 *   institutional form (piton) rather than disappearing or evolving into a
 *   genuine coordination mechanism (rope).
 *
 * KEY AGENTS:
 *   - Trapped Payers: Ordinary economic actors (consumers, businesses, regulated entities) subject to mandatory extraction with no exit — powerless/trapped
 *   - Institutional Apparatus: Bureaucratic structure that collects and distributes the rent; sees the extraction as performative and inertial but is constrained from dismantling it by path-dependence — institutional/constrained
 *   - Alternative Institutional Coalition: Competing institutions (foreign providers, neighboring jurisdictions, alternative service providers) that offer equivalent function without the inertial rent; organized/mobile
 *   - Analytical Observer: Systems view that recognizes the extraction has decoupled from any genuine service coordination — analytical/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_extraction_without_actor, 0.58).
domain_priors:suppression_score(structural_extraction_without_actor, 0.68).
domain_priors:theater_ratio(structural_extraction_without_actor, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_extraction_without_actor, extractiveness, 0.58).
narrative_ontology:constraint_metric(structural_extraction_without_actor, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(structural_extraction_without_actor, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_extraction_without_actor, piton).
narrative_ontology:human_readable(structural_extraction_without_actor, "The Inertial Rent Siphon").
narrative_ontology:topic_domain(structural_extraction_without_actor, "economic/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_extraction_without_actor, institutional_apparatus).
narrative_ontology:constraint_victim(structural_extraction_without_actor, subjected_populations).
narrative_ontology:constraint_victim(structural_extraction_without_actor, economic_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED PAYER (SNARE) — Ordinary economic actors (consumers, small businesses, regulated entities) face mandatory extraction through fees, levies, or bureaucratic requirements with no meaningful exit. The originating justification has vanished, but the extraction persists through inertia. Suppression is high: regulatory penalty, social pressure, and institutional momentum make non-compliance costly. Effective extraction (chi) is high — these agents have no arbitrage, no mobility, no alternative institutional route.
constraint_indexing:constraint_classification(structural_extraction_without_actor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADMINISTRATIVE APPARATUS (PITON) — The bureaucratic structure that collects the rent sees the extraction as substantially performative. The original function has atrophied (the service provider automated, consolidated, or merged away), but the collection mechanism persists through institutional inertia. Theater ratio is high: the apparatus maintains the extraction ritual (collection, accounting, periodic justification) despite minimal functional output. Coordination benefit has eroded; only extraction remains. The apparatus is constrained, not trapped — it could dismantle the extraction, but institutional path-dependence and political economy keep it in place.
constraint_indexing:constraint_classification(structural_extraction_without_actor, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALTERNATIVE INSTITUTIONAL COALITION (TANGLED ROPE) — Competing institutions, regulatory regimes, or jurisdictions (other countries, neighboring states, alternative service providers) offer functionally equivalent services without the inertial rent. They benefit from capture of clients fleeing the siphon (coordination gain) while also experiencing extraction from the incumbent regime's regulatory resistance. The extraction is real but asymmetric — the coalition has mobility and can arbitrage between jurisdictions.
constraint_indexing:constraint_classification(structural_extraction_without_actor, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a systems view, the inertial rent siphon is a coordination problem solved by institutional path-dependence. The extraction itself is decoupling from any genuine service coordination; what remains is a pure rent-extraction mechanism maintained by institutional inertia. The observer sees how the institutional apparatus and trapped payers are locked in mutual constraint — neither can easily exit without transaction costs. The constraint persists because the cost of dismantling it exceeds the perceived benefit for any single actor.
constraint_indexing:constraint_classification(structural_extraction_without_actor, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_extraction_without_actor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_extraction_without_actor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_extraction_without_actor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_extraction_without_actor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_extraction_without_actor, TR),
    TR >= 0.70.

:- end_tests(structural_extraction_without_actor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The extraction persists at scale despite the originating justification having eroded. However, it is not maximal (≥0.70) because the mechanism lacks active enforcement — no single institutional actor actively opposes exit or escalates consequences beyond the standard regulatory apparatus. The trapped payers could, in principle, lobby for reform, and the institutional apparatus could unilaterally dismantle the extraction without legal constraint. The fact that neither happens reflects coordination failure and path-dependence, not structural impossibility. Suppression (0.68): High. Multiple barriers prevent exit: regulatory penalties, social/professional pressure to comply, transaction costs of switching institutions, and the diffuse nature of the collective burden (no single actor sees sufficient benefit from exit to bear the transaction costs alone). Suppression is not maximum (≥0.75) because alternatives exist — they are just not salient to trapped actors due to information asymmetry or institutional entrenchment. Theater ratio (0.81): High. The institutional apparatus maintains substantial performative activity: annual audits justifying the fee's necessity, periodic official reviews concluding it should continue, staff positions dedicated to rent collection and distribution, and formal ceremonies of compliance. The actual service delivery that originally justified these performances has largely vanished. The high theater ratio (increasing from 0.62 to 0.81 over the interval) reflects the apparatus's adaptation strategy: as functional output declines, performative activity increases to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival disagreement. The trapped payer sees pure extraction (Snare) — they bear full cost with no benefit and no exit. The institutional apparatus sees a degraded ritual (Piton) — the extraction persists through inertia despite functional atrophy. The alternative coalition sees hybrid coordination-extraction (Tangled Rope) — they benefit from client capture (coordination) while being harmed by the incumbent's regulatory resistance (extraction). The analytical observer sees coordination failure (Rope) — the institutional structure and trapped payers are locked in mutual constraint, neither able to exit without bearing transaction costs that exceed their individual benefits. The gap reflects the degree to which the constraint has become purely inertial: different structural positions generate radically different readings of whether this is extraction, coordination, degradation, or system lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is determined by their structural position relative to the extraction flow. Trapped payers have high d (~0.92): they are victims with no arbitrage options and no exit. The institutional apparatus has moderate d (~0.35): it is nominally a beneficiary (receives the rent), but its structural position is constrained — it could exit by dismantling the apparatus, but path-dependence and staff interests create a semi-victim status. The alternative coalition has low d (~0.15): they are genuine beneficiaries (capture clients fleeing the incumbent), and they have mobile options. The analytical observer has high d (~0.75) in the analysis perspective but moderate d (~0.50) in terms of personal stake — they see the structure clearly but have no direct interest in either side. The engine derives these d values from the beneficiary/victim declarations and exit options; the high theater ratio and generational time horizon indicate the constraint is inertial rather than actively enforced.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originating_function_recovery,
    'Could the original service that justified the extraction be meaningfully restored, converting this from piton back to rope or tangled_rope?',
    'Historical analysis of the original service''s function; cost-benefit analysis of full restoration vs incremental modernization; stakeholder preference surveys',
    'If restorable at reasonable cost: the extraction could be legitimized as true coordination benefit, changing classification to Rope. If unrestorable: confirms piton diagnosis — extraction is purely inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originating_function_recovery, empirical, 'Whether the original service function can be recovered').

omega_variable(
    critical_mass_exit_threshold,
    'At what percentage of payers switching to alternative institutions would the institutional apparatus dismantle the inertial rent?',
    'Observation of past reform episodes; institutional cost models; breakeven analysis for the collection apparatus',
    'If threshold is low (<10%): suppression is weaker than measured; trapped exit options may upgrade to constrained. If threshold is high (>40%): suppression confirms full trapping; institutional lock-in is stronger than market pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_exit_threshold, empirical, 'Exit threshold at which apparatus dismantles rent mechanism').

omega_variable(
    theater_maintenance_cost,
    'What fraction of the collected rent goes to maintaining the performative apparatus (justifications, audits, staff, ritual compliance) vs. actual residual benefit distribution?',
    'Administrative budget analysis; tracing of collected rents to final destinations; comparison of stated vs. actual service delivery',
    'If >80% theater cost: piton diagnosis is confirmed at maximum severity. If 40-60% theater: suggests hybrid rope-piton rather than pure piton — some residual coordination function persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_maintenance_cost, empirical, 'Fraction of rent dedicated to apparatus maintenance vs. service delivery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_extraction_without_actor, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inertial_tr_t0, structural_extraction_without_actor, theater_ratio, 0, 0.62).
narrative_ontology:measurement(inertial_tr_t15, structural_extraction_without_actor, theater_ratio, 15, 0.75).
narrative_ontology:measurement(inertial_tr_t30, structural_extraction_without_actor, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(inertial_be_t0, structural_extraction_without_actor, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inertial_be_t15, structural_extraction_without_actor, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(inertial_be_t30, structural_extraction_without_actor, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_extraction_without_actor, resource_allocation).
narrative_ontology:affects_constraint(structural_extraction_without_actor, regulatory_capture_cycles).
narrative_ontology:affects_constraint(structural_extraction_without_actor, institutional_path_dependence).
narrative_ontology:affects_constraint(structural_extraction_without_actor, jurisdictional_arbitrage).

% DUAL FORMULATION NOTE:
% The inertial rent siphon is downstream of regulatory capture (the original beneficiary captured the apparatus to extract rents) but structurally distinct. Regulatory capture is about active extraction by an organized actor; the inertial siphon is about extraction persisting after that actor has vanished or degraded. Link to institutional_path_dependence to model how the extraction persists despite lack of active enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_extraction_without_actor, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
