% ============================================================================
% CONSTRAINT STORY: legislative_gridlock_snare
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legislative_gridlock_snare, []).

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
 *   constraint_id: legislative_gridlock_snare
 *   human_readable: Legislative Gridlock as Extractive Constraint
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Legislative gridlock functions as a pure extraction mechanism in which
 *   supermajority requirements, procedural holds, and multiple veto points
 *   prevent legislative action reflecting electoral mandates or addressing
 *   public problems. Unlike coordination mechanisms that require consensus
 *   and distribute benefits broadly, gridlock concentrates blocking power in
 *   minority factions whose veto imposes costs on majorities and the public
 *   without generating compensating collective benefits. The constraint's
 *   severity has increased over the measurement interval (2004-2024) as
 *   political polarization has deepened and supermajority thresholds have
 *   become binding constraints rather than rare gates. The extractiveness
 *   trajectory (0.42 → 0.68) reflects accumulating policy inaction and crisis
 *   deferral; the theater ratio trajectory (0.45 → 0.68) reflects increasing
 *   performativity of legislative debate as substantive passage becomes
 *   unlikely. The constraint exhibits all six DR types depending on
 *   perspective, but the analytical classification at national scale is
 *   unambiguously snare: high extraction from victims (the legislative
 *   majority, the public, policy responsiveness), high suppression (rules
 *   prevent normal legislative function), no compensating coordination
 *   benefit, and existence that depends on suppressing alternatives (the
 *   constraint would collapse immediately if supermajority thresholds were
 *   lowered or procedural holds eliminated).
 *
 * KEY AGENTS:
 *   - Legislative Majority: Primary victim (powerless/trapped) — holds formal authority but cannot translate it into legislation; no exit option except abandoning representation
 *   - Minority Blocking Coalition: Secondary victim (moderate/constrained) — holds veto power but is trapped by inability to govern or advance own agenda; suppression is self-inflicted through use of blocking power
 *   - The Public / Policy Responsiveness: Primary victim (powerless/trapped) — abstract collective unable to organize or exit; bears cost of foregone legislation and deferred crises
 *   - Procedural Rules System: Institutional actor (institutional/arbitrage) — maintains theatrical legitimacy while enabling gridlock; benefits from inertia and the appearance of orderly process
 *   - Local/State Legislatures: Alternative actor (organized/mobile) — show that gridlock is not inherent to representative democracy; procedural flexibility enables coordination and legislative function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees gridlock as contingent institutional architecture, not natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legislative_gridlock_snare, 0.68).
domain_priors:suppression_score(legislative_gridlock_snare, 0.72).
domain_priors:theater_ratio(legislative_gridlock_snare, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legislative_gridlock_snare, extractiveness, 0.68).
narrative_ontology:constraint_metric(legislative_gridlock_snare, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legislative_gridlock_snare, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legislative_gridlock_snare, snare).
narrative_ontology:human_readable(legislative_gridlock_snare, "Legislative Gridlock as Extractive Constraint").
narrative_ontology:topic_domain(legislative_gridlock_snare, "political/institutional").

domain_priors:requires_active_enforcement(legislative_gridlock_snare).

% --- Structural relationships ---
narrative_ontology:constraint_victim(legislative_gridlock_snare, legislative_majorities).
narrative_ontology:constraint_victim(legislative_gridlock_snare, public_good_provision).
narrative_ontology:constraint_victim(legislative_gridlock_snare, policy_responsiveness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRUSTRATED LEGISLATIVE MAJORITY (SNARE) — A coalition commanding a majority of seats cannot pass legislation reflecting their policy mandate due to supermajority requirements, procedural holds, or veto-proof thresholds. The majority is structurally trapped: they hold the formal authority to govern but are blocked by rules that require consensus beyond majority will. Exit options are minimal — leaving the legislature means abandoning representation; changing rules requires the same supermajority that is blocking them. Experiences maximum extraction as their electoral mandate is rendered non-functional.
constraint_indexing:constraint_classification(legislative_gridlock_snare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OBSTINATE MINORITY COALITION (SNARE) — A minority faction holds blocking power through procedural rules (filibuster, supermajority gates, committee gatekeeping). They can prevent legislation they oppose but cannot pass their own agenda. This appears as veto power but functions as a mutual extraction mechanism: both majority and minority are locked in gridlock, unable to advance substantive goals. The minority benefits from the ability to block but is trapped by their inability to govern. High suppression: the minority's own veto power suppresses their capacity to legislate.
constraint_indexing:constraint_classification(legislative_gridlock_snare, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (SNARE) — From a generational perspective, gridlock functions as a pure extraction mechanism. The constraint extracts value from public goods provision: infrastructure, healthcare, education, climate policy, and economic regulation all require legislative action that gridlock prevents. The public bears the cost of inaction (foregone benefits, policy obsolescence, crisis accumulation). Neither majority nor minority benefits in aggregate — both pay the cost of gridlock while gaining only the narrow satisfaction of blocking opponents. The engine classifies this as snare: high extraction from the victims (the public and governance capacity), high suppression (procedural rules prevent normal legislative function), no compensating coordination benefit.
constraint_indexing:constraint_classification(legislative_gridlock_snare, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 4: LOCAL COLLECTIVE ACTION ALTERNATIVE (TANGLED ROPE) — At local/state level with more fluid coalition dynamics, gridlock appears as mixed coordination and extraction. Sub-national legislatures can often pass budgets and legislation through compromise, supermajority coalitions, and procedural flexibility. The coordination function (compromise-driven agenda-setting) coexists with extraction (supermajority requirements extract overrepresentation from minorities). This perspective shows that gridlock is not inherent to representative democracy but rather to specific procedural architectures. Exit is more viable at local level: voters can migrate, politicians can change parties or form new coalitions.
constraint_indexing:constraint_classification(legislative_gridlock_snare, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: PROCEDURAL RITUAL SYSTEM (PITON) — The legislative process maintains theatrical legitimacy through floor debates, committee hearings, and voting rituals even when outcomes are predetermined by gridlock. The theater ratio (0.68) reflects that much legislative activity is performative: debates that change no minds, committee meetings that produce no substantive results, floor votes that are forestalled by procedural blocks. The procedural system persists through institutional inertia — it confers legitimacy (voters see their representatives 'working') despite low functional capacity. The engine classifies this as piton: degraded institution maintained by theatrical performance rather than substantive function.
constraint_indexing:constraint_classification(legislative_gridlock_snare, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW VIEW / FALSE SUMMIT (MOUNTAIN) — The naturalized frame claims gridlock is inherent to democratic checks and balances, an immutable structural feature of separated powers and minority protection. This perspective claims accessibility_collapse (≥0.85) — 'there is no way to escape gridlock within a democratic framework' — and low resistance (≤0.15) to the constraint as a natural law. However, the structural data contradicts this: gridlock is a specific feature of certain procedural architectures (supermajority thresholds, filibuster rules, veto points), not inherent to democracy. Comparative evidence (parliamentary systems, unicameral legislatures, simple-majority rules) shows gridlock is contingent, not immutable. The engine detects this as a false summit: naturalization of institutional choice as natural law.
constraint_indexing:constraint_classification(legislative_gridlock_snare, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legislative_gridlock_snare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legislative_gridlock_snare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legislative_gridlock_snare, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legislative_gridlock_snare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legislative_gridlock_snare, TR),
    TR >= 0.70.

:- end_tests(legislative_gridlock_snare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint prevents legislation that would address public problems (climate change, infrastructure, healthcare, taxation). The extraction is the opportunity cost of blocked action — resources and welfare foregone. The rising trajectory reflects accumulating gridlock: as polarization increases and coalitions become more rigid, even previously-routine legislative functions (budget passage, debt ceiling raises) become paralyzed. Initial extractiveness (0.42) reflects some legislative capacity in earlier period; final extractiveness (0.68) reflects near-total blockade in recent cycles. Suppression (0.72): High. Procedural rules (60-vote filibuster threshold, committee gatekeeping, veto points) explicitly suppress the legislative capacity of majorities. The suppression is structural and intentional — the rules are designed to require supermajority consensus, not simple majority rule. Theater ratio (0.68): High and rising. Much legislative activity is performative: floor debate on bills that will not pass, committee work on legislation that will be filibustered, compromise proposals that are dead-on-arrival. The rising trajectory reflects that as gridlock has deepened, the gap between legislative process and legislative outcome has widened. The procedural system maintains legitimacy (voters see their representatives debating and voting) while actual policy passage becomes increasingly unlikely.
 *
 * PERSPECTIVAL GAP:
 *   The frustrated legislative majority sees the constraint as a snare: they have the votes to pass legislation reflecting their mandate, but procedural rules prevent them from translating authority into action. The minority coalition sees a different snare: they can block legislation they oppose, but the same blocking power traps them in gridlock — they cannot advance their own agenda. The public sees the extraction most clearly: foregone legislation, deferred crises, policy obsolescence, and unresponsive government. The procedural system (through its defenders) sees the constraint as coordination: supermajority requirements ensure broad consensus and protect minorities. The local legislature sees this claim and disproves it: legislatures without supermajority thresholds coordinate effectively and pass legislation. The naturalizing view claims gridlock is inherent to democracy and separated powers, but comparative analysis shows it is contingent on specific procedural choices. The perspectival gap reveals that gridlock is not a coordination problem pretending to be extraction — it is pure extraction masked by coordination rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   The majority has high d (near 1.0) — they are trapped victims of the constraint, experiencing maximum extraction relative to their structural position and formal authority. The minority has elevated d (0.70-0.80) — they are also victims of gridlock despite holding veto power, because veto power that prevents all legislation (including their own) is extraction, not benefit. The public has maximum d (1.0) — they are fully targeted by foregone legislation and bear the cost of inaction with no compensating benefit. The procedural system appears as beneficiary (low d) only in a narrow sense: it preserves institutional legitimacy and continuity. But because the constraint produces no substantive coordination benefit, no actor experiences negative or low d values that would offset the victims' high d. There are no true beneficiaries of gridlock — only agents with different costs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY FULLY RESOLVED: This constraint demonstrates that pure extraction masquerading as coordination is the defining pathology of high-suppression mechanisms. The coordination rhetoric ('supermajority requirements ensure consensus,' 'veto points protect minorities,' 'separation of powers prevents tyranny') accurately describes the theoretical function but obscures the practical outcome: gridlock that blocks beneficial legislation and harms the public. The mandatrophy is resolved by measuring: Does the supermajority requirement produce consensus on substantive legislation, or does it produce veto deadlock? Evidence shows the latter. Comparative analysis (parliamentary systems, unicameral legislatures, simple-majority rules) shows consensus can be achieved without supermajority suppression — consensus reflects culture and coalition dynamics, not procedural thresholds. The mandatrophy resolution: gridlock is snare, not rope. The coordination function (ensuring broad consensus) is never realized; only the extraction function (enabling veto) operates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gridlock_cause_attribution,
    'Is legislative gridlock primarily a consequence of procedural rules (filibuster, supermajority thresholds, veto points) or of deep political polarization that would produce gridlock even under majoritarian procedures?',
    'Comparative analysis across parliamentary systems with majoritarian procedures vs presidential systems with multiple veto points; historical correlation between rule changes and gridlock severity; electoral realignment data correlating polarization with gridlock timing',
    'If procedural: gridlock is contingent and changeable (snare classification stable). If polarization-driven: gridlock reflects underlying conflict and rule change is insufficient (classification depends on whether polarization itself is constrainable). Mixed: both are true, and the snare is nested within a deeper structural conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_cause_attribution, empirical, 'Whether gridlock is caused by procedural rules or underlying polarization').

omega_variable(
    supermajority_coordination_function,
    'Do supermajority requirements serve a coordination function (ensuring broad consensus, protecting minorities) or primarily function as extraction tools that enable minorities to block majorities?',
    'Historical analysis of supermajority outcomes: do they produce more durable/consensual policy or simply enable obstruction? Comparison of legislative satisfaction/performance metrics in supermajority vs simple-majority regimes; analysis of whether supermajority requirements actually protect minority interests or merely entrench minority veto power',
    'If coordination function: classification shifts toward tangled_rope (mixed coordination + extraction). If purely extractive: snare classification is correct. Determines whether rule reform would resolve or merely relocate the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supermajority_coordination_function, empirical, 'Whether supermajority requirements serve coordination or pure veto').

omega_variable(
    procedural_exit_feasibility,
    'Can legislatures reform their procedural rules to reduce gridlock, or do those rules entrench themselves such that reform is procedurally impossible (the very supermajority that blocks legislation prevents rule change)?',
    'Historical precedent: cases where legislative procedures were changed (filibuster reform, committee reform, budget process reform); analysis of whether rule-making procedures themselves have changed over time; counterfactual: what fraction of attempted procedural reforms failed due to gridlock on procedural rules?',
    'If procedural exit is feasible: classification implies agents have agency (exit_options could be upgraded from trapped to constrained). If procedural rules entrench themselves: gridlock is self-reinforcing and the snare classification is reinforced (trapped with no exit even to the rule-making process).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_exit_feasibility, empirical, 'Whether procedural reform is possible or self-entrenchingly blocked').

omega_variable(
    public_extraction_mechanism,
    'Does gridlock directly extract resources from the public (through inaction that harms welfare) or does it harm through opportunity cost (legislation that would have benefited the public is prevented)?',
    'Welfare analysis comparing enacted legislation in high-gridlock vs low-gridlock periods; measurement of foregone benefits from blocked legislation; analysis of crisis accumulation and delayed response as gridlock costs',
    'Direct extraction: victims include taxpayers and beneficiary classes. Opportunity cost: victims are abstract (future benefits foregone). Affects severity assessment and determines whether gridlock is optimizable (reducing costs) vs eliminable (removing the constraint entirely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_extraction_mechanism, empirical, 'Whether gridlock extracts directly or through opportunity cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legislative_gridlock_snare, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legis_tr_t0, legislative_gridlock_snare, theater_ratio, 0, 0.45).
narrative_ontology:measurement(legis_tr_t10, legislative_gridlock_snare, theater_ratio, 10, 0.58).
narrative_ontology:measurement(legis_tr_t20, legislative_gridlock_snare, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(legis_be_t0, legislative_gridlock_snare, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legis_be_t10, legislative_gridlock_snare, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(legis_be_t20, legislative_gridlock_snare, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legislative_gridlock_snare, enforcement_mechanism).
narrative_ontology:affects_constraint(legislative_gridlock_snare, two_party_duopoly_snare).
narrative_ontology:affects_constraint(legislative_gridlock_snare, regulatory_capture_via_gridlock).
narrative_ontology:affects_constraint(legislative_gridlock_snare, crisis_deferred_as_extraction).

% DUAL FORMULATION NOTE:
% Legislative gridlock is downstream of several structural constraints including partisan polarization, primary election dynamics, and campaign finance concentration. Each upstream constraint affects how rigid coalitions become and how binding supermajority thresholds are. Gridlock also feeds forward to regulatory capture: when legislatures cannot update rules, regulators and courts fill the gap, creating a secondary extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legislative_gridlock_snare, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
