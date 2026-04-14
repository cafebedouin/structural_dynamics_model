% ============================================================================
% CONSTRAINT STORY: sotu_1954_eisenhower_european_defense_community
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1954_eisenhower_european_defense_community, []).

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
 *   constraint_id: sotu_1954_eisenhower_european_defense_community
 *   human_readable: European Defense Community (1954): Unified Western European Military Integration
 *   domain: foreign_policy/institutional_governance
 *
 * SUMMARY:
 *   The European Defense Community (EDC), championed by Eisenhower in his
 *   1954 State of the Union address, represents a complex institutional
 *   mechanism for managing German rearmament within a collective Western
 *   European framework while simultaneously reducing U.S. direct military
 *   expenditure. The mechanism aims to enable Western European self-reliance
 *   while preventing a destabilized German unilateral rearmament that could
 *   fragment NATO. The EDC distributes defense burdens across multiple
 *   nations—enabling the U.S. to reduce its troop presence while maintaining
 *   strategic influence through alliance structure. However, the constraint
 *   exhibits radically different classifications from different structural
 *   positions: the U.S. experiences pure coordination (Rope), France
 *   experiences mixed coordination with significant sovereignty loss (Tangled
 *   Rope), smaller states experience complete subordination (Snare), Germany
 *   experiences identity-locked military constraint (Snare with
 *   identity_locked exit), and the institutional structure itself functions
 *   as temporary scaffold for burden-transfer with eventual sunset. The
 *   mechanism's theater ratio increases over time as institutional compliance
 *   becomes performative rather than functional—states go through EDC
 *   procedures even as the underlying threat perception and necessity wane.
 *
 * KEY AGENTS:
 *   - United States (Eisenhower administration): Primary strategic beneficiary (institutional/arbitrage) — designs mechanism to reduce U.S. expenditure while maintaining hegemony; no meaningful exit costs
 *   - France: Organized victim-beneficiary hybrid (organized/constrained) — gains collective security and burden-sharing but surrenders military sovereignty; faces catastrophic exit costs (Soviet encirclement, U.S. abandonment)
 *   - West Germany: Identity-locked victim (moderate/identity_locked) — structurally capable of unilateral rearmament but internally constituted as reformed, non-militarist state; trapped by internalized post-Nazi identity
 *   - Smaller Western European states (Belgium, Netherlands, Luxembourg): Powerless victims (powerless/trapped) — no exit capacity; geopolitical necessity forces subordination to collective structure
 *   - NATO institutional structure: Powerful container (powerful/mobile) — experiences EDC as coordination scaffold with eventual sunset; maintains flexibility to reshape after Cold War threat recedes
 *   - Soviet Union (external threat): Impetus for suppression (analytical/trapped in adversarial position) — creates necessity for Western European collective defense, raising suppression across all perspectives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1954_eisenhower_european_defense_community, 0.52).
domain_priors:suppression_score(sotu_1954_eisenhower_european_defense_community, 0.65).
domain_priors:theater_ratio(sotu_1954_eisenhower_european_defense_community, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1954_eisenhower_european_defense_community, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1954_eisenhower_european_defense_community, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1954_eisenhower_european_defense_community, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sotu_1954_eisenhower_european_defense_community, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sotu_1954_eisenhower_european_defense_community, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1954_eisenhower_european_defense_community, tangled_rope).
narrative_ontology:human_readable(sotu_1954_eisenhower_european_defense_community, "European Defense Community (1954): Unified Western European Military Integration").
narrative_ontology:topic_domain(sotu_1954_eisenhower_european_defense_community, "foreign_policy/institutional_governance").

domain_priors:requires_active_enforcement(sotu_1954_eisenhower_european_defense_community).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1954_eisenhower_european_defense_community, united_states).
narrative_ontology:constraint_beneficiary(sotu_1954_eisenhower_european_defense_community, participating_western_european_states).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_european_defense_community, french_military_sovereignty).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_european_defense_community, german_rearmament_constraints).
narrative_ontology:constraint_victim(sotu_1954_eisenhower_european_defense_community, smaller_nation_defense_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER WESTERN EUROPEAN STATES (SNARE) — Belgium, Netherlands, Luxembourg face no genuine exit from Cold War alignment. Surrendering unilateral defense capacity to collective EDC structure is structurally necessary; refusal means exposure to Soviet pressure without U.S. guarantee. Trapped by geopolitical necessity. Experience extraction: loss of independent military decision-making, permanent subordination to larger powers within integrated structure. No exit capacity; full absorption into mechanism.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FRANCE (TANGLED ROPE) — Organized institutional actor facing genuine coordination benefit (burden-sharing, collective defense against Soviet threat) alongside extraction: permanent loss of independent nuclear/conventional capacity, subordination to larger NATO alliance structure, inability to pursue independent foreign policy. Constrained exit: France could refuse EDC, but costs are catastrophic (Soviet encirclement, abandonment by U.S., economic isolation). Benefits and costs coexist; active enforcement required to maintain French participation against nationalist backlash.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: WEST GERMANY / GERMAN REARMAMENT (SNARE with IDENTITY_LOCKED EXIT) — Germany structurally mobile (has capacity for independent military rebuilding) but identity-locked: internal identity as reformed, non-militarist state constituted through collective security arrangement. Unilateral rearmament would shatter identity frame—'remilitarization' triggers existential anxiety about Nazi past. High suppression from both directions: cannot rearm independently (violates internalized reformed identity) AND cannot exit collective structure (would trigger Western abandonment). The binding mechanism is cognitive/identity-based, not purely structural. Experiences extraction through permanent military subordination, loss of strategic autonomy, dependent security status.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(continental))).

% PERSPECTIVE 4: UNITED STATES (ROPE) — Primary strategic beneficiary experiencing EDC as pure coordination mechanism. Enables German rearmament without destabilizing NATO, reduces U.S. direct military expenditure in Europe, distributes Cold War burden across Western alliance, prevents unilateral German military capability that could threaten alliance unity. No meaningful extraction experienced; high arbitrage capacity (can withdraw support, redirect resources, reshape alliance structure at will). Benefits flow toward U.S. through reduced expenditure and maintained strategic influence.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COLLECTIVE WESTERN EUROPEAN EDC INSTITUTION (TANGLED ROPE at institutional/powerful level) — The EDC structure itself as institution benefits from coordination function (pooled defense, collective bargaining power, reduced vulnerability to Soviet divide-and-conquer) while enabling asymmetric extraction: larger economies (France, Germany) bear disproportionate defense costs and military contributions; institutional overhead concentrates decision-making power. Active enforcement required to prevent free-riding and maintain burden-sharing. Powerful institutional actors experience this as optimization problem; weaker members experience it as imposition.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: NATO AS STRUCTURAL CONTAINER (SCAFFOLD) — EDC functions as temporary scaffold within broader NATO structure, designed to transition from immediate U.S. military presence toward European self-reliance. Sunset logic: EDC is meant to be temporary burden-sharing mechanism until Western Europe reaches military-economic maturity sufficient for independent NATO participation (1970s-onward trajectory). Suppression is tolerably high because both parties see endpoint: European military independence reduces need for integrated command structure. Mobile exit for powerful actors (NATO membership persists even as EDC dissolves).
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: CIVILIZATIONAL ANALYTICAL VIEW (PITON) — From post-Cold War perspective, EDC appears as degraded institutional form: it performed genuine coordination function (containing German rearmament, distributing defense burden) but simultaneously enabled extraction mechanisms (U.S. hegemony in alliance, loss of European military autonomy, permanent subordination of continental powers to U.S. strategic interests). The theatrical component increased over time as Cold War threat perception shifted and European capacity for independent defense matured. The mechanism persisted through institutional inertia and path dependency even as original necessity waned. Theater ratio reflects ceremonial rather than functional operation by 1960s.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: NATURAL LAW VIEW (MOUNTAIN / FALSE SUMMIT CANDIDATE) — From civilizational universality, containment of German military power within collective European structure appears as immutable constraint on European political order: any attempt at unilateral German rearmament destabilizes continental balance, making collective institutional containment a permanent structural necessity. However, this 'natural law' reading is a false summit—it naturalizes the specific institutional arrangements (EDC, NATO hierarchy, U.S. hegemony) as inevitable when alternative orderings exist (bilateral security arrangements, European-only defense coordination, post-alliance structures). The engine's false summit detector will flag beneficiary presence (U.S., participating Western European states) against the mountain classification.
constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1954_eisenhower_european_defense_community_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1954_eisenhower_european_defense_community, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1954_eisenhower_european_defense_community, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1954_eisenhower_european_defense_community, TR),
    TR >= 0.70.

:- end_tests(sotu_1954_eisenhower_european_defense_community_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, trending upward over the interval. Initial value (0.35) reflects genuine coordination need in 1950—Soviet threat is acute, burden-sharing is genuinely mutually beneficial. Rising to 0.52 by 1960 reflects: (1) French political resentment at permanent sovereignty loss, (2) German frustration at security dependence and inability to pursue independent foreign policy, (3) Eisenhower administration's increasing leverage over European strategic choices, (4) institutional overhead that benefits U.S. more than European members. The extraction is not predatory but structural—the mechanism transfers strategic decision-making power toward Washington and away from continental capitals. Suppression (0.65): High and stable. Reflects: (1) Cold War geopolitical necessity—Soviet threat makes exit from any defense alliance suicidal, (2) EDC institutional enforcement mechanisms that penalize defection and free-riding, (3) U.S. veto over European strategic autonomy, (4) locked-in path dependency—once integrated into collective structure, unilateral withdrawal is politically catastrophic. Theater ratio (0.48): Moderate and rising. Initial value (0.25) reflects genuine functional coordination—states are solving real collective action problems. Rising to 0.48 by 1960 reflects increasing ceremonial component: burden-sharing procedures become ritualized, strategic autonomy becomes nominal, actual decisions concentrate in U.S./NATO command structures. The rise suggests that coordination function is gradually replaced by performative institutional maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence: U.S. perceives Rope (coordination), Western Europe perceives Snare/Tangled Rope (extraction with lock-in), Germany perceives identity-locked trap, and the NATO container perceives temporary Scaffold. The gap reflects genuine structural differences: U.S. has arbitrage options (can withdraw, reshape, reallocate resources) while European states have constrained or trapped options (exit is politically/economically suicidal). France's Tangled Rope classification reflects its hybrid position: it gains real security coordination benefits but at cost of permanent military subordination and loss of independent foreign policy. Smaller states' Snare classification reflects absence of exit options and complete institutional subordination. Germany's identity_locked classification reveals that the binding mechanism is not purely structural coercion but internalized cognitive frame—German identity as post-Nazi reformed state prevents even contemplating unilateral rearmament, even though capacity exists. The civilizational analytical view risks false-summarizing this as natural law (inevitable constraint on German power) when it is actually contingent institutional arrangement. The perspectival gap is maximal because power asymmetries, exit capacities, and structural positions diverge most sharply in this mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural relationship to the extraction flow. U.S. as institutional beneficiary with arbitrage exit (can withdraw support, reshape alliance, reallocate resources) derives low d (~0.10)—experiences negative or near-zero extraction. France as organized victim-beneficiary hybrid with constrained exit (could refuse EDC but faces catastrophic costs) derives moderate d (~0.55)—experiences meaningful extraction despite coordination benefits. Smaller states as powerless victims with trapped exit derive high d (~0.90)—experience maximal extraction with no viable alternatives. Germany as moderate victim with identity_locked exit (structurally mobile but identity-constituted as non-militarist) derives d (~0.75)—experiences high extraction because the binding mechanism is cognitive rather than purely structural; cannot exercise structural mobility without identity dissolution. The chi formula χ = ε × f(d) × σ(S) applies scope modifier σ(continental) = 1.1, amplifying effective extraction for continental-scope perspectives. France's d ~0.55 produces f(d) ≈ 0.75; with ε=0.52 and σ=1.1, effective χ ≈ 0.43, placing it in Tangled Rope territory. Smaller states' d ~0.90 produces f(d) ≈ 1.42; with ε=0.52 and σ=1.1, effective χ ≈ 0.81, firmly Snare territory.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is classified as Tangled Rope because it satisfies all three gates: (1) COORDINATION FUNCTION: genuine collective defense problem requiring burden-sharing and mutual commitment—removing the constraint leaves smaller states exposed to Soviet pressure without U.S. guarantee. (2) ASYMMETRIC EXTRACTION: U.S. captures disproportionate strategic influence, Europe surrenders military autonomy, larger powers (France, Germany) bear disproportionate costs. (3) ACTIVE ENFORCEMENT: requires continuous institutional maintenance, pressure on member states to comply with burden-sharing obligations, U.S. veto over European strategic choices. The mandatrophy is resolved by recognizing that all three elements are structurally present—this is not a Rope disguised as extraction, nor is it pure Snare with spurious coordination claims. The constraint genuinely solves a collective action problem (preventing unilateral German rearmament and Soviet opportunism) while simultaneously enabling asymmetric extraction (U.S. hegemony, European subordination). Different perspectives see different prominence of coordination vs extraction based on their structural position: U.S. experiences it as Rope because they gain benefits; Germany experiences it as Snare because identity-lock prevents exit even though structural barriers relax; France experiences it as Tangled Rope because they experience genuine security benefits alongside real sovereignty loss. The mandatrophy confirms that Tangled Rope is the correct classification at the level of institutional structure itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_forced_compliance,
    'To what degree is EDC genuine burden-sharing coordination versus forced compliance with U.S. strategic preferences?',
    'Analysis of participant state incentives independent of U.S. pressure; comparison of EDC terms with voluntary alternative arrangements proposed by European actors; historical counterfactual—would Western Europe have unified defense without U.S. insistence?',
    'High genuine coordination: classification shifts toward Rope across more perspectives. High forced compliance: classification shifts toward Snare, Tangled Rope across European perspectives. This omega determines whether suppression reflects structural necessity or imposed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_forced_compliance, empirical, 'Degree of genuine coordination versus forced compliance in EDC participation').

omega_variable(
    german_identity_lock_mechanism,
    'Is German subordination to collective EDC structure truly identity-locked (internalized reformed-state identity preventing unilateral rearmament) or identity_constrained (external NATO/U.S. pressure with internal identity as secondary factor)?',
    'Analysis of German political discourse: language of ''Western integration'' vs ''security dependence''; counterfactual—what prevents German unilateral rearmament? (NATO treaty obligation, U.S. veto, or internalized post-Nazi identity frame?). Longitudinal: as German identity de-emphasizes Nazi past, does acceptance of military subordination weaken or persist through structural locking?',
    'True identity-lock: German exit from EDC remains unthinkable even when structural barriers relax (explains post-1990 continued NATO integration despite military capability). Structural constraint: exit becomes thinkable as Cold War threat recedes and NATO weakens (explains 2000s-onward German military ambivalence). This determines whether suppression mechanism is internalized or external.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(german_identity_lock_mechanism, empirical, 'Whether German military subordination is identity-locked or structurally constrained').

omega_variable(
    french_sovereignty_asymmetry,
    'Does France experience EDC extraction differently than smaller NATO members due to its great-power status and nuclear capacity trajectory?',
    'Comparative analysis: French political resistance to EDC (manifested in 1954 rejection) versus Dutch/Belgian acceptance despite similar sovereignty loss. Examination of French exit options (Force de Frappe development, NATO withdrawal, independent European defense) relative to smaller states'' options. Does France''s potential for independent nuclear deterrent change the classification from Tangled Rope to something with higher negotiating leverage?',
    'If France genuinely has independent deterrent pathway: classification shifts to Constrained (not Trapped). Smaller states'' classification may shift from Snare to Constrained or Tangled Rope depending on whether French example creates template for European independence. If Force de Frappe is blocked by U.S./NATO, French actual exit capacity reverts to Trapped/Constrained, validating Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(french_sovereignty_asymmetry, empirical, 'French sovereignty and exit asymmetry compared to smaller European states').

omega_variable(
    soviet_threat_as_suppression_mechanism,
    'Is measured suppression (0.65) primarily generated by EDC institutional enforcement or by external Soviet threat that makes exit from any defense alliance suicidal?',
    'Counterfactual analysis: if Soviet Union did not exist, would EDC suppression remain at 0.65 or collapse? Would institutional enforcement mechanisms (command structure, burden-sharing penalties) generate significant suppression independently? Post-Cold War trajectory: does institutional suppression persist after Soviet threat vanishes?',
    'If suppression is primarily Soviet-threat-driven: it is not intrinsic to EDC but to Cold War context. Classification could be Rope (genuine coordination) once threat recedes. If suppression is institutional: Tangled Rope classification holds post-Cold War. This determines whether EDC is a natural response to external threat or an extraction mechanism parasitic on threat perception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_threat_as_suppression_mechanism, empirical, 'Attribution of suppression to EDC mechanism versus external Soviet threat').

omega_variable(
    sunset_mechanism_credibility,
    'Is NATO/EDC as Scaffold a genuine temporary mechanism with credible endpoint, or indefinite extraction mechanism with aspirational sunset language?',
    'Historical analysis: what were the stated timelines for European military independence? Were they met? (1960s maturity claims: did Western Europe achieve independent defense capacity, or did NATO subordination deepen?) What changed 1990-onward when Cold War actually ended—did burden-sharing normalize or did U.S. hegemony intensify? Does current trajectory toward European autonomous defense capability (2020s) vindicate or invalidate the scaffold hypothesis?',
    'Genuine sunset: EDC is Scaffold, high credibility of endpoint, participants experience temporary suppression. Fake sunset: EDC is Tangled Rope (extraction mechanism with ideological cover), suppression persists indefinitely. This determines whether the mechanism''s theater is functional (coordinating temporary burden-shift) or performative (naturalizing permanent asymmetry).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_mechanism_credibility, empirical, 'Credibility of NATO/EDC sunset mechanism and endpoint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1954_eisenhower_european_defense_community, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edc_theater_1950, sotu_1954_eisenhower_european_defense_community, theater_ratio, 0, 0.25).
narrative_ontology:measurement(edc_theater_1954, sotu_1954_eisenhower_european_defense_community, theater_ratio, 4, 0.38).
narrative_ontology:measurement(edc_theater_1960, sotu_1954_eisenhower_european_defense_community, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(edc_extr_1950, sotu_1954_eisenhower_european_defense_community, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(edc_extr_1954, sotu_1954_eisenhower_european_defense_community, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(edc_extr_1960, sotu_1954_eisenhower_european_defense_community, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1954_eisenhower_european_defense_community, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_european_defense_community, nato_alliance_structure).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_european_defense_community, german_rearmament_pathway).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_european_defense_community, french_military_sovereignty).
narrative_ontology:affects_constraint(sotu_1954_eisenhower_european_defense_community, us_cold_war_strategy).

% DUAL FORMULATION NOTE:
% The EDC functions as institutional mechanism subordinate to NATO alliance structure and enabled by broader Cold War strategic logic. The constraint family includes: (1) EDC institutional mechanism (extractiveness 0.52, this file), (2) NATO alliance structure (higher-level container, extractiveness ~0.45), (3) German rearmament pathway (extractiveness ~0.38, constraint on unilateral capacity), (4) U.S. Cold War strategy (extractiveness ~0.42, broader geopolitical positioning). Each story has distinct ε because they measure different structural claims: EDC measures the specific coordination-extraction hybrid of pooled military governance; NATO measures the broader alliance framework; German rearmament measures constraint on German military capacity; U.S. strategy measures hegemonic extraction from global positioning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1954_eisenhower_european_defense_community, organized, 0.55).
constraint_indexing:directionality_override(sotu_1954_eisenhower_european_defense_community, powerless, 0.9).
constraint_indexing:directionality_override(sotu_1954_eisenhower_european_defense_community, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
