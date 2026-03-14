% ============================================================================
% CONSTRAINT STORY: french_electoral_legitimacy_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_electoral_legitimacy_crisis, []).

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
 *   constraint_id: french_electoral_legitimacy_crisis
 *   human_readable: French Electoral Legitimacy Crisis
 *   domain: political/institutional
 *
 * SUMMARY:
 *   The French electoral system exhibits a structural tension between its
 *   legitimating narrative (universal suffrage, democratic will, popular
 *   representation) and its institutional logic (coalition gatekeeping via
 *   cordon sanitaire, two-round ballot structure concentrating power in
 *   centrist coalitions, regional pork-barrel coordination). Since the
 *   2016-2017 electoral cycle, this tension has become acute: far-right
 *   (Rassemblement National) and far-left (La France Insoumise / Nupes)
 *   parties achieved 30%+ combined vote share, yet remained excluded from
 *   coalition participation through explicit cordon sanitaire rules. This
 *   constraint exemplifies Tangled Rope classification — it contains genuine
 *   coordination functions (multi-round ballots enabling local
 *   accountability, coalition discipline producing stable governance, EU
 *   policy alignment) paired with asymmetric extraction (electoral power →
 *   legislative representation mapping is non-linear, disenfranchised voters
 *   bear suppression costs, challenger movements face locked institutional
 *   exit paths). The extractiveness has risen from 0.35 (2010) to 0.58 (2025)
 *   as vote fragmentation increased while coalition logic remained constant,
 *   generating widening gap between electoral input and parliamentary output.
 *   Theater ratio has risen (0.52→0.68) as public discourse on 'Republican
 *   values' and 'universal suffrage' has become increasingly disconnected
 *   from actual institutional function — the legitimating myth requires
 *   explicit performance while the structural reality contradicts it.
 *
 * KEY AGENTS:
 *   - Disenfranchised Voters: Primary victim (powerless/trapped) — structurally locked into a system where voting preference does not map to coalition representation. Suppression operates through multi-round ballot mechanics and cordon sanitaire norms.
 *   - Centrist Political Establishment (LREM, Socialists, Republicans): Primary beneficiary (institutional/arbitrage) — experiences the constraint as coordination mechanism; derives arbitrage value through government formation reliability and coalition partner negotiating power.
 *   - Far-Right/Far-Left Challenger Movements: Secondary victim (powerful/mobile) — high vote share but constrained by coalition gatekeeping; theoretically mobile (could exit through constitutional reform) but path is locked by the constraint itself.
 *   - European Union Institutional Framework: Secondary beneficiary (institutional/arbitrage) — supranational level that derives policy alignment benefits (fiscal discipline, regulatory compliance) from French electoral structure locking in centrist governments.
 *   - Regional Constituencies/Departmental Politics: Moderate agent (moderate/constrained) — benefits from local voice and pork-barrel allocation while enforcing centrist constraint at national level.
 *   - Public Discourse on Republican Values: Institutional actor (organized/constrained) — maintains performative framing (universal suffrage, democratic will) despite contradicting institutional reality; theater persists through mandatory invocation of legitimating myth.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing gatekeeping as immutable feature of liberal democracy itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_electoral_legitimacy_crisis, 0.58).
domain_priors:suppression_score(french_electoral_legitimacy_crisis, 0.62).
domain_priors:theater_ratio(french_electoral_legitimacy_crisis, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_electoral_legitimacy_crisis, extractiveness, 0.58).
narrative_ontology:constraint_metric(french_electoral_legitimacy_crisis, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(french_electoral_legitimacy_crisis, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_electoral_legitimacy_crisis, tangled_rope).
narrative_ontology:human_readable(french_electoral_legitimacy_crisis, "French Electoral Legitimacy Crisis").
narrative_ontology:topic_domain(french_electoral_legitimacy_crisis, "political/institutional").

domain_priors:requires_active_enforcement(french_electoral_legitimacy_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_electoral_legitimacy_crisis, centrist_establishment).
narrative_ontology:constraint_beneficiary(french_electoral_legitimacy_crisis, eu_institutional_continuity).
narrative_ontology:constraint_victim(french_electoral_legitimacy_crisis, democratic_accountability).
narrative_ontology:constraint_victim(french_electoral_legitimacy_crisis, peripheral_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (SNARE) — Trapped within an electoral system that concentrates power in centrist coalitions regardless of voting preference. Cordon sanitaire logic prevents far-right or far-left parties from coalition eligibility despite significant vote share. Exit options are purely formal (switching votes produces identical structural outcome). Theater: mandatory voting absence, protest voting, or resignation. Maximum experienced extraction — structural barrier to meaningful electoral representation.
constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL CONSTITUENCY (TANGLED ROPE) — Constrained by multi-round ballot structure and coalition politics, but also benefits from local voice in parliament and pork-barrel resource allocation. Regional representatives both leverage the system for local benefit and enforce the centrist constraint nationally. Significant extraction (exclusion from national coalition power) paired with genuine coordination benefit (local infrastructure, regional advocacy). Exit cost is high (loss of local influence) but not insurmountable.
constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRIST ESTABLISHMENT (ROPE) — Institutional beneficiary. Experiences the electoral constraint as coordination: the multi-round system, cordon sanitaire norms, and coalition discipline all produce stable governance. Exit from these norms would fragment parliamentary control. The establishment derives arbitrage value through government formation reliability and coalition partner negotiating power. Low effective extraction — the constraint subsidizes institutional continuity.
constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EU INSTITUTIONAL FRAMEWORK (ROPE) — Supranational beneficiary. French electoral legitimacy constraints (centrist bias, establishment consolidation) produce reliable EU policy alignment. The system coordinates French institutional behavior toward EU regulatory compliance and fiscal discipline. Exit would require constitutional change. Low extraction from EU's perspective — the constraint is coordination of policy alignment across member states.
constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: REPUBLICAN VALUES DISCOURSE (PITON) — Highly performative. Political actors invoke 'Republican values,' 'universal suffrage,' and 'popular sovereignty' while enforcing cordon sanitaire rules that contradict these principles. Theater ratio is elevated because the legitimating narrative (democratic will, electoral representation) is disconnected from actual institutional function (coalition gatekeeping). The theater persists through institutional inertia — actors cannot publicly justify the constraint without violating its legitimating myth.
constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CHALLENGER MOVEMENTS (TANGLED ROPE) — Powerful (high vote share) but mobile (can exit through institutional reform, EU exit rhetoric, or constitutional challenge). Experiences the constraint as mixed: genuine coordination benefits from institutional stability (rule of law, property rights, contract enforcement) paired with asymmetric extraction (coalition exclusion despite electoral strength). Can theoretically dissolve the constraint by winning supermajority, but this requires structural path unavailable under current rules. High extracted value paired with genuine institutional coordination benefit.
constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, liberal democracies require some exclusion mechanism to prevent democratic collapse via authoritarian takeover. The cordon sanitaire is then viewed as an immutable structural feature of democratic governance itself — like entropy in thermodynamics. However, this naturalizes a contingent institutional choice. The false summit will be detected by the engine.
constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_electoral_legitimacy_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_electoral_legitimacy_crisis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_electoral_legitimacy_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_electoral_legitimacy_crisis, TR),
    TR >= 0.70.

:- end_tests(french_electoral_legitimacy_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from disenfranchised voters (reduced representative power, suppressed electoral agency) and from challenger movements (excluded from coalition participation despite 30%+ vote share). However, extraction is not maximal because the system provides genuine coordination benefits (stable governance, multi-round ballots enabling local accountability, EU policy reliability) and because some agents (regional constituencies) benefit from the arrangement. The rising trajectory (0.35→0.58) reflects increasing fragmentation without proportional expansion of coalition rules — as more voters shift to excluded parties, the gap between electoral input and legislative representation widens, driving extractiveness upward. Suppression (0.62): Moderate-high. Suppression operates through both structural barriers (multi-round ballot mechanics requiring strategic voting, legal rules prohibiting far-right coalition participation, information asymmetry in national vs local media coverage) and cognitive/internalized mechanisms (voters internalize narrative that 'centrist compromise is inevitable,' that extreme parties are dangerous by definition, that voting for alternatives is wasted effort). The cognitive component is significant — it allows the system to persist without explicit enforcement. Theater ratio (0.68): High. Political actors invoke 'Republican values,' 'universal suffrage,' and 'democratic will' while simultaneously enforcing rules that contradict these principles. The theater has increased as the legitimacy gap widened — more performative invocation of democratic values correlates with more explicit rule-based gatekeeping. Public intellectuals defend cordon sanitaire through democratic theory while acknowledging its tension with democratic principle; this metacognitive dissonance is itself theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between beneficiaries (centrist establishment, EU) who experience the constraint as Rope (coordination with arbitrage benefits) and victims (disenfranchised voters, challengers) who experience it as Snare or Tangled Rope (extraction with constrained/trapped exit). Regional actors occupy an intermediate position — they benefit locally while enforcing centrally. The analytical observer risks false summit by naturalizing the constraint as inherent to democracy. The legitimacy crisis emerges from the widening gap: as challenger vote share increases without corresponding coalition rule expansion, the system's coordination function becomes visible as gatekeeping. When coordination visibly benefits one coalition and harms others, the Rope narrative collapses and Snare becomes visible to previously-passive observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the constraint. Disenfranchised voters are victims with trapped exit options: high d → high f(d) → high experienced extractiveness. Centrist establishment are beneficiaries with arbitrage options: low d → low/negative f(d) → low/negative experienced extractiveness (extraction runs toward them). Challenger movements are victims but with mobile exit options: d is moderate (0.55-0.65, higher than beneficiaries but lower than trapped victims) → moderate f(d) → moderate χ. Regional constituencies with local power derive moderate d depending on whether they are aligned or misaligned with national centrist coalitions. The EU institutional perspective is a beneficiary with arbitrage options (can exit French coordination through treaty renegotiation but this is prohibitively costly): very low d. The Piton perspective on Republican values has organizational status (organized agents maintain the discourse) with constrained exit options (cannot publicly abandon democratic legitimating narrative): moderate-high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is between the coordination function (multi-round ballots enabling local accountability, coalition discipline producing stable governance) and the extraction mechanism (non-linear vote-to-seat mapping, coalition gatekeeping of excluded parties). A pure Rope reading would emphasize coordination; a pure Snare reading would emphasize extraction. Tangled Rope resolves this by asserting that both are real and structurally coupled — the same institutional rules that enable coordination also enable gatekeeping. The false summit (Mountain) arises from attempting to naturalize the gatekeeping as inherent to democracy. The Piton perspective correctly identifies that theater ratio is high — the legitimating narrative (universal suffrage) is performatively maintained while the actual mechanism (coalition gatekeeping) contradicts it. The legitimacy crisis emerges precisely when the theater breaks — voters recognize the performance as performance. The constraint will either transition to explicit Snare (if centrist coalition admits the gatekeeping) or attempt to restore the theater through new legitimating narratives (e.g., 'democratic circuit-breaker,' 'protection against extremism'). The Scaffold perspective is absent from the current state but could emerge if electoral reform (proportional representation, primary elections, supermajority requirement reductions) creates a sunset trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cordon_legitimacy_threshold,
    'At what electoral threshold does cordon sanitaire exclusion become structurally illegitimate under democratic theory?',
    'Comparative analysis of democratic systems with similar vote-share thresholds; normative democratic theory assessment of representation vs security tradeoffs; longitudinal tracking of legitimacy surveys in countries with explicit cordon sanitaire rules',
    'If threshold is low (15-20% vote share): current French system is indefensible exclusion. If threshold is high (35-40%): cordon sanitaire is legitimate democratic circuit-breaker. This determines whether the constraint is Snare (illegitimate) or Scaffold (legitimate protection with sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cordon_legitimacy_threshold, conceptual, 'Threshold at which cordon sanitaire becomes illegitimate').

omega_variable(
    institutional_exit_feasibility,
    'Can far-right/far-left challenger movements actually reform the electoral system through constitutional amendment, or is the institutional path itself locked by the constraint?',
    'Analysis of amendment procedures; assessment of coalition logic for supermajority; comparative case studies of electoral system reform in constrained democracies (Italy 1993, Hungary 2011)',
    'If exit is feasible (constitutional amendment achievable): challenger power is real and Tangled Rope classification holds. If exit is locked: constraint is more like Snare for challengers, and the system exhibits true structural domination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_exit_feasibility, empirical, 'Whether institutional exit path is available for challengers').

omega_variable(
    eu_delegation_necessity,
    'Is centrist policy continuity a genuine EU coordination requirement, or is it a post-hoc justification for institutional gatekeeping that benefits French economic elites?',
    'Comparative policy analysis: do countries with different electoral structures have dramatically different EU compliance rates? Analysis of EU enforcement actions against centrist vs non-centrist governments. Counterfactual assessment of whether Left/Right alternatives could meet EU fiscal/regulatory requirements.',
    'If genuine requirement: EU institutional perspective is justified. If post-hoc: the ''coordination'' is actually cultural gatekeeping disguised as technical necessity. This affects whether EU sees Rope or benefits from Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_delegation_necessity, empirical, 'Whether EU alignment is genuine coordination requirement or post-hoc justification').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Is voter suppression in this constraint primarily structural (legal barriers, ballot mechanics, information asymmetry) or cognitive (internalized narratives about inevitability, identity fusion with centrist compromise)?',
    'Voter turnout analysis by party affiliation; survey data on perceived electoral agency; tracking of alternative platform visibility. If suppression drops after centrist legitimacy crisis (e.g., failed government, policy failure), attribution to cognitive component.',
    'If structural: suppression persists across electoral cycles regardless of outcomes. If cognitive: suppression breaks when centrist legitimacy breaks. Classification implications: Snare (structural) vs identity_locked Tangled Rope (cognitive). Measurement implications: expect different trajectories post-crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, empirical, 'Whether suppression is structural or cognitive/internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_electoral_legitimacy_crisis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(felc_tr_t0, french_electoral_legitimacy_crisis, theater_ratio, 0, 0.52).
narrative_ontology:measurement(felc_tr_t5, french_electoral_legitimacy_crisis, theater_ratio, 5, 0.61).
narrative_ontology:measurement(felc_tr_t10, french_electoral_legitimacy_crisis, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(felc_be_t0, french_electoral_legitimacy_crisis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(felc_be_t5, french_electoral_legitimacy_crisis, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(felc_be_t10, french_electoral_legitimacy_crisis, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_electoral_legitimacy_crisis, enforcement_mechanism).
narrative_ontology:affects_constraint(french_electoral_legitimacy_crisis, eu_regulatory_capture).
narrative_ontology:affects_constraint(french_electoral_legitimacy_crisis, french_housing_crisis).
narrative_ontology:affects_constraint(french_electoral_legitimacy_crisis, public_service_legitimacy).

% DUAL FORMULATION NOTE:
% French electoral legitimacy crisis decomposes into at least two structurally distinct constraints: (1) the two-round ballot coordination system (ε~0.25, primarily Rope with efficiency benefits), and (2) the cordon sanitaire gatekeeping rule (ε~0.68, primarily Snare/Tangled Rope with explicit extraction). This JSON story treats them as one unified constraint because their institutional coupling is structural — you cannot remove gatekeeping without rebuilding two-round ballot logic, and you cannot defend two-round ballots without invoking gatekeeping justification. Network edges capture downstream impacts: EU regulatory outcomes depend on French coalition stability (affects_constraints: eu_regulatory_capture); regional coalition gatekeeping reduces local investment in non-aligned constituencies (affects_constraints: french_housing_crisis); public legitimacy of elections affects legitimacy of public institutions generally (affects_constraints: public_service_legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_electoral_legitimacy_crisis, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
