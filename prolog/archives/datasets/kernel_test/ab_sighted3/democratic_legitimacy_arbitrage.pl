% ============================================================================
% CONSTRAINT STORY: democratic_legitimacy_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_democratic_legitimacy_arbitrage, []).

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
 *   constraint_id: democratic_legitimacy_arbitrage
 *   human_readable: Democratic Legitimacy Arbitrage in Populist Governance
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The democratic legitimacy arbitrage constraint emerges when populist
 *   supporters simultaneously endorse representative democracy as an abstract
 *   ideal and strong-leader governance without parliamentary or judicial
 *   interference as a practical necessity. This creates an arbitrage
 *   opportunity for populist leadership: claim democratic legitimacy through
 *   electoral victory while dismantling the institutional checks that
 *   constrain executive power. The constraint is downstream of both
 *   post-industrial spatial extraction (which creates the economic grievances
 *   populism mobilizes) and populist-as-class-realignment (which provides the
 *   political vehicle). Survey data from multiple democracies experiencing
 *   populist governance shows strong correlation between populist support and
 *   simultaneous endorsement of both democratic principles and strong-leader
 *   preferences, with the correlation increasing over the populist governance
 *   period. The legitimacy arbitrage operates through redefinition:
 *   'democracy' comes to mean popular sovereignty embodied in the leader
 *   rather than institutional procedures and constraints. This redefinition
 *   is invisible to supporters whose identity is constituted through the
 *   populist frame but visible to opposition actors and external observers.
 *   The constraint's theater ratio (0.64) reflects that democratic procedures
 *   (elections, legislative debate, judicial review) increasingly become
 *   performative rather than functional as institutional checks erode. The
 *   extraction accumulates over the interval as initial institutional
 *   weakening enables further concentration of power.
 *
 * KEY AGENTS:
 *   - Populist Supporter: Primary target (powerless/identity_locked) — identity constituted through populist frame; cannot perceive contradiction between democratic endorsement and strong-leader support
 *   - Opposition Voter: Secondary victim (powerless/trapped) — experiences pure extraction; no exit option within biographical horizon
 *   - Populist Leadership: Primary beneficiary (institutional/arbitrage) — exploits legitimacy arbitrage to concentrate power while maintaining democratic claim
 *   - Civil Society Organization: Constrained actor (moderate/constrained) — mixed experience of democratic framing (enables advocacy) and strong-leader governance (undermines protections)
 *   - International Democracy Promotion Network: Organized observer (organized/mobile) — coordination problem with embedded extraction; democratic framing complicates conditionality
 *   - Independent Judiciary: Institutional victim (moderate/constrained) — direct target of delegitimization; cannot exit without abandoning role
 *   - Comparative Politics Scholar: Analytical observer (analytical/analytical) — reveals structural logic but also benefits from and is constrained by the phenomenon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(democratic_legitimacy_arbitrage, 0.58).
domain_priors:suppression_score(democratic_legitimacy_arbitrage, 0.68).
domain_priors:theater_ratio(democratic_legitimacy_arbitrage, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(democratic_legitimacy_arbitrage, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(democratic_legitimacy_arbitrage, snare).
narrative_ontology:human_readable(democratic_legitimacy_arbitrage, "Democratic Legitimacy Arbitrage in Populist Governance").
narrative_ontology:topic_domain(democratic_legitimacy_arbitrage, "political_economy/comparative_politics/democratic_theory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(democratic_legitimacy_arbitrage, populist_leadership).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, institutional_checks_and_balances).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, opposition_political_actors).
narrative_ontology:constraint_victim(democratic_legitimacy_arbitrage, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULIST SUPPORTER (SNARE) — Identity-locked within the populist frame that treats democratic legitimacy as residing in the leader's direct connection to 'the people' rather than in institutional procedures. Simultaneously endorses representative democracy (as abstract ideal) and strong-leader governance without checks (as practical necessity). The contradiction is invisible from within the identity frame because 'democracy' has been redefined as popular will embodied in the leader. High extraction: institutional protections eroded, but supporter cannot exit because their political identity is constituted through the populist movement.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION VOTER (SNARE) — Trapped by the electoral outcome and institutional erosion. Experiences the constraint as pure extraction: the legitimacy arbitrage allows populist leadership to claim democratic mandate while dismantling the institutional checks that would constrain executive power. No exit option within the biographical horizon — cannot leave the polity, cannot organize effective resistance when institutions are captured, cannot appeal to checks that have been delegitimized.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: POPULIST LEADERSHIP (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the constraint as pure coordination: the simultaneous endorsement of democratic legitimacy and strong-leader governance solves the political problem of maintaining popular support while concentrating power. Can exit to international safe havens, alternative power bases, or post-political careers. The legitimacy arbitrage is a resource to be exploited, not a constraint to be endured.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY ORGANIZATION (TANGLED ROPE) — Constrained by funding dependencies, legal harassment, and delegitimization campaigns, but retains some agency and benefits from the democratic framing (can still appeal to 'the people' and 'democratic values'). Experiences mixed extraction: the legitimacy arbitrage both enables (democratic rhetoric provides cover for advocacy) and constrains (strong-leader governance undermines institutional protections for civil society). Can exit at high cost (international relocation, dissolution) but not easily.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL DEMOCRACY PROMOTION NETWORK (TANGLED ROPE) — Organized actors (EU institutions, international NGOs, democracy indices) with mobile exit options. Experience the constraint as coordination problem with embedded extraction: the legitimacy arbitrage complicates their monitoring and conditionality frameworks (how to sanction a regime that wins elections and claims democratic mandate?). Benefits from the democratic framing (provides entry point for engagement) but extraction occurs through institutional erosion that undermines their leverage.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: INDEPENDENT JUDICIARY (SNARE) — Constrained by constitutional position (cannot exit without abandoning role) and facing direct institutional attack. The legitimacy arbitrage delegitimizes judicial review as 'undemocratic' obstruction of the popular will. High extraction: institutional independence eroded, individual judges face career risk and legal harassment, but formal exit (resignation) means abandoning the institution entirely. Suppression operates through both formal mechanisms (court-packing, jurisdiction-stripping) and informal pressure (media campaigns, threats).
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Analytical perspective reveals the legitimacy arbitrage as a structural feature of populist governance, not a cognitive error by supporters. The simultaneous endorsement is instrumentally rational given the populist frame: 'democracy' means popular sovereignty (which the leader embodies), not institutional constraints (which obstruct the popular will). However, the analytical observer also benefits from the constraint (research funding, policy relevance, citation advantage) while being constrained by it (difficulty accessing field sites, pressure to avoid 'delegitimizing' elected governments). Mixed coordination and extraction.
constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(democratic_legitimacy_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(democratic_legitimacy_arbitrage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(democratic_legitimacy_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(democratic_legitimacy_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The legitimacy arbitrage extracts from institutional checks and balances (primary victim) and opposition political actors (secondary victim) while benefiting populist leadership. The extraction is substantial but not maximal because some institutional constraints persist and the democratic framing provides limited protection for opposition actors. The value reflects that the arbitrage is a real extraction mechanism, not merely a coordination problem, but operates through institutional erosion rather than direct coercion. Suppression (0.68): High. Significant barriers to exit and resistance include identity lock for supporters (cannot perceive the contradiction from within the populist frame), institutional capture (opposition cannot appeal to checks that have been delegitimized), legal harassment of civil society, media control, and electoral manipulation. Suppression increases over the interval as institutional erosion compounds. Theater ratio (0.64): Moderate-high. Democratic procedures increasingly become performative as institutional checks erode: elections are held but opposition is disadvantaged, legislative debate occurs but parliament is sidelined, judicial review exists but courts are packed or jurisdiction-stripped. The theater has increased over the interval as the gap between democratic form and authoritarian function widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the identity_locked mechanism at scale: populist supporters genuinely endorse both representative democracy and strong-leader governance without perceiving contradiction because their identity frame redefines 'democracy' as popular will embodied in the leader. From within this frame, institutional checks are undemocratic obstacles to the popular will, not protections of democratic procedure. The opposition voter sees pure extraction (snare) because they experience the institutional erosion directly and have no exit. The populist leadership sees coordination (rope) because the legitimacy arbitrage solves their political problem. Civil society and international networks see mixed coordination and extraction (tangled rope) because the democratic framing both enables and constrains their work. The analytical observer reveals the structural logic: the simultaneous endorsement is instrumentally rational given the populist frame, not a cognitive error. The perspectival gap between identity_locked and trapped powerless agents is diagnostic: both are powerless, both are at biographical horizon, but one is cognitively bound and one is materially bound. The gap reveals that the constraint operates through both mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   The populist supporter perspective uses identity_locked exit, which produces high directionality (d ≈ 0.89) despite the supporter's sincere belief in the legitimacy of both representative democracy and strong-leader governance. The identity lock is cognitive rather than material: the supporter is structurally mobile (could vote differently, could exit the populist movement) but functionally trapped by the identity frame that makes the contradiction invisible. The opposition voter is genuinely trapped (d ≈ 0.95) with no exit option within the biographical horizon. The populist leadership is the primary beneficiary with arbitrage exit options (d ≈ 0.05), experiencing the constraint as pure coordination. Civil society organizations and international networks have constrained or mobile exit options (d ≈ 0.55-0.65), producing moderate effective extraction. The independent judiciary is constrained (d ≈ 0.75) — higher than civil society because exit means abandoning the institutional role entirely. The analytical observer has analytical exit (d ≈ 0.72) but also benefits from the constraint through research opportunities, producing mixed experience.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the legitimacy arbitrage is genuine extraction (snare from multiple victim perspectives) rather than pure coordination, but the extraction operates through identity lock and institutional erosion rather than direct coercion. The populist supporter's sincere endorsement of both democracy and strong-leader governance is not evidence that the constraint is coordination — it is evidence that the extraction mechanism operates through redefinition of democratic legitimacy. The analytical perspective reveals this: the simultaneous endorsement is instrumentally rational given the populist frame, which means the frame itself is the extraction mechanism. The constraint is a snare because it traps victims (opposition voters, institutional checks) while benefiting extractors (populist leadership), but the trap operates through cognitive capture of supporters rather than through direct suppression alone. The theater ratio (0.64) confirms that democratic procedures have become substantially performative, which is diagnostic of extraction masked by coordination rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_arbitrage_intentionality,
    'Is the simultaneous endorsement of representative democracy and strong-leader governance a deliberate elite strategy (manufactured consent) or an emergent property of populist framing (sincere belief)?',
    'Longitudinal analysis of elite messaging vs mass opinion formation; experimental manipulation of framing to test causal direction; comparison of top-down vs bottom-up populist movements',
    'If elite-driven: higher extractiveness (deliberate manipulation), snare classification strengthened. If emergent: lower extractiveness (coordination around shared frame), potential tangled_rope reclassification for some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_arbitrage_intentionality, empirical, 'Whether legitimacy arbitrage is elite strategy or emergent framing').

omega_variable(
    institutional_erosion_reversibility,
    'Is the institutional erosion caused by legitimacy arbitrage reversible within a biographical time horizon, or does it create path-dependent lock-in?',
    'Historical analysis of post-populist institutional recovery; comparison of countries that experienced populist governance and subsequently restored checks and balances vs those that did not',
    'If reversible: lower suppression, potential scaffold classification (temporary institutional weakening). If path-dependent: higher suppression, snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_erosion_reversibility, empirical, 'Reversibility of institutional erosion within biographical horizon').

omega_variable(
    democratic_redefinition_scope,
    'Does the populist redefinition of ''democracy'' (from institutional procedures to popular will embodied in leader) represent a coherent alternative democratic theory or a cover story for authoritarian consolidation?',
    'Philosophical analysis of populist democratic theory; empirical tracking of whether populist regimes maintain competitive elections and peaceful transfers of power; comparison with historical cases of democratic backsliding',
    'If coherent alternative: conceptual omega (different normative frameworks, not extractive constraint). If cover story: snare classification confirmed (extraction masked by democratic rhetoric).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_redefinition_scope, conceptual, 'Whether populist democracy is alternative theory or authoritarian cover').

omega_variable(
    identity_lock_breakage_conditions,
    'Under what conditions does the identity lock on populist supporters break, allowing them to perceive the contradiction between democratic endorsement and strong-leader support?',
    'Analysis of populist supporter defection patterns; identification of critical events or information that trigger frame-breaking; comparison of supporters who exit vs those who remain',
    'If identity lock is fragile: lower effective suppression for identity_locked perspective, potential reclassification to constrained. If robust: snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_breakage_conditions, empirical, 'Conditions for breaking populist supporter identity lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(democratic_legitimacy_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dem_arb_tr_t0, democratic_legitimacy_arbitrage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dem_arb_tr_t3, democratic_legitimacy_arbitrage, theater_ratio, 3, 0.53).
narrative_ontology:measurement(dem_arb_tr_t6, democratic_legitimacy_arbitrage, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(dem_arb_be_t0, democratic_legitimacy_arbitrage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dem_arb_be_t3, democratic_legitimacy_arbitrage, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(dem_arb_be_t6, democratic_legitimacy_arbitrage, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dem_arb_su_t0, democratic_legitimacy_arbitrage, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(dem_arb_su_t3, democratic_legitimacy_arbitrage, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(dem_arb_su_t6, democratic_legitimacy_arbitrage, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(democratic_legitimacy_arbitrage, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of post_industrial_spatial_extraction (which creates the economic grievances populism mobilizes) and populist_as_class_realignment (which provides the political vehicle). The legitimacy arbitrage is a distinct structural constraint with its own extractiveness reflecting the institutional erosion and identity lock mechanisms, not merely an epiphenomenon of the upstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
