% ============================================================================
% CONSTRAINT STORY: sanctions_regime_effectiveness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sanctions_regime_effectiveness, []).

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
 *   constraint_id: sanctions_regime_effectiveness
 *   human_readable: Sanctions Regime Effectiveness and Extractive Coalition Dynamics
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   A sanctions regime represents a coalition enforcement mechanism that
 *   coordinates international norms while simultaneously extracting from both
 *   the targeted population and peripheral coalition members. The constraint
 *   exhibits structural duality: from the beneficiary's (sanctioning core)
 *   perspective, sanctions coordinate geopolitical resolve and signaling;
 *   from the victim's (sanctioned civilian) perspective, they are pure
 *   extraction with no exit. The theater ratio increases over time (0.52 →
 *   0.68) as sanctions persist without achieving stated goals, indicating
 *   growing performative content: the institutional machinery continues
 *   despite evident ineffectiveness, maintained through coalition signaling
 *   rather than functional enforcement. This pattern suggests a piton dynamic
 *   emerging within the architecture while the snare intensifies for
 *   civilians. The key agents represent distinct structural positions in the
 *   extraction flow: the sanctioning coalition core captures geopolitical
 *   benefits; sanctioned civilians bear maximum economic cost; neutral third
 *   parties navigate constrained positioning; regime opposition faces mixed
 *   incentives; and humanitarian organizations push against the regime's
 *   edges seeking exemption carve-outs.
 *
 * KEY AGENTS:
 *   - Sanctioning Coalition Core: Primary beneficiary (institutional/arbitrage) — captures geopolitical advantage, demonstrates resolve, achieves coalition signaling. Can exit through negotiation.
 *   - Sanctioned Civilian Population: Primary victim (powerless/trapped) — bears economic collapse, currency devaluation, medical scarcity. No exit options.
 *   - Targeted Regime's Opposition: Secondary victim/beneficiary (organized/constrained) — benefits from international support but suffers from economic collapse affecting their base. Constrained by reputational dynamics.
 *   - Neutral Third-Party Economies: Secondary victim (moderate/constrained) — face supply chain disruption, energy price shocks, trade diversion costs. Constrained by reputational pressure to comply.
 *   - Humanitarian/Development Organizations: Boundary negotiators (organized/mobile) — push for exemptions, maintain aid corridors, have mobile exit option through legal carve-outs.
 *   - International Sanctions Architecture: Institutional actor (institutional/constrained) — UN Security Council, export control regimes, financial sanctions apparatus. Constrained by organizational norms; theater increasing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sanctions_regime_effectiveness, 0.58).
domain_priors:suppression_score(sanctions_regime_effectiveness, 0.65).
domain_priors:theater_ratio(sanctions_regime_effectiveness, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sanctions_regime_effectiveness, extractiveness, 0.58).
narrative_ontology:constraint_metric(sanctions_regime_effectiveness, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sanctions_regime_effectiveness, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sanctions_regime_effectiveness, tangled_rope).
narrative_ontology:human_readable(sanctions_regime_effectiveness, "Sanctions Regime Effectiveness and Extractive Coalition Dynamics").
narrative_ontology:topic_domain(sanctions_regime_effectiveness, "geopolitical/economic").

domain_priors:requires_active_enforcement(sanctions_regime_effectiveness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sanctions_regime_effectiveness, sanctioning_coalition_core).
narrative_ontology:constraint_beneficiary(sanctions_regime_effectiveness, regime_opposition_factions).
narrative_ontology:constraint_victim(sanctions_regime_effectiveness, targeted_civilian_population).
narrative_ontology:constraint_victim(sanctions_regime_effectiveness, global_trade_partners).
narrative_ontology:constraint_victim(sanctions_regime_effectiveness, sanctioning_coalition_periphery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONED CIVILIANS (SNARE) — Face maximum extraction through economic collapse, currency devaluation, medicine/food scarcity, and infrastructure decay. No exit options; trapped within borders and economic collapse. Suppression is structural (legal prohibition on trade, asset freezes, banking isolation). Extraction increases over time as regime responds to sanctions with internal controls.
constraint_indexing:constraint_classification(sanctions_regime_effectiveness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEUTRAL THIRD PARTIES (TANGLED ROPE) — Experience both coordination benefit (alignment with sanctioning coalition's stated norms) and extraction (economic cost through supply chain disruption, energy price shocks, trade diversion). Constrained by reputational risk of non-compliance; exit costs are high but not absolute. Mixed experience reflects genuine dilemma between coordination and extraction.
constraint_indexing:constraint_classification(sanctions_regime_effectiveness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: SANCTIONING COALITION CORE (ROPE) — Primary beneficiary (institutional actors with arbitrage options). Experience sanctions as coordination mechanism: enforcing international norms, demonstrating resolve, signaling unity. Net beneficiary through reputational gains, geopolitical advantage, and coalition cohesion. Can exit sanctions through diplomatic negotiation (arbitrage option). Theater ratio is moderate for this perspective — enforcement is largely genuine, not performative.
constraint_indexing:constraint_classification(sanctions_regime_effectiveness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SANCTIONS ARCHITECTURE (PITON) — UN Security Council framework, export control regimes, financial sanctions apparatus. Theater ratio (0.68) reflects that much of the institutional activity is performative: Security Council resolutions are often symbolic, enforcement is inconsistent, secondary sanctions are theatrical compliance signaling. The institutional machinery persists through inertia despite low functional effectiveness in achieving stated policy goals (regime change, behavioral modification). Architecture actors are constrained by organizational norms.
constraint_indexing:constraint_classification(sanctions_regime_effectiveness, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANITARIAN COALITION (SCAFFOLD) — Organized agents (NGOs, UN agencies, medical organizations) see sanctions regime as a temporary response to crisis that requires sunset clauses: humanitarian corridors, medicine exemptions, development aid carve-outs. Mobile exit option — these organizations can work around sanctions through legal exemptions and can exit if exemptions are revoked. Low extraction from this perspective because organized agents have negotiating power and see clear policy pathways (exemption expansion, targeted lifting of sectoral sanctions).
constraint_indexing:constraint_classification(sanctions_regime_effectiveness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIME OPPOSITION (TANGLED ROPE) — Benefit from sanctions as coordination mechanism (international support for regime change) but also experience extraction through economic collapse affecting their own support bases and capacity to organize. Constrained exit — cannot openly abandon sanctions without reputational cost; cannot benefit without appearing complicit. Theater ratio reflects that sanctions are often invoked as regime-change tool but lack capability to achieve stated goal, creating performative cycle of enforcement.
constraint_indexing:constraint_classification(sanctions_regime_effectiveness, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, sanctions are depicted as inevitable consequence of international law and norms enforcement — presented as unchangeable natural law of geopolitics. However, structural data contradicts this: sanctions are institutionally contingent policy choices with measurable extractiveness, suppression, and theater. Engine will flag this as false summit, revealing that 'inevitable enforcement' naturalizes what is actually a malleable institutional arrangement.
constraint_indexing:constraint_classification(sanctions_regime_effectiveness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sanctions_regime_effectiveness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sanctions_regime_effectiveness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sanctions_regime_effectiveness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sanctions_regime_effectiveness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sanctions_regime_effectiveness, TR),
    TR >= 0.70.

:- end_tests(sanctions_regime_effectiveness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting two competing dynamics. Initial extractiveness (0.35) captures the coordination function — genuine international cooperation on norms enforcement. But extractiveness rises over 6-year interval to 0.58 as the regime persists without achieving stated policy goals (regime change, behavioral shift), indicating accumulation of extraction without functional return. Suppression (0.65): High. Legal prohibition on trade, asset freezes, banking isolation, capital controls create severe barriers. But suppression is not maximal (0.85+) because some goods flow through secondary channels, some exemptions exist (humanitarian corridors), and targeted regime maintains some external trade relationships. Theater ratio (0.68): Moderate-high and rising. Sanctions enforcement includes genuine policy coordination (Security Council resolutions, aligned export controls) but also increasingly performative content: enforcement inconsistency, secondary sanctions as theatrical compliance signaling, institutional machinery continuing despite known ineffectiveness. The rising theater curve suggests degradation toward piton classification for the architecture itself. Claimed type (Tangled Rope): Correct. The regime has clear coordination function (enforcing international norms, signaling unity) alongside asymmetric extraction (concentrated on civilians and peripheral economies). Active enforcement is required; beneficiaries and victims are distinct.
 *
 * PERSPECTIVAL GAP:
 *   The original constraint statement assumes a single sanctions regime, but the structural data reveals multiple distinct constraints decomposed by actor perspective. The core-state experience (Rope coordination) and civilian experience (Snare extraction) have different ε values and different failure modes. The architecture's performative content (Piton) emerges as a third distinct constraint within the broader regime. This suggests decomposition into a constraint family: (1) sanctions_as_coalition_coordination (ε ≈ 0.20, Rope), (2) sanctions_civilian_extraction (ε ≈ 0.75, Snare), (3) sanctions_architecture_performance (ε ≈ 0.55, Piton/Tangled Rope mixed). However, these are structurally coupled: the coalition coordination works because civilians bear costs; civilian extraction is only possible because the architecture enforces it. The coupling is tight enough that a single constraint story is defensible, with perspectives showing the perspectival divergence. Trade-off: single story captures the coupling; decomposed family captures the structural distinctness. Current choice: single story with strong perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness flows from their structural position relative to the extraction pipeline. Beneficiaries (sanctioning core) are positioned at the source of benefit; their d-value is low because the constraint flow runs toward them. Victims (sanctioned civilians) are positioned as targets; their d-value is high because the constraint flow runs away from them. The sigmoid f(d) translates position into experienced force. Constrained actors (third parties, opposition) sit in between; their d-values reflect mixed position (some benefit, some cost). Mobile actors (humanitarian organizations) can escape extraction through negotiated exemptions; their d-values are moderated by exit option. The directionality overrides are unnecessary here — the structural data (beneficiary/victim declarations + exit options + power levels) produces accurate d-values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC OF COALITION EXTRACTION: This constraint resolves potential mandatrophy by showing that the tangled_rope classification correctly captures the coordination-extraction hybrid. The danger zone: misclassifying as pure Rope (assuming all agents experience coordination benefit) or pure Snare (assuming all agents experience extraction). The structural data prevents this error: clear beneficiary (sanctioning core) and clear victims (sanctioned civilians) are declared, preventing conflation. The theater increase (0.52 → 0.68) flags that the functional coordination is degrading and performative signaling is increasing, which could presage reclassification toward Piton if theater continues rising. The architecture perspective correctly captures this Piton risk. Mandatrophy is resolved by the perspectival plurality: the constraint IS both coordination and extraction depending on observer position; the classification system captures both readings simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_measurement_ambiguity,
    'What constitutes ''effectiveness'' of sanctions — stated policy goal (regime change, behavioral modification) or actual measurable outcome (compliance, policy shift)?',
    'Longitudinal analysis of sanctions outcomes: correlation between sanctions duration and stated vs actual policy changes; comparison of regime behavior with/without sanctions; measurement of compliance on specific demands',
    'If effectiveness = stated goal: most sanctions regimes fail (rarely achieve regime change); extractiveness classification remains high. If effectiveness = compliance on specific demands: some regimes show modest compliance; extractiveness may moderate. Framework depends on observable chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_measurement_ambiguity, empirical, 'Ambiguity between stated vs actual effectiveness metrics').

omega_variable(
    civilian_burden_moral_asymmetry,
    'Is civilian suffering a necessary cost of sanctions enforcement, or is it an extractive side effect of a mechanism that could be redesigned (sectoral targeting, exemption carve-outs)?',
    'Comparative analysis of different sanctions designs: targeted financial sanctions vs comprehensive embargoes; outcome data on regimes responding to humanitarian sanctions vs broad population sanctions; counterfactual modeling of exemption-heavy regimes',
    'If suffering is necessary: snare classification for civilians is correct; suppression is intrinsic. If suffering is contingent on design choice: extractiveness could be reduced through institutional redesign; perspective shifts from snare toward constrained tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_burden_moral_asymmetry, preference, 'Whether civilian suffering is necessary or contingent design choice').

omega_variable(
    coalition_unity_signaling_vs_enforcement,
    'Are sanctions primarily an enforcement mechanism (designed to change target behavior) or a coalition-unity signaling device (designed to demonstrate resolve to coalition members)?',
    'Analysis of sanctioning coalition decision-making: interviews with policymakers, policy documents, timing of sanctions adoption relative to diplomatic opportunities; measurement of variance between stated enforcement goals and actual enforcement intensity',
    'If primarily enforcement: tangled_rope classification is correct. If primarily signaling: more of the extractiveness is performative (theater_ratio should be higher); classification shifts toward piton for coalition members and snare for civilians.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_unity_signaling_vs_enforcement, empirical, 'Primary function of sanctions: enforcement vs coalition signaling').

omega_variable(
    sanctions_regime_exit_pathways,
    'Do negotiated sanctions relief pathways exist that would allow exit before regime change, or is the sanctions regime effectively permanent until political transformation?',
    'Historical analysis of sanctions lifting: conditions under which sanctions have been lifted without regime change; current diplomatic pathways available to targeted regime; feasibility modeling of partial compliance + partial relief scenarios',
    'If pathways exist: constraint can be scaffold (temporary with sunset). If pathways blocked: constraint is snare (no exit). Current assessment: pathways are theoretically available but politically constrained, making this a binomial uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_regime_exit_pathways, empirical, 'Existence and accessibility of sanctions relief pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sanctions_regime_effectiveness, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanc_tr_t0, sanctions_regime_effectiveness, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sanc_tr_t3, sanctions_regime_effectiveness, theater_ratio, 3, 0.6).
narrative_ontology:measurement(sanc_tr_t6, sanctions_regime_effectiveness, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(sanc_be_t0, sanctions_regime_effectiveness, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sanc_be_t3, sanctions_regime_effectiveness, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sanc_be_t6, sanctions_regime_effectiveness, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sanctions_regime_effectiveness, enforcement_mechanism).
narrative_ontology:affects_constraint(sanctions_regime_effectiveness, humanitarian_access_bottleneck).
narrative_ontology:affects_constraint(sanctions_regime_effectiveness, trade_diversion_arbitrage).
narrative_ontology:affects_constraint(sanctions_regime_effectiveness, regime_legitimacy_paradox).

% DUAL FORMULATION NOTE:
% Sanctions regime effectiveness decomposes into at least three structurally distinct constraints: (1) Coalition coordination on norm enforcement (Rope), (2) Civilian extraction through economic collapse (Snare), (3) Architecture performativity as sanctions persist without policy goals achieved (Piton). The three are coupled via the enforcement pipeline but have different ε values and failure modes. Current story models the coupling as a single tangled_rope; decomposition into family may be warranted as empirical data on architecture degradation accumulates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sanctions_regime_effectiveness, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
