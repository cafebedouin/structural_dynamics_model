% ============================================================================
% CONSTRAINT STORY: nato_burden_sharing_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nato_burden_sharing_asymmetry, []).

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
 *   constraint_id: nato_burden_sharing_asymmetry
 *   human_readable: NATO Burden-Sharing Asymmetry
 *   domain: international_relations/security_alliance
 *
 * SUMMARY:
 *   NATO burden-sharing asymmetry represents a structural tension between the
 *   alliance's coordinating function (collective defense against external
 *   threat) and the asymmetric extraction of security costs and benefits. The
 *   United States provides nuclear deterrence and forward-basing
 *   infrastructure subsidizing European security; in exchange, it extracts
 *   geopolitical leverage and maintains forward-operating positions in
 *   Eurasia. This arrangement persists despite decades of complaint because
 *   the alternative — independent European rearmament or withdrawal from US
 *   security umbrella — carries high costs for dependent states and reduces
 *   US strategic position. The constraint exhibits genuine coordination
 *   (Article 5 collective defense), significant asymmetric extraction (burden
 *   concentration), and active institutional enforcement (NATO treaty
 *   obligations) alongside increasingly theatrical burden-sharing
 *   announcements (2% spending guideline announced repeatedly but
 *   unenforced). The theater ratio has risen over the interval as
 *   institutional ritual substitutes for actual equalization.
 *
 * KEY AGENTS:
 *   - United States: Primary beneficiary (institutional/arbitrage) — extracts geopolitical leverage, forward basing, strategic dominance in alliance decision-making; can arbitrage commitment levels
 *   - European Dependent States (Baltics, Poland, etc.): Primary victims (powerless/trapped) — face structural dependency on US nuclear deterrence; bear asymmetric defense burden; no realistic exit options
 *   - High-Burden European Coalition: Organized victims (organized/constrained) — coordinate through NATO and EU but constrained by geography and Russian proximity; benefit from US protection but bear higher per-capita costs
 *   - Western European Powers (France, Germany): Mixed position (powerful/constrained) — benefit from US nuclear umbrella; bear moderate burden; constrained by alliance politics but powerful enough to negotiate terms
 *   - European Strategic Autonomy Movement: Organized reformers (organized/mobile) — building alternative coordination pathways; have exit-oriented options through EU defense initiatives and autonomous rearmament
 *   - NATO Institutional Structure: Inertial enforcer (institutional/arbitrage) — maintains consensus fiction and ceremonial burden-sharing discussions; preserves alliance through theater rather than equalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nato_burden_sharing_asymmetry, 0.58).
domain_priors:suppression_score(nato_burden_sharing_asymmetry, 0.65).
domain_priors:theater_ratio(nato_burden_sharing_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nato_burden_sharing_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(nato_burden_sharing_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nato_burden_sharing_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nato_burden_sharing_asymmetry, tangled_rope).
narrative_ontology:human_readable(nato_burden_sharing_asymmetry, "NATO Burden-Sharing Asymmetry").
narrative_ontology:topic_domain(nato_burden_sharing_asymmetry, "international_relations/security_alliance").

domain_priors:requires_active_enforcement(nato_burden_sharing_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nato_burden_sharing_asymmetry, united_states).
narrative_ontology:constraint_beneficiary(nato_burden_sharing_asymmetry, nuclear_umbrella_dependents).
narrative_ontology:constraint_victim(nato_burden_sharing_asymmetry, high_burden_european_states).
narrative_ontology:constraint_victim(nato_burden_sharing_asymmetry, alliance_collective_defense_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EUROPEAN DEPENDENT STATE (SNARE) — Small-to-medium NATO members face structural dependency on US nuclear deterrence and cannot exit without bearing existential security risk. Suppression operates through security dependency: alternatives (independent nuclear capability, non-alignment) are ruled out by cost and geopolitical isolation. Bears asymmetric defense burden without exit capacity.
constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: HIGH-BURDEN EUROPEAN COALITION (TANGLED ROPE) — Poland, Baltics, parts of CEE coordinate genuine collective defense through NATO (rope function) while also extracting disproportionate US military presence (extraction function). These states benefit from US security provision but bear higher per-capita defense spending and host military infrastructure. Constrained by geographic proximity to Russia; organized through NATO structures.
constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: UNITED STATES (ROPE) — Experiences NATO as coordination mechanism for managing European security and maintaining strategic position in Eurasian balance of power. Has exit option (arbitrage): can withdraw, reduce commitment, or redirect military spending. Net beneficiary of alliance structure — extracts geopolitical leverage and forward-basing rights while providing security subsidy.
constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN EUROPEAN MAJOR POWERS (TANGLED ROPE) — France, Germany coordinate genuine alliance functions (interoperability, collective defense planning) while also benefiting from US nuclear umbrella with lower burden-sharing than CEE. Constrained by alliance commitments and intra-alliance politics; powerful enough to negotiate but not to exit. Experience mixed extraction and coordination.
constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EUROPEAN STRATEGIC AUTONOMY MOVEMENT (SCAFFOLD) — Organized actors (EU defense initiatives, PESCO, permanent structured cooperation) are building alternative coordination pathways that reduce dependency on US umbrella. See the asymmetry as temporary — sunset mechanism operates through European rearmament and autonomous deterrence development. Mobile because they can invest in alternatives; organized because they coordinate through EU structures. Theater remains moderate because genuine military capability building is occurring, not just rhetorical posturing.
constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: NATO INSTITUTIONAL CONSENSUS (PITON) — The alliance maintains rhetorical commitment to shared burden-sharing (2% GDP spending guideline) and collective defense (Article 5) while enforcement mechanisms are weak and burden distribution remains asymmetric. NATO institutions preserve consensus fiction through ceremonial meetings and communiqués. Theater ratio reflects that institutional ritual (summit declarations, NATO reporting) substitutes for actual burden equalization. Primary function (collective defense) persists but is maintained through inertia as much as active enforcement.
constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, NATO exhibits genuine coordination function (collective defense, interoperability, crisis response) alongside asymmetric extraction (US security subsidy concentration, strategic leverage asymmetry, forward-basing rights). The constraint persists through enforced alliance treaty (Article 5 obligation) while burden-sharing remains unequalized. Neither pure coordination nor pure extraction — the extraction is enabled by the coordination function.
constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nato_burden_sharing_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nato_burden_sharing_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nato_burden_sharing_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nato_burden_sharing_asymmetry, TR),
    TR >= 0.70.

:- end_tests(nato_burden_sharing_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The US extracts significant value — geopolitical leverage, forward-basing rights, influence over European security decisions — while bearing substantial costs. But the extraction is constrained by Article 5 obligation and coalition politics; the US cannot unilaterally dictate terms without risking alliance dissolution. The value has increased over the measurement interval as European rearmament lags and dependency deepens. Suppression (0.65): High. Dependent states face severe barriers to exit: geographic proximity to Russia, nuclear asymmetry, NATO treaty obligations, economic interdependency, and lack of credible alternative security arrangements. Suppression operates through structural security dependency rather than coercive force — states rationally choose to remain because exit costs are prohibitive. Theater ratio (0.48): Moderate. NATO institutions maintain ongoing ritual commitment to burden-sharing equalization (2% spending guideline, summit declarations, burden-sharing assessments) but enforcement mechanisms are weak. Some theater, but substantive military coordination and capability-building remains real — not yet a pure piton. The ratio has risen over 20 years as repeated burden-sharing promises have failed to equalize distribution.
 *
 * PERSPECTIVAL GAP:
 *   US perspective (Rope): Alliance is a coordination mechanism for collective defense; US benefits through geopolitical position and forward-presence; rational actors cooperate. Dependent state perspective (Snare): Trapped by security dependency; bear costs without exit; extracted from. High-burden coalition perspective (Tangled Rope): Mixed — genuine coordination for collective defense but asymmetric burden on geographically vulnerable members. Western Europe perspective (Tangled Rope): Moderate burden acceptable in exchange for security; can negotiate terms. EU autonomy movement perspective (Scaffold): Constraint is temporary; rearmament and autonomous deterrence will sunset US dependency within 10-20 years. NATO institutional perspective (Piton): Consensus mechanism with weak enforcement; burden-sharing announcements replace actual equalization. The perspectival gap reveals that alignment-based framing (NATO as alliance) obscures structural extraction; the constraint looks like cooperation from the beneficiary's view but extraction from the victim's view. Both are analytically correct about their experienced reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the structural relationship of each agent to the constraint. US (institutional/arbitrage beneficiary): Low d derived from beneficiary status + ability to exit via arbitrage (can reduce commitment or reallocate military resources without eliminating exit option). Dependent states (powerless/trapped victims): High d derived from victim status + inability to exit (security dependency is structural; alternatives ruled out by cost). High-burden coalition (organized/constrained): Intermediate d derived from mixed status (benefit from collective defense; bear concentrated costs; can organize within alliance but cannot exit entirely). Western Europe (powerful/constrained): Mid-range d; powerful enough to negotiate but constrained by alliance commitment and geographic position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing NATO as a genuine tangled rope — it performs authentic collective defense coordination while simultaneously enabling asymmetric extraction. The constraint is NOT pure coordination (Rope) because burden distribution is asymmetric and extractive; the US captures disproportionate benefits while dependent states bear concentrated costs. The constraint is NOT pure extraction (Snare) because collective defense genuinely requires coordinated interoperability, integrated command structures, and shared planning — removing the coordination function would degrade alliance capability for all members. The apparent contradiction (how can it be both?) resolves through the structural insight: the extraction mechanism is enabled BY the coordination function. US dominance of alliance decision-making, forward-basing rights, and strategic leverage all derive from the legitimate coordination role of providing nuclear deterrence and capable rapid-response forces. The asymmetry is not incidental to the coordination; it is structurally embedded in it. The mandatrophy is resolved by showing that Tangled Rope is the correct classification precisely because both functions (coordination and extraction) are simultaneously present and structurally coupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_allocation_methodology,
    'What constitutes ''defense burden'' in NATO burden-sharing? Does GDP-indexed spending capture forward-basing rights, nuclear umbrella subsidy, interoperability investment, and strategic leverage asymmetry?',
    'Comparative accounting: lifecycle cost of NATO membership vs. counterfactual independent defense scenarios; attribution of US forward-basing and nuclear deterrence value across alliance members',
    'If current spending metrics undercount: perceived asymmetry is understated; higher-burden states actually bear more. If metrics overcount: some burden-sharing is illusory (peacekeeping, humanitarian operations counted as defense spending).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_allocation_methodology, empirical, 'Methodology for measuring NATO defense burden allocation').

omega_variable(
    exit_cost_boundary,
    'What is the true exit cost for dependent European states? Is security dependency structural (geopolitically irreplaceable) or contingent (could be replaced by EU deterrence, neutrality with guarantees, or alternative alliances)?',
    'Scenario analysis of European rearmament sufficiency; assessment of whether EU nuclear deterrence or NATO-independent European force posture could achieve equivalent security against Russian threat',
    'If structural: dependent states have no genuine exit; classification remains snare/trapped. If contingent: exit cost is high but surmountable; reclassify to constrained. Affects whether burden-sharing asymmetry is extractive or structural dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_boundary, empirical, 'Whether NATO dependency is structural or contingent').

omega_variable(
    us_strategic_commitment_credibility,
    'Is the US commitment to collective defense (Article 5) credible, or does the asymmetry reflect latent US preference to reduce alliance overhead?',
    'Historical analysis of US Article 5 invocations and response; statements of US strategic intent; comparison to alliance burden-sharing in other US alliances (Japan, South Korea)',
    'If credible: asymmetry reflects structural dependency management within a stable alliance. If non-credible: asymmetry reflects negotiating position; European states over-invest in burden-sharing to ensure US commitment. Affects directionality of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_strategic_commitment_credibility, empirical, 'Credibility of US Article 5 commitment').

omega_variable(
    european_autonomous_deterrence_feasibility,
    'Can European strategic autonomy movement actually produce credible independent deterrence within 10-20 years, or does nuclear deterrence require scale/technology only the US possesses?',
    'Technical assessment of EU nuclear deterrence development; comparison to French/British deterrence models; feasibility analysis of European integrated air defense and rapid-reaction forces',
    'If feasible: scaffold perspective is correct; sunset mechanism is real and timeline is 10-20 years. If infeasible: scaffold is aspirational; dependence is structural; burden-sharing asymmetry persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(european_autonomous_deterrence_feasibility, empirical, 'Feasibility of European independent deterrence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nato_burden_sharing_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nato_tr_t0, nato_burden_sharing_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nato_tr_t10, nato_burden_sharing_asymmetry, theater_ratio, 10, 0.42).
narrative_ontology:measurement(nato_tr_t20, nato_burden_sharing_asymmetry, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(nato_be_t0, nato_burden_sharing_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nato_be_t10, nato_burden_sharing_asymmetry, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(nato_be_t20, nato_burden_sharing_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nato_burden_sharing_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(nato_burden_sharing_asymmetry, european_military_capability_development).
narrative_ontology:affects_constraint(nato_burden_sharing_asymmetry, russian_sphere_influence_expansion).
narrative_ontology:affects_constraint(nato_burden_sharing_asymmetry, us_strategic_pivot_asia).

% DUAL FORMULATION NOTE:
% NATO burden-sharing is upstream of specific burden-distribution disputes (3% spending debates, forward-force posture agreements) but represents a distinct structural constraint on how defense costs and benefits are allocated within the alliance. Decomposition by country (US extraction story, European dependency story, strategic autonomy story) would fragment the analysis; the constraint family links these through network relationships rather than separate constraint stories, as the burden-sharing mechanism is a unified alliance-level phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nato_burden_sharing_asymmetry, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
