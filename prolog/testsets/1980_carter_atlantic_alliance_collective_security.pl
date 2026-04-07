% ============================================================================
% CONSTRAINT STORY: 1980_carter_atlantic_alliance_collective_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1980_carter_atlantic_alliance_collective_security, []).

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
 *   constraint_id: 1980_carter_atlantic_alliance_collective_security
 *   human_readable: NATO Atlantic Alliance: Collective Security Commitment Against Soviet Expansion
 *   domain: foreign_policy/geopolitical_alliance
 *
 * SUMMARY:
 *   The NATO Atlantic Alliance, formalized in 1949 and invoked by Carter in
 *   1980 as the institutional response to Soviet expansion, represents a
 *   complex hybrid constraint combining genuine collective security
 *   coordination with asymmetric extraction. The alliance benefits Western
 *   Europe through mutual defense guarantees and prevents Soviet bilateral
 *   domination of weaker nations. It simultaneously extracts military
 *   commitment from the United States, enforces alignment requirements on
 *   European members, and constrains the autonomy of non-aligned nations
 *   caught between superpower spheres. The constraint's structural role is to
 *   institutionalize coalition formation and prevent a world where each
 *   smaller state negotiates individually with Soviet power. The
 *   theater_ratio (0.48) reflects that NATO's function is partially genuine
 *   deterrence coordination and partially organizational ritual — command
 *   structures, planning exercises, and consensus-building mechanisms
 *   maintain institutional presence beyond the minimal requirements of
 *   credible deterrence. The measurement trajectory shows gradual increase in
 *   both theater and extractiveness over the first decade of the 1980s,
 *   reflecting the escalation of Cold War tensions and the increasing
 *   bureaucratic infrastructure required to maintain coalition cohesion under
 *   stress.
 *
 * KEY AGENTS:
 *   - Western Europe and Allied Nations: Primary beneficiary (institutional/arbitrage) — gain collective defense guarantee and protection from Soviet bilateral domination; experience moderate extraction through military burden-sharing and alignment requirements
 *   - United States: Primary beneficiary and extractor (institutional/arbitrage) — gains institutionalized coalition structure for global power projection and ideological leadership; extracts military commitment and geopolitical compliance
 *   - Non-Aligned Nations: Primary victim (powerless/trapped) — caught between Soviet pressure and Western alignment requirements; constrained autonomy in foreign policy; experience extraction through hegemonic pressure without security benefits
 *   - European Member States: Secondary beneficiary and victim (moderate/constrained) — benefit from deterrence but bear disproportionate military burden relative to size; constrained by NATO requirements but have exit option at geopolitical cost
 *   - NATO Bureaucratic Infrastructure: Institutional maintenance actor (institutional/arbitrage) — maintains itself through inertia; operates with high theater ratio despite external structural shifts
 *   - European Defense Integration Movement: Organized actors (organized/mobile) — see NATO as temporary coordination structure with planned sunset through European defense autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1980_carter_atlantic_alliance_collective_security, 0.38).
domain_priors:suppression_score(1980_carter_atlantic_alliance_collective_security, 0.52).
domain_priors:theater_ratio(1980_carter_atlantic_alliance_collective_security, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1980_carter_atlantic_alliance_collective_security, extractiveness, 0.38).
narrative_ontology:constraint_metric(1980_carter_atlantic_alliance_collective_security, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(1980_carter_atlantic_alliance_collective_security, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1980_carter_atlantic_alliance_collective_security, tangled_rope).
narrative_ontology:human_readable(1980_carter_atlantic_alliance_collective_security, "NATO Atlantic Alliance: Collective Security Commitment Against Soviet Expansion").
narrative_ontology:topic_domain(1980_carter_atlantic_alliance_collective_security, "foreign_policy/geopolitical_alliance").

domain_priors:requires_active_enforcement(1980_carter_atlantic_alliance_collective_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1980_carter_atlantic_alliance_collective_security, western_europe_allied_nations).
narrative_ontology:constraint_beneficiary(1980_carter_atlantic_alliance_collective_security, united_states_power_projection).
narrative_ontology:constraint_victim(1980_carter_atlantic_alliance_collective_security, non_aligned_nations).
narrative_ontology:constraint_victim(1980_carter_atlantic_alliance_collective_security, u_s_military_commitment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED NATION (SNARE) — Geographically proximate to NATO sphere without membership. Trapped between Soviet pressure and Western alliance constraints. Cannot exit the bipolar structure; bears full extraction of limited autonomy in foreign policy. The constraint operates as coercive deterrent that limits this agent's independent action while providing no direct security benefit.
constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WESTERN EUROPEAN MEMBER STATE (TANGLED ROPE) — Benefits from collective defense guarantee against Soviet threat but constrained by NATO military commitments, host nation troop deployments, and alignment requirements. Exit is costly — loss of security guarantee and geopolitical isolation — but structurally possible. Genuine coordination function (mutual defense) combined with asymmetric extraction (military burden on smaller states relative to size).
constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITED STATES STRATEGIC POWER (ROPE) — Primary beneficiary of institutionalized coalition formation. NATO commitment enables power projection globally, prevents Soviet bilateral domination of weaker states, and institutionalizes U.S. as coordinator of democratic alliance. Exit is available (unilateral withdrawal) but costly to global influence. Net experience is coordination benefit — the constraint solves the collective action problem of maintaining Western cohesion.
constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EUROPEAN DEFENSE INTEGRATION (SCAFFOLD) — Organized agents (France, nascent European defense frameworks) see NATO as temporary coordination structure with sunset clause: eventual European autonomy in defense is the intended trajectory. NATO enforces coordination during Cold War but is designed to transition as Europe consolidates. Low effective extraction because organized European actors perceive exit path through institutional evolution rather than withdrawal.
constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: NATO BUREAUCRATIC APPARATUS (PITON) — The organizational infrastructure maintains itself through institutional inertia. NATO command structures, planning cycles, and coordination rituals continue with high theater ratio (0.48 reflects significant performative activity in exercises, declarations, consensus-building meetings) alongside genuine deterrence function. The bureaucracy has partially become an end in itself rather than purely a means to collective defense. Exit costs are organizational rather than geopolitical — internal reform is difficult despite external structural shifts.
constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY (MOUNTAIN) — From a civilizational horizon, the bipolar nuclear standoff between superpowers creates an inherent structural requirement for coordinated deterrence among smaller allied nations. No alternative to collective security arrangements exists under the constraint of nuclear mutual vulnerability and power disparity. This perspective risks naturalizing the Soviet threat as inherent and unchangeable rather than as a contingent historical phenomenon. The engine's false summit detection will flag this as potential naturalization of a contingent geopolitical arrangement.
constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1980_carter_atlantic_alliance_collective_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1980_carter_atlantic_alliance_collective_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1980_carter_atlantic_alliance_collective_security, TR),
    TR >= 0.70.

:- end_tests(1980_carter_atlantic_alliance_collective_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from the U.S. through military deployment and resource commitment, from Europe through burden-sharing and alignment requirements, and from non-aligned nations through hegemonic pressure. However, the extraction is not severe (< 0.46) because genuine coordination benefits exist — collective security does solve the real problem of Soviet threat management more efficiently than bilateral arrangements would. The measurement trajectory shows extractiveness increasing from 0.28 to 0.38 over the interval, reflecting intensifying Cold War pressures and accumulating military commitments. Suppression (0.52): Moderate-high. NATO enforcement includes diplomatic coercion, military base requirements, ideological conformity pressure, and threat of abandonment for non-compliant members. Non-aligned nations face highest suppression (limited autonomy, proxy conflict risks). European members face moderate suppression (military requirements, alignment constraints). U.S. experiences suppression through institutional obligation and prestige commitment. Theater ratio (0.48): Moderate. NATO coordination includes genuine military planning and deterrence function alongside organizational ritual (NATO summits, consensus procedures, bureaucratic processes). The theater has increased over the interval as the alliance has shifted from immediate post-WWII coordination to institutionalized Cold War management. Some NATO activity (exercises, training, planning cycles) has dual function — both operationally necessary and performatively reassuring to alliance members. Claimed type (Tangled Rope): The constraint combines genuine collective security coordination (beneficiaries include all members who gain deterrence value) with asymmetric extraction (burden-sharing is asymmetric; non-aligned nations bear costs without benefits; U.S. uses alliance for global power projection beyond pure deterrence).
 *
 * PERSPECTIVAL GAP:
 *   The Atlantic Alliance demonstrates that a single constraint structure produces radically different classifications depending on the observer's structural position. U.S. institutional actors (arbitrage exit, beneficiary status) classify this as Rope — pure coordination solving the collective action problem. Western European members (constrained exit, dual beneficiary/victim status) classify as Tangled Rope — genuine benefits alongside real extraction. Non-aligned nations (trapped exit, victim-only status) classify as pure Snare — coercive constraint with no benefits. European defense integration advocates (organized, mobile exit, beneficiary status with planned exit) classify as Scaffold — temporary coordination with sunset. The NATO bureaucratic apparatus (institutional, arbitrage, increasingly self-referential) classifies as Piton — degrading institution maintained by inertia. The civilizational analytical observer risks misclassifying as Mountain — treating the bipolar nuclear structure as an immutable natural law requiring permanent collective security. This perspectival divergence is not a measurement problem but a structural feature: the constraint genuinely produces different extraction profiles for different agents, and different classifications reflect genuine differences in structural experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. institutional perspective derives d ≈ 0.10 from: beneficiary status (gains coordinated coalition for power projection) + arbitrage exit options (can withdraw but at cost to global influence) = low d, producing negative or near-zero chi (Rope perspective). Western European moderate perspective derives d ≈ 0.58 from: dual status (beneficiary of deterrence, victim of burden-sharing asymmetry) + constrained exit (can leave but at cost of security guarantee and geopolitical isolation) = moderate d, producing χ ≈ 0.55-0.70 (Tangled Rope). Non-aligned powerless perspective derives d ≈ 0.92 from: victim-only status (bears hegemonic pressure, gains no deterrence benefit) + trapped exit (geographically or politically unable to exit bipolar constraint) = high d, producing χ ≈ 0.88-1.20 (Snare, approaching boundary). The directionality overrides are not necessary here — the structural derivation captures the asymmetries accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   NATO's classification as Tangled Rope resolves the mandatrophy by establishing that the constraint must satisfy three conditions: (1) genuine coordination function exists (collective deterrence is more efficient than bilateral arrangements), (2) asymmetric extraction is documented (U.S. gains geopolitical advantages beyond military burden-sharing; Europe bears disproportionate burden relative to size; non-aligned nations bear costs without benefits), and (3) active enforcement is required (diplomatic pressure, military base requirements, alliance discipline mechanisms). All three conditions are met. The Tangled Rope classification prevents misclassification as pure Rope (which would erase the asymmetric extraction) or as pure Snare (which would erase the genuine deterrence coordination). The perspectival variance across six classifications reflects that the constraint's experienced nature depends structurally on the agent's position — this is not inconsistency but correct perspectival modeling of an inherently asymmetric institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soviet_expansion_threat_assessment,
    'Is Soviet expansion a genuine structural threat requiring collective deterrence, or a framing that justifies Western institutional consolidation?',
    'Historical analysis of Soviet military doctrine, expansion intentions vs. defensive positioning; counterfactual: would Soviet behavior differ significantly under alternative Western alliance structures?',
    'If genuine threat: NATO classification as Tangled Rope or Rope is justified. If contingent framing: classification shifts toward Snare or false-summit Mountain for non-members; Europe''s constraint becomes extraction disguised as security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_expansion_threat_assessment, conceptual, 'Whether Soviet expansion threat is objective or framing').

omega_variable(
    european_defense_autonomy_feasibility,
    'Can Western Europe develop autonomous defense capability sufficient to replace NATO commitment, or is European defense permanently dependent on U.S. nuclear guarantee?',
    'Assessment of European military capacity trajectory, nuclear weapons development, industrial capacity; comparison with Soviet threat evolution. Timeline for genuine European autonomy.',
    'If feasible: Scaffold classification is accurate and sunset is real (20-40 year horizon). If infeasible: European dependence becomes structural lock; Tangled Rope with no true exit option, downgrading exit_options to ''trapped'' for European members.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(european_defense_autonomy_feasibility, empirical, 'Whether European defense autonomy is structurally achievable').

omega_variable(
    collective_extraction_vs_collective_benefit,
    'Does NATO extract more value from non-aligned nations (through hegemonic alignment pressure, military base hosting, ideological conformity) than it provides through deterrence benefits?',
    'Accounting of direct costs to non-aligned states (military expenditure pressure, geopolitical constraints, policy restrictions) vs. benefits (deterrence value, protection from larger powers). Comparison with countries outside NATO sphere.',
    'If extraction > benefits: Snare classification confirmed for non-aligned agents; extractiveness should increase to 0.55+. If benefits > costs: classification shifts toward Rope; reveals coordination rather than extraction logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_extraction_vs_collective_benefit, empirical, 'Net extraction vs. benefit calculation for non-aligned nations').

omega_variable(
    institutional_inertia_vs_functional_necessity,
    'What proportion of NATO''s institutional activity (0.48 theater_ratio) is necessary for deterrence vs. organizational self-maintenance?',
    'Analysis of NATO planning cycles, exercises, meetings, bureaucratic processes; counterfactual: if theater ratio dropped to 0.25, would deterrence function suffer? Comparative analysis with other military alliance structures.',
    'If theater > 0.60: Piton classification dominates; NATO is degraded institution. If theater < 0.30: Tangled Rope confirmed; institutional activity is genuine coordination cost. Current 0.48 suggests hybrid — some functional necessity, some inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_functional_necessity, empirical, 'Proportion of NATO activity that is institutional inertia vs. functional necessity').

omega_variable(
    u_s_extraction_through_alliance_coordination,
    'Does the U.S. extract disproportionate geopolitical benefit from NATO coordination beyond the direct military burden-sharing calculus?',
    'Accounting of U.S. benefits: power projection infrastructure, allied compliance on Cold War policies, ideological leadership, strategic positioning. Comparison of U.S. influence with and without alliance structure.',
    'If U.S. gains exceed military costs: confirms beneficiary status and extraction from Europe (downward pressure on European perspective classification toward Snare). If symmetric: European Tangled Rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(u_s_extraction_through_alliance_coordination, empirical, 'Degree of U.S. geopolitical extraction beyond military costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1980_carter_atlantic_alliance_collective_security, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nato_cs_tr_t0, 1980_carter_atlantic_alliance_collective_security, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nato_cs_tr_t5, 1980_carter_atlantic_alliance_collective_security, theater_ratio, 5, 0.42).
narrative_ontology:measurement(nato_cs_tr_t10, 1980_carter_atlantic_alliance_collective_security, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(nato_cs_be_t0, 1980_carter_atlantic_alliance_collective_security, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(nato_cs_be_t5, 1980_carter_atlantic_alliance_collective_security, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(nato_cs_be_t10, 1980_carter_atlantic_alliance_collective_security, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1980_carter_atlantic_alliance_collective_security, enforcement_mechanism).
narrative_ontology:affects_constraint(1980_carter_atlantic_alliance_collective_security, soviet_military_doctrine_deterrence).
narrative_ontology:affects_constraint(1980_carter_atlantic_alliance_collective_security, european_political_integration).
narrative_ontology:affects_constraint(1980_carter_atlantic_alliance_collective_security, global_american_hegemony).

% DUAL FORMULATION NOTE:
% NATO Atlantic Alliance is upstream of multiple structural constraints: (1) Soviet military posture adjusts to NATO capabilities (upstream of soviet_military_doctrine_deterrence), (2) European integration is both shaped by and shapes NATO (bidirectional with european_political_integration), (3) global American hegemony depends on NATO institutionalization (upstream of global_american_hegemony). Each linked constraint has distinct epsilon value reflecting different observable and structural question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1980_carter_atlantic_alliance_collective_security, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
