% ============================================================================
% CONSTRAINT STORY: iranian_expansion_regional
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iranian_expansion_regional, []).

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
 *   constraint_id: iranian_expansion_regional
 *   human_readable: Iranian Regional Expansion Through Proxy Networks and Militia Control
 *   domain: geopolitical/military/regional_power
 *
 * SUMMARY:
 *   Iranian regional expansion via proxy militia networks represents a
 *   structural constraint on Middle Eastern state sovereignty, population
 *   autonomy, and regional balance. The mechanism operates through support
 *   and control of non-state armed groups (Popular Mobilization Forces in
 *   Iraq, Hezbollah in Lebanon, Houthis in Yemen, smaller groups in Syria and
 *   Palestine) that function simultaneously as legitimate actors in local
 *   political contexts and as instruments of Iranian state power projection.
 *   This creates a hybrid coordination-extraction dynamic: genuine deterrent
 *   coordination against perceived threats to Shia interests and Iranian
 *   security coexists with asymmetric extraction of local governance
 *   capacity, resource flows, and state sovereignty. The extractiveness has
 *   increased over the measurement interval (0.35 → 0.62) as proxy networks
 *   consolidated institutional control, deepened resource dependence, and
 *   expanded cross-border operational integration. Theater ratio (0.55)
 *   reflects moderate performative content: Iranian narratives emphasize
 *   resistance to imperialism and regional self-determination, while actual
 *   mechanisms involve coercive control, resource monopolization, and militia
 *   autonomy suppression. The constraint exhibits all characteristics of
 *   Tangled Rope: (1) genuine coordination function (threat deterrence, Shia
 *   power projection, anti-status-quo coalition building), (2) asymmetric
 *   extraction (local populations and neighboring states bear costs), (3)
 *   active enforcement through militia violence and coercion, (4)
 *   beneficiaries and victims clearly differentiated by structural position.
 *
 * KEY AGENTS:
 *   - Iranian State Apparatus: Primary beneficiary (institutional/arbitrage) — projects regional power, achieves strategic objectives at lower cost than conventional military, bypasses military disadvantage vs. Gulf states and Israel
 *   - Shia Militia Networks: Secondary beneficiary and partially trapped agent (organized/constrained) — gain Iranian military support and legitimacy but constrained by operational control and strategic direction from Tehran
 *   - Local Populations (Iraq, Syria, Lebanon): Primary victims (powerless/trapped) — experience militia occupation, economic disruption, sovereignty loss, violence, with geographic and economic barriers to exit
 *   - Neighboring States (Saudi Arabia, Israel, UAE, Jordan): Secondary victims (powerful/constrained) — face military costs, loss of regional influence, proxy warfare, but retain diplomatic and military response options
 *   - International Sanctions Regime: Organized constraint (organized/constrained) — attempts to limit Iranian expansion through time-bound pressure; designed with explicit sunset logic tied to compliance
 *   - Regional Balance-of-Power Framework: Institutional inertia actor (institutional/arbitrage) — Cold War-era patronage logic persists through institutional tradition despite functional degradation
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees authentic hybrid structure requiring Tangled Rope classification rather than pure extraction or coordination narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iranian_expansion_regional, 0.58).
domain_priors:suppression_score(iranian_expansion_regional, 0.72).
domain_priors:theater_ratio(iranian_expansion_regional, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iranian_expansion_regional, extractiveness, 0.58).
narrative_ontology:constraint_metric(iranian_expansion_regional, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(iranian_expansion_regional, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iranian_expansion_regional, tangled_rope).
narrative_ontology:human_readable(iranian_expansion_regional, "Iranian Regional Expansion Through Proxy Networks and Militia Control").
narrative_ontology:topic_domain(iranian_expansion_regional, "geopolitical/military/regional_power").

domain_priors:requires_active_enforcement(iranian_expansion_regional).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iranian_expansion_regional, iranian_state_apparatus).
narrative_ontology:constraint_beneficiary(iranian_expansion_regional, shia_militia_networks).
narrative_ontology:constraint_beneficiary(iranian_expansion_regional, regional_iranian_allies).
narrative_ontology:constraint_victim(iranian_expansion_regional, regional_state_sovereignty).
narrative_ontology:constraint_victim(iranian_expansion_regional, sunni_populations).
narrative_ontology:constraint_victim(iranian_expansion_regional, local_governance_capacity).
narrative_ontology:constraint_victim(iranian_expansion_regional, neighboring_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED LOCAL POPULATIONS (SNARE) — Communities in Iraq, Syria, and Lebanon experience Iranian proxy militia control as an inescapable extraction mechanism. Local populations bear the cost of militia presence (violence, economic disruption, loss of sovereignty) with no meaningful exit: geographic immobility, economic dependence, and coercive militia presence prevent departure. Suppression is structural and nearly total.
constraint_indexing:constraint_classification(iranian_expansion_regional, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NEIGHBORING STATES (TANGLED ROPE) — States like Saudi Arabia, Israel, UAE, and Jordan experience Iranian expansion as both a coordination problem (managing shared security interests, preventing destabilization) and extraction (loss of regional influence, military expenditure burden, proxy warfare costs). High suppression but not total — these states have military and diplomatic options, but at significant cost. The constraint exhibits hybrid coordination (common threat management) with asymmetric extraction (Iran's costs lower than neighbors').
constraint_indexing:constraint_classification(iranian_expansion_regional, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IRANIAN STATE APPARATUS (ROPE) — Iran experiences the expansion mechanism as a coordination tool: proxy networks extend state power, project influence, bypass military disadvantage vs. conventional competitors, and achieve strategic objectives at lower cost than direct military engagement. For Iran, the constraint is primarily functional—it solves genuine coordination problems (deterrence, regional positioning) with high benefit. The state has leverage and arbitrage options (can escalate, de-escalate, or shift proxy composition).
constraint_indexing:constraint_classification(iranian_expansion_regional, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SHIA MILITIA NETWORKS (TANGLED ROPE) — Proxy militias (Popular Mobilization Forces in Iraq, Hezbollah in Lebanon, Houthi forces in Yemen) experience Iranian control as mixed: genuine coordination of shared ideological and strategic interests (Shia regional power) with asymmetric extraction (operational autonomy constrained by Iranian directives, resource flows controlled by Tehran). Militias benefit from Iranian military support and legitimacy but also bear the cost of being instruments of Iranian strategy. Constrained exit — leaving Iranian patronage networks entails loss of resources and protection.
constraint_indexing:constraint_classification(iranian_expansion_regional, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL CONSTRAINTS & SANCTIONS REGIME (SCAFFOLD) — International sanctions (JCPOA framework, secondary sanctions, proxy-specific designations) and diplomatic pressure represent a temporary coordination mechanism with designed sunset. The international system aims to constrain Iranian expansion through time-limited pressure (sanctions relief tied to compliance). Theater ratio moderate because sanctions are partially performative (easily circumvented) but also structurally binding. The sunset logic is explicit: compliance leads to sanctions relief and reintegration into international system.
constraint_indexing:constraint_classification(iranian_expansion_regional, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL BALANCE-OF-POWER NORMS (PITON) — The historical framework of maintaining regional balance through great power patronage networks and proxy forces is substantially degraded yet institutionally persistent. This mechanism persists through Cold War-era inertia even though its functional purpose has eroded: the multipolar regional system of the 1980s-90s has become more complex, state capacity has fragmented, and proxy networks have acquired semi-independent agency. The theater ratio reflects that maintaining 'balance' narratives requires increasing performative effort as the actual mechanism becomes increasingly divorced from stated objectives.
constraint_indexing:constraint_classification(iranian_expansion_regional, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a full structural analysis, Iranian regional expansion exhibits both genuine coordination (deterrence against military threats, regional power projection) and asymmetric extraction (control over local populations, resource flows, sovereignty violations). The constraint is neither pure extraction nor pure coordination but an authentic hybrid. Extraction flows primarily to the Iranian state apparatus and occurs largely via suppression of local agency. Coordination benefits accrue to anti-status quo regional actors (Shia-aligned populations, Iran-aligned states) and involve non-trivial threat management among regional powers.
constraint_indexing:constraint_classification(iranian_expansion_regional, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iranian_expansion_regional_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iranian_expansion_regional, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iranian_expansion_regional, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iranian_expansion_regional, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iranian_expansion_regional, TR),
    TR >= 0.70.

:- end_tests(iranian_expansion_regional_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Iranian expansion extracts tangible benefits: governance capacity (local militias subordinated to Tehran's strategic direction), resource flows (petrodollars subsidizing proxy forces), sovereignty (foreign military presence in Iraq, Syria, Lebanon), and population autonomy (militia control over daily life in captured regions). However, extractiveness is not at snare levels (0.66+) because: (1) genuine coordination function exists (deterrence against real military threats), (2) some local populations benefit from militia provision of security and services, (3) Iran itself bears costs (military spending, sanctions consequences, vulnerability to retaliation). The measurement trajectory shows increasing extraction over time as proxy networks consolidated control: early phases (t=0, ε=0.35) involved more transactional relationships; mature phases (t=24, ε=0.62) involve entrenched institutional subordination. Suppression (0.72): High. Barriers to local population exit are severe: geographic immobility (displacement is dangerous), economic dependence (militia-controlled markets and employment), coercive militia presence (violence against dissidents), and absence of alternative governance. Suppression is not quite at snare floor (0.80+) because some local actors maintain limited agency: militia recruitment remains partly voluntary, some economic activity persists outside militia control, and neighboring states retain diplomatic/military options. Theater ratio (0.55): Moderate. The constraint's performative content includes narrative claims about 'resistance to imperialism,' 'Shia self-determination,' and 'regional liberation,' which frame coercive control as liberation. However, the functional mechanism (military force, resource control, elite capture) is transparent enough that theater doesn't reach degraded piton levels (0.70+). The narratives are deployed but remain contested — neighboring states and Western observers openly frame the mechanism as expansion rather than liberation.
 *
 * PERSPECTIVAL GAP:
 *   The Iranian expansion constraint demonstrates perspectival divergence precisely because different agents occupy different structural positions relative to extraction flows. The Iranian state sees the mechanism as solving real security and power problems (Rope classification). Local populations see the same mechanism as pure extraction with suppression (Snare). Neighboring states see a mixed problem requiring both defensive coordination and response to extraction (Tangled Rope). The gap is not reducible to disagreement about values — it reflects genuine structural difference in who benefits and who bears costs. This is the diagnostic signature of Tangled Rope: multiple legitimate classifications from different positions, indicating authentic hybridity rather than mislabeling.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (Iranian state apparatus) has arbitrage options — can escalate, de-escalate, shift proxy composition, or negotiate constraints. This produces low directionality (d ≈ 0.08 → f(d) ≈ -0.11), meaning Iran experiences the constraint as net beneficial coordination rather than extraction. Trapped local populations have no exit, making them the primary target for extraction (d ≈ 0.92 → f(d) ≈ 1.38), producing maximum experienced extractiveness. Militia networks occupy an intermediate position: benefiting from Iranian patronage but constrained in autonomy, producing d ≈ 0.40 and mixed perspective (some see Rope, some see Tangled Rope depending on measurement of autonomy degree — captured in the proxy_autonomy_threshold omega). Neighboring powerful states have constrained but non-zero options (military response, sanctions coordination, alliance building), producing d ≈ 0.58 and Tangled Rope classification. This differentiation explains why perspectives produce different types despite shared base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint classifies as Tangled Rope, preventing false reduction to either pure extraction (Snare) or pure coordination (Rope). The mandatrophy resolves by recognizing that the same mechanism serves both functions: Iranian deterrence and power projection (genuine coordination problem) is inseparable from extraction of local governance capacity and population autonomy (genuine asymmetric extraction). The false alternatives are: (1) claiming it is pure coordination (ignores suppression, sovereignty violation, resource extraction), (2) claiming it is pure extraction (ignores genuine deterrent function, Shia benefit from regional power, real threat environment). Tangled Rope correctly captures that the coordination function is not justification for the extraction — it is institutional entanglement that makes them inseparable. The mandatrophy dissolves when the analyst recognizes that 'but it also coordinates deterrence' does not invalidate the extraction harm, and 'but it extracts governance' does not negate the coordination benefit. Both are structurally true. This is why Tangled Rope exists: to classify exactly this case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_autonomy_threshold,
    'At what point does proxy force autonomy (financing, targeting decisions, resource allocation) constitute genuine coalition vs. pure Iranian instrument?',
    'Longitudinal analysis of proxy force decision-making independence: frequency of deviation from Iranian directives, resource flow tracing (Iranian subsidy percentage vs. autonomous income), targeting pattern correlation with Iranian strategic interests vs. local actor interests',
    'If high autonomy (>40% decisions independent): reclassify militia perspectives toward Rope (coalition members). If low autonomy (<20% independence): reclassify toward Snare (pure instruments). Affects whether ''shia_militia_networks'' should appear as beneficiaries or victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_autonomy_threshold, empirical, 'Degree of proxy autonomy determining coalition vs. instrument status').

omega_variable(
    collateral_coordination_function,
    'Does Iranian proxy network expansion provide genuine coordination benefits to regional Shia communities (security, representation, resource access) or is the ''coordination'' narrative purely a cover for extraction?',
    'Comparative analysis of Shia population welfare metrics in proxy-controlled vs. non-proxy-controlled regions; measurement of resource flows to local populations vs. Iran apparatus; survey data on perceived agency and benefit among populations',
    'If genuine coordination (tangible welfare benefits, participatory governance): classification remains Tangled Rope. If purely extractive (welfare declines, autonomy suppressed, resource flows to Tehran): reclassify to Snare from broader regional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collateral_coordination_function, empirical, 'Whether proxy networks provide genuine benefits to populations or solely serve extraction').

omega_variable(
    sanctions_effectiveness_terminus,
    'Do sanctions regimes demonstrate real capacity to constrain Iranian expansion, or is the ''sunset'' logic merely aspirational?',
    'Measurement of proxy network expansion rate pre- vs. post-sanctions; correlation between sanctions intensity and Iranian military spending on proxies; test whether sanctions relief (JCPOA period) actually reduced proxy operations',
    'If sanctions effective (documented constraint of proxy spending): Scaffold classification confirmed. If sanctions ineffective (expansion continues regardless): Scaffold reclassifies to Piton (performative constraint maintained through inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctions_effectiveness_terminus, empirical, 'Effectiveness of sanctions in constraining Iranian proxy expansion').

omega_variable(
    regional_state_coalition_feasibility,
    'Can neighboring states coordinate effective counter-constraint (military alliance, unified sanctions policy, intelligence sharing) or is regional fragmentation structural?',
    'Analysis of historical coalition attempts (Gulf Cooperation Council, Abraham Accords, US-led coalitions); measurement of policy coordination frequency, failure rates, sustainability; mapping of conflicting regional interests that prevent unified response',
    'If coalition feasible (demonstrated coordination capacity): neighboring states move from Tangled Rope toward Rope, suggesting Iranian expansion may face organized collective resistance. If structural fragmentation: states remain Tangled Rope (constrained), and Iranian advantage persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_state_coalition_feasibility, empirical, 'Capacity of regional states to form unified counter-coalition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iranian_expansion_regional, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_exp_tr_t0, iranian_expansion_regional, theater_ratio, 0, 0.38).
narrative_ontology:measurement(iran_exp_tr_t8, iranian_expansion_regional, theater_ratio, 8, 0.47).
narrative_ontology:measurement(iran_exp_tr_t16, iranian_expansion_regional, theater_ratio, 16, 0.55).
narrative_ontology:measurement(iran_exp_tr_t24, iranian_expansion_regional, theater_ratio, 24, 0.63).

% Extraction over time
narrative_ontology:measurement(iran_exp_be_t0, iranian_expansion_regional, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iran_exp_be_t8, iranian_expansion_regional, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(iran_exp_be_t16, iranian_expansion_regional, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(iran_exp_be_t24, iranian_expansion_regional, base_extractiveness, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iranian_expansion_regional, enforcement_mechanism).
narrative_ontology:affects_constraint(iranian_expansion_regional, gulf_state_military_spending).
narrative_ontology:affects_constraint(iranian_expansion_regional, lebanese_state_capacity_collapse).
narrative_ontology:affects_constraint(iranian_expansion_regional, iraqi_sovereignty_fragmentation).
narrative_ontology:affects_constraint(iranian_expansion_regional, yemeni_conflict_entanglement).
narrative_ontology:affects_constraint(iranian_expansion_regional, syrian_reconstruction_dependency).

% DUAL FORMULATION NOTE:
% Iranian expansion is upstream of regional state capacity degradation. Proxy networks cause downstream constraints: Gulf states must increase military spending, Lebanon loses governance capacity, Iraq's sovereignty fragments, Yemen's conflict deepens, Syria becomes dependent on external patronage. Each downstream constraint has its own ε value reflecting how much extraction flows through that mechanism. The family shares the beneficiary (Iranian state apparatus) but distributes victims across multiple regional populations and states.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iranian_expansion_regional, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
