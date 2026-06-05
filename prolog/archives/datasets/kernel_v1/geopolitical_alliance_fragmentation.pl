% ============================================================================
% CONSTRAINT STORY: geopolitical_alliance_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_alliance_fragmentation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geopolitical_alliance_fragmentation
 *   human_readable: Geopolitical Alliance Fragmentation and Member State Extraction
 *   domain: international_relations/strategic_alliances
 *
 * SUMMARY:
 *   Geopolitical alliance fragmentation emerges as a structural constraint
 *   wherever a hegemonic power maintains military and economic dominance
 *   through a network of subordinate allies while those allies simultaneously
 *   face genuine security threats that prevent unilateral exit. This
 *   constraint exhibits the full typology of DR classifications: the
 *   hegemonic core experiences pure coordination (rope), vulnerable member
 *   states experience extraction traps (snare), mid-tier states negotiate
 *   within constrained coalitions (tangled rope), autonomous defense
 *   initiatives provide temporary scaffolding (scaffold), formal alliance
 *   institutions persist through inertial performance rather than function
 *   (piton), and civilizational analysis risks naturalizing constructed
 *   hierarchy as immutable geopolitical law (mountain/false summit). The
 *   constraint's theater_ratio trajectory (0.48 → 0.68) reflects how alliance
 *   institutions have become increasingly performative as technological
 *   change, burden-sharing disputes, and strategic divergence have eroded
 *   functional integration. Member states conduct regular exercises, maintain
 *   integrated command structures, and perform collective defense postures
 *   primarily to signal commitment and maintain the legitimacy fiction of the
 *   alliance, while actual warfighting capacity depends on the hegemonic
 *   power's nuclear guarantee and global force projection. The extractiveness
 *   trajectory (0.38 → 0.52) models how burden-sharing demands have
 *   intensified as the hegemonic power faces fiscal pressure and seeks to
 *   distribute defense costs more heavily to allies, while suppression (0.42
 *   → 0.58) reflects how the structural necessity of the alliance (created by
 *   the security dilemma and external threat) becomes a mechanism for
 *   enforcing subordinate compliance on non-defense issues.
 *
 * KEY AGENTS:
 *   - Hegemonic Power: Primary beneficiary (institutional/arbitrage) — captures geopolitical leverage, extracts defense burden-sharing, maintains network of subordinate states; can realign, threaten, or withdraw without existential consequence
 *   - Vulnerable Member States: Primary victims (powerless/trapped) — face existential security threats that cannot be unilaterally managed; exit from alliance entails catastrophic military vulnerability; experience maximum extraction of resources and policy subordination
 *   - Mid-Tier Member States: Secondary actors (organized/constrained) — possess regional power but global vulnerability; can coordinate for collective bargaining but face significant strategic costs of alliance exit; experience mixed coordination and extraction
 *   - Autonomous Defense Coalition: Organized actors (organized/constrained) — explicitly building alternative defense structures as transition pathway; experience low effective extraction due to agency and stated sunset timeline
 *   - Alliance Institutional Apparatus: Institutional actor (institutional/arbitrage) — formal structures, command hierarchy, integrated planning persist through inertia; increasingly performative as functional capacity erodes; maintains legitimacy through regular exercises and signaling
 *   - Collective Defense Credibility: Victim (powerless/trapped) — abstract structural good that cannot organize or advocate; vulnerable to degradation as alliance institutions become performative; bears full cost of false deterrence signaling
 *   - External Adversary Coalition: Structural antagonist (institutional/arbitrage) — provides the security justification for alliance necessity; benefit flows from member state suppression and resource extraction toward alliance institutional maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_alliance_fragmentation, 0.52).
domain_priors:suppression_score(geopolitical_alliance_fragmentation, 0.58).
domain_priors:theater_ratio(geopolitical_alliance_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_alliance_fragmentation, extractiveness, 0.52).
narrative_ontology:constraint_metric(geopolitical_alliance_fragmentation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geopolitical_alliance_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_alliance_fragmentation, tangled_rope).
narrative_ontology:human_readable(geopolitical_alliance_fragmentation, "Geopolitical Alliance Fragmentation and Member State Extraction").
narrative_ontology:topic_domain(geopolitical_alliance_fragmentation, "international_relations/strategic_alliances").

domain_priors:requires_active_enforcement(geopolitical_alliance_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_alliance_fragmentation, hegemonic_power).
narrative_ontology:constraint_beneficiary(geopolitical_alliance_fragmentation, alliance_institutional_apparatus).
narrative_ontology:constraint_victim(geopolitical_alliance_fragmentation, junior_member_states).
narrative_ontology:constraint_victim(geopolitical_alliance_fragmentation, collective_defense_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE MEMBER STATE (SNARE) — Small or geographically exposed states face existential security threats that cannot be unilaterally managed. Alliance membership is materially necessary, not voluntary. Exit is impossible without accepting catastrophic military vulnerability. This agent experiences maximum extraction: contribution to alliance infrastructure, deference to hegemonic preferences on foreign policy, subordination of economic interests, and security guarantee that can be withdrawn if political alignment drifts.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: MID-TIER MEMBER STATE COALITION (TANGLED ROPE) — Mid-sized states with regional power but global vulnerability can coordinate among themselves for collective bargaining, but exit from the alliance entails significant strategic costs. They experience genuine coordination benefits (integrated air defense, coordinated procurement) alongside extraction (mandatory defense spending, constraint on autonomous foreign policy, subordination to hegemonic deterrence strategy). Organization enables them to negotiate, but does not eliminate the asymmetry.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: HEGEMONIC POWER (ROPE) — The alliance's primary beneficiary experiences the constraint as pure coordination: maintaining a network of subordinate states, extracting defense burden-sharing, securing geopolitical leverage, and projecting military power globally. The hegemonic power has full arbitrage capacity — it can realign allies, adjust burden-sharing, or threaten withdrawal without existential consequence. The constraint solves coordination problems that directly serve its interests. Effective extraction toward this agent is negative (the constraint subsidizes its strategic position).
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AUTONOMOUS DEFENSE COALITION (SCAFFOLD) — Organized efforts by member states to build autonomous defense capacity (European Union Defense Fund, independent air defense networks, coordinated procurement) represent a temporary scaffolding mechanism. These coalitions see the alliance as a transition state: reducing dependency on the hegemonic power's deterrence guarantee while maintaining alliance membership. The sunset is explicit — as autonomous capacity matures, alliance extraction mechanisms lose force. Low effective extraction because the coalition has agency, explicitly states a timeline, and is building alternatives.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ALLIANCE INSTITUTIONAL APPARATUS (PITON) — The alliance's formal institutions (command structure, integrated headquarters, treaty obligations, mutual defense clauses) persist through institutional inertia despite eroding functional capacity. Deterrence strategy relies increasingly on threat credibility rather than functional integration. Regular exercises and meetings are largely performative — demonstrating commitment and maintaining institutional legitimacy rather than genuinely testing or improving coordinated response capacity. Theater ratio (0.68) reflects the ratio of performative signaling to functional warfighting capability.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of geopolitical alliance is an immutable feature of international relations: the anarchic structure of the state system creates security dilemmas that cannot be unilaterally solved. Burden-sharing, subordination of interests, and asymmetric defense arrangements are inherent to how states balance power. This perspective risks naturalizing what is actually a contingent institutional arrangement shaped by post-WWII hegemonic order design. The engine's false summit detector will identify this as naturalization of constructed subordination.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_alliance_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_alliance_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_alliance_fragmentation, TR),
    TR >= 0.70.

:- end_tests(geopolitical_alliance_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, accurately reflecting the mixed coordination-extraction hybrid. The hegemonic power extracts genuine benefits from the alliance structure (geopolitical leverage, defense burden-sharing, global force projection basing), but also provides real coordination services (collective deterrence, integrated defense planning, security guarantee). The extraction is not maximal because member states retain some agency through coalition formation and some genuine benefits from coordinated defense. The trajectory from 0.38 to 0.52 reflects increasing burden-sharing demands and policy subordination as the hegemonic power seeks to distribute costs while maintaining control. Suppression (0.58): Moderate-high. Member states face significant barriers to alliance exit: immediate military vulnerability, loss of security guarantees, potential economic sanctions, and diplomatic isolation. However, suppression is not total (≥0.60 would trigger snare gate) because member states retain partial agency through coalition formation, autonomous defense initiatives, and the fact that exit, while costly, is not physically impossible. The trajectory from 0.42 to 0.58 models how external threat perception and internal burden-sharing disputes increase the pressure on members to remain subordinate. Theater ratio (0.68): High and rising. Alliance institutions have become increasingly performative as technological change, diverging threat perceptions, and strategic doctrinal differences have reduced functional integration. Command exercises demonstrate readiness rhetorically; weapons standardization is partial and aspirational; rapid-response protocols exist on paper but coordination capacity is untested. The rise from 0.48 to 0.68 reflects how alliance institutions must work harder to maintain legitimacy as their functional capacity faces erosion from strategic divergence and burden-sharing disputes. The alliance has shifted from functional coordination toward theater: signaling commitment, demonstrating capability (symbolically rather than operationally), and maintaining the fiction of collective defense.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates perspectival divergence across all six types, demonstrating how the same structural phenomenon appears radically differently depending on the observer's structural position. The hegemonic power sees coordination and benefit (rope); vulnerable member states see extraction and entrapment (snare); mid-tier states see mixed coordination and constraint (tangled rope); autonomous defense coalitions see temporary scaffolding with a sunset (scaffold); alliance institutions see their own degraded function persisting through inertia (piton); and civilizational analysis risks naturalizing constructed hierarchy as immutable law (mountain/false summit). The perspectival gap is both structural and interpretive: the structural gap reflects real differences in exit options (the hegemonic power can realign; vulnerable states cannot exit), while the interpretive gap reflects different frames for understanding necessity. The hegemonic power frames alliance subordination as mutual benefit; vulnerable states experience it as coerced burden-sharing; autonomous defense coalitions frame it as transitional; institutions frame it as structural necessity; and civilizational analysis frames it as natural law. These frames are not equally defensible — the structural data shows extraction flowing toward the hegemonic power and suppression falling on vulnerable states — but they are all coherent readings of the same constraint from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The automatic directionality derivation produces the correct d values for each perspective. The hegemonic power as beneficiary with arbitrage exit options derives d ≈ 0.05-0.15 (full beneficiary), producing f(d) ≈ -0.12 to 0.02 and effective extraction χ toward this agent that is negative or minimal. Vulnerable member states as victims with trapped exit options derive d ≈ 0.95 (full target), producing f(d) ≈ 1.42 and maximum experienced extraction. Mid-tier states as partial victims with constrained exit derive d ≈ 0.65-0.75, producing f(d) ≈ 1.00-1.15 and significant but not maximum extraction. The analytical observer derives d ≈ 0.73 (slightly target-oriented from global scope and civilizational time horizon), producing f(d) ≈ 1.15. These derivations accurately capture the structural asymmetry without requiring overrides. No directionality overrides are necessary because the beneficiary and victim declarations map cleanly to the observable structural relationships. The hegemonic power genuinely benefits (arbitrage exit, low d); vulnerable states genuinely suffer (trapped exit, high d). The direction of extraction is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how mandatrophy is resolved through perspectival multiplicity and structural honesty. The constraint is not 'which type is correct?' but 'what is each actor's genuine experience?' The hegemonic power genuinely experiences coordination (rope); vulnerable member states genuinely experience extraction (snare); mid-tier states genuinely experience mixed coordination and extraction (tangled rope). These are not contradictory — they are the same structural constraint viewed from different positions with different exit options and extraction directions. The mandatrophy resolves by acknowledging that the constraint simultaneously IS all these types depending on who measures it. The analytical observer's mountain classification is the one that requires scrutiny — it naturalizes what is actually contingent institutional design as immutable law, making the false summit detector essential for preventing the 'inevitability' frame from obscuring the constructed nature of the subordination. The constraint's trajectory (extractiveness rising, theater rising, suppression rising) shows the alliance moving toward snare-like characteristics as functional capacity erodes and extraction intensifies, while the autonomous-defense scaffold represents a real exit pathway that could eventually resolve the constraint by replacing hegemonic coordination with truly autonomous member-state coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hegemonic_withdrawal_credibility,
    'How credible is the hegemonic power''s threat to withdraw security guarantees if member states fail to comply with political demands?',
    'Historical analysis of alliance restructuring episodes; examination of cases where withdrawal was threatened vs. executed; assessment of reputational costs to hegemonic deterrence if withdrawal occurs',
    'If highly credible: suppression is accurately measured (0.58); member states face genuine existential risk. If low credibility: suppression is overstated; member states have more exit capacity than measured; reclassify from snare/trapped to tangled_rope/constrained for vulnerable states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_withdrawal_credibility, empirical, 'Credibility of hegemonic withdrawal threat as enforcement mechanism').

omega_variable(
    autonomous_defense_viability_timeline,
    'What is the realistic timeline for member state coalitions to achieve sufficient autonomous defense capacity to exit the alliance without catastrophic military vulnerability?',
    'Technical assessment of autonomous air defense, early warning, and rapid-response capabilities; comparison of projected vs. required capability timelines; stress-testing of autonomous deterrence against peer adversaries',
    'If achievable within 15 years: scaffold classification is accurate and sunset is structural. If 50+ years: scaffold is aspirational rather than real, and the structure is permanently tangled_rope. If asymptotically impossible: member states are structurally trapped, and the snare classification for vulnerable states is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_defense_viability_timeline, empirical, 'Timeline for achieving autonomous defense viability').

omega_variable(
    alliance_institutional_functional_degradation,
    'What proportion of alliance institutional activities are genuinely functional (capable of conducting deterrence operations) vs. performative (signaling commitment and maintaining legitimacy)?',
    'Assessment of command readiness, exercise outcomes, integration fidelity, and actual deployment capacity; comparison of peacetime institutional activity vs. wartime operational necessity',
    'If theater_ratio accurately reflects functional capacity: piton classification is correct. If theater is much higher (>0.85): alliance institutions are nearly pure performance and may be reclassified toward the piton floor (ε ≤ 0.20). If theater is lower (<0.55): institutions retain sufficient function to upgrade from piton to rope or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_institutional_functional_degradation, empirical, 'Functional vs. performative content of alliance institutional activities').

omega_variable(
    member_state_policy_subordination_magnitude,
    'To what degree do member states subordinate autonomous foreign policy preferences to alliance coordination demands, and how much of this subordination is coerced vs. voluntary?',
    'Analysis of voting patterns in alliance councils; case studies of member state policy reversals on non-defense issues (trade, immigration, human rights) following hegemonic pressure; survey data on elite perception of constraint vs. choice',
    'If coercion is dominant: extraction component (ε) is understated; reclassify toward snare. If voluntary coordination is dominant: extraction is overstated; reclassify toward rope. If mixed: current tangled_rope classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_policy_subordination_magnitude, empirical, 'Magnitude of policy subordination and coercion mechanism').

omega_variable(
    false_summit_natural_law_naturalization,
    'Is the ''inevitability'' of geopolitical alliance formation a genuine feature of anarchic international structure, or a contingent arrangement that reflects post-WWII power distribution and hegemonic institutional design?',
    'Historical comparison with pre-WWII alliance patterns; examination of periods of genuine great-power coordination without subordination structures; alternative institutional designs that achieve burden-sharing without hegemonic extraction',
    'If genuine natural law: mountain classification is correct; beneficiary declarations trigger FSM but ultimate classification remains mountain. If contingent: FSM fires; engine reclassifies to tangled_rope; the ''naturalness'' framing becomes visible as a cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_naturalization, conceptual, 'Whether alliance necessity is natural law or contingent institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_alliance_fragmentation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geoal_tr_t0, geopolitical_alliance_fragmentation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(geoal_tr_t15, geopolitical_alliance_fragmentation, theater_ratio, 15, 0.62).
narrative_ontology:measurement(geoal_tr_t30, geopolitical_alliance_fragmentation, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(geoal_be_t0, geopolitical_alliance_fragmentation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(geoal_be_t15, geopolitical_alliance_fragmentation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(geoal_be_t30, geopolitical_alliance_fragmentation, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(geoal_su_t0, geopolitical_alliance_fragmentation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(geoal_su_t15, geopolitical_alliance_fragmentation, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(geoal_su_t30, geopolitical_alliance_fragmentation, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_alliance_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(geopolitical_alliance_fragmentation, nato_burden_sharing_dispute).
narrative_ontology:affects_constraint(geopolitical_alliance_fragmentation, european_strategic_autonomy).
narrative_ontology:affects_constraint(geopolitical_alliance_fragmentation, hegemonic_alliance_credibility).

% DUAL FORMULATION NOTE:
% Geopolitical alliance fragmentation is a parent constraint that affects multiple downstream constraints: burden-sharing disputes (ε≈0.45, tangled_rope) represent the extraction mechanism becoming explicit; European strategic autonomy initiatives (ε≈0.35, scaffold) represent the autonomous defense exit pathway; hegemonic alliance credibility (ε≈0.62, snare) represents the deterrence guarantee's degradation as theater increases. These three constraint stories decompose different functional aspects of the parent constraint and link back via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
