% ============================================================================
% CONSTRAINT STORY: saudi_iran_rapprochement_2023
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_saudi_iran_rapprochement_2023, []).

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
 *   constraint_id: saudi_iran_rapprochement_2023
 *   human_readable: Saudi-Iran Rapprochement as Constraint Structure (2023)
 *   domain: geopolitical/regional_state_relations
 *
 * SUMMARY:
 *   The March 2023 Chinese-brokered rapprochement between Saudi Arabia and
 *   Iran represents a major structural shift in Middle Eastern geopolitics,
 *   but the underlying constraint is ambiguous — it simultaneously functions
 *   as coordination mechanism, state-level extraction apparatus, and degraded
 *   institutional order. From Saudi and Iranian perspectives, the
 *   rapprochement appears as a Rope coordination solution to a 40-year proxy
 *   conflict: both states gain by reducing military spending, normalizing
 *   commerce, and ending zero-sum positioning. From the perspective of proxy
 *   militias, smaller Gulf states, and internal opposition factions, the
 *   rapprochement appears as a Snare: they are trapped by state-level
 *   decisions made without their input, and their exit options are
 *   eliminated. From the perspective of the US-led alliance system, the
 *   rapprochement appears as a Piton: the post-Cold War security architecture
 *   persists through inertia but its primary organizing logic (containing
 *   Iran) is degraded. From a civilizational analytical view, great-power
 *   competition appears as an immutable law of nature, but this naturalizes
 *   what is actually contingent on Chinese mediation capacity, petrodollar
 *   flows, and internal factional politics. The constraint exhibits high
 *   extractiveness (0.52) because state-level beneficiaries are imposing
 *   constraints on lower-power actors without compensation. Theater ratio
 *   (0.58) reflects that the rapprochement involves substantial diplomatic
 *   ceremony and performative confidence-building measures, but underlying
 *   security gains and reduced proxy activity are structurally real.
 *
 * KEY AGENTS:
 *   - Saudi Arabia (State Actor): Primary institutional beneficiary (institutional/arbitrage) — captures benefits of reduced regional spending and normalized trade; experiences rapprochement as coordination solution to proxy war problem
 *   - Iran (State Actor): Co-primary institutional beneficiary (institutional/arbitrage) — gains regional legitimacy, sanctions relief pathways, reduced military spending; arbitrage capacity to return to escalation if needed
 *   - China (Mediator/Beneficiary): Tertiary institutional beneficiary (institutional/arbitrage) — mediator role expands geopolitical influence and positions China as alternative arbiter to US-led order
 *   - Houthi Militias and Iraqi PMU (Proxy Networks): Primary victims (powerless/trapped) — structurally subordinated to state-level decisions with no exit options; bear full cost of constraint through operational restrictions
 *   - Smaller Gulf States (UAE, Kuwait, Bahrain, Oman, Qatar): Secondary victims (moderate/constrained) — benefit from de-escalation but face extraction through reduced agency in regional decision-making; constrained by economic dependency
 *   - Internal Opposition Factions: Tertiary victims (powerless/identity_locked) — Pro-war factions and hardline militias face suppression enabled by rapprochement; identity fused with regional conflict narrative
 *   - US-Led Alliance System: Institutional piton (institutional/arbitrage) — post-Cold War security framework persists through inertia; primary function (containing Iran) degraded but alliance structures maintained performatively
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent geopolitical arrangements as immutable structural laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(saudi_iran_rapprochement_2023, 0.52).
domain_priors:suppression_score(saudi_iran_rapprochement_2023, 0.65).
domain_priors:theater_ratio(saudi_iran_rapprochement_2023, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(saudi_iran_rapprochement_2023, extractiveness, 0.52).
narrative_ontology:constraint_metric(saudi_iran_rapprochement_2023, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(saudi_iran_rapprochement_2023, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(saudi_iran_rapprochement_2023, tangled_rope).
narrative_ontology:human_readable(saudi_iran_rapprochement_2023, "Saudi-Iran Rapprochement as Constraint Structure (2023)").
narrative_ontology:topic_domain(saudi_iran_rapprochement_2023, "geopolitical/regional_state_relations").

domain_priors:requires_active_enforcement(saudi_iran_rapprochement_2023).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(saudi_iran_rapprochement_2023, saudi_arabia_state).
narrative_ontology:constraint_beneficiary(saudi_iran_rapprochement_2023, iran_state).
narrative_ontology:constraint_beneficiary(saudi_iran_rapprochement_2023, china_as_mediator).
narrative_ontology:constraint_victim(saudi_iran_rapprochement_2023, regional_proxy_networks).
narrative_ontology:constraint_victim(saudi_iran_rapprochement_2023, smaller_gulf_states).
narrative_ontology:constraint_victim(saudi_iran_rapprochement_2023, internal_opposition_factions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROXY NETWORKS (SNARE) — Houthi militias, Iraqi PMU, and other proxy forces are structurally trapped by the rapprochement. They cannot exit the regional conflict structure; the agreement imposes constraints on their operations without providing alternative income or political pathways. Maximum extraction as military proxies are subordinated to state-level diplomacy they cannot influence. Theater ratio near zero — military enforcement is direct, not performative.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALLER GULF STATES (TANGLED ROPE) — These states benefit from reduced proxy warfare and restored shipping security (coordination function) but face extraction through reduced agency in regional decision-making. They are constrained by economic dependency on larger powers and risk of becoming proxy battlegrounds if the rapprochement breaks. Genuine coordination gains (de-escalation of Houthi attacks) combined with subordination to bilateral Saudi-Iran negotiations.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SAUDI ARABIA (ROPE) — Primary beneficiary. The rapprochement enables arbitrage: Saudi Arabia maintains military capabilities and US alliance while reducing regional spending and normalizing relations with Iran. Experiences the constraint as coordination — solving the 40-year proxy conflict problem. Extraction runs toward this state, not away. Low theater — direct diplomatic engagement, not performative.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IRAN (ROPE) — Co-beneficiary. Achieves regional legitimacy, sanctions relief pathways, and reduced military spending without surrendering core capabilities. Arbitrage position: can exit to proxy escalation if needed, but gains more from normalized commerce and reduced isolation. Coordination function: ending the zero-sum proxy game enables both states to invest domestically. Theater moderate — diplomatic ritual is performative (extensive ceremony and mediation theater by China) but underlying security gains are real.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINA (ROPE) — Tertiary beneficiary through mediation role. Arbitrage position: provides mediation service, gains regional influence, can escalate or de-escalate pressure. Experiences the rapprochement as coordination of its own positioning — using Middle East neutrality to expand geopolitical influence. Theater very high — mediation ceremony and diplomatic theater are performative investments in positioning China as the alternative arbiter to US-led order.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL ARMS CONTROL / NUCLEAR CONSTRAINT COALITIONS (SCAFFOLD) — International actors and coalitions organized around nuclear non-proliferation, regional arms limitations, and Yemen humanitarian corridors see the rapprochement as a temporary coordination framework with a sunset clause. If sustained for 10-15 years, it could mature into institutional arms control mechanisms. Constrained by inability to independently enforce agreements; see the framework as scaffolding toward mature regional governance. Theater moderate-high — confidence-building measures and humanitarian corridors are partly performative.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: US-LED ALLIANCE SYSTEM (PITON) — The post-Cold War security framework (US military bases, bilateral defense treaties, Gulf Cooperation Council coordination) is partially degraded by the rapprochement. The system persists through institutional inertia and continued US commitment, but its primary function (containing Iran) is no longer the central organizing principle. Theater very high — alliance rituals (military exercises, intelligence sharing, arms sales) continue performatively while the underlying strategic logic has shifted. This is theatrical maintenance of a declining institutional order.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / REALPOLITIK VIEW (MOUNTAIN) — From a civilizational frame, great-power competition in the Middle East follows immutable structural laws: competing regional hegemons will proxy-war, balance-of-power shifts create rapprochements, and multipolar competition is inherent to the system. This view naturalizes the rapprochement as an inevitable structural adjustment. However, the structural data contradicts this — the rapprochement is contingent on Chinese mediation, petrodollar flows, and internal Iranian/Saudi factional politics, not on immutable geopolitical laws. This is a false summit, naturalizing contingent institutional arrangements.
constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(saudi_iran_rapprochement_2023_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(saudi_iran_rapprochement_2023, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(saudi_iran_rapprochement_2023, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(saudi_iran_rapprochement_2023, TR),
    TR >= 0.70.

:- end_tests(saudi_iran_rapprochement_2023_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The rapprochement enables significant state-level benefits (Saudi and Iran as primary beneficiaries) while imposing constraints on proxy networks and smaller states with no compensation. The extraction is structural asymmetry between institutional beneficiaries and powerless/constrained victims. Theater ratio (0.58): Moderate-high. The rapprochement involves substantial diplomatic ceremony (Chinese mediation theater, multilateral announcements, confidence-building measures) alongside genuine security gains (reduced Houthi attacks, normalized trade corridors). The theater increased post-agreement as states invested in maintaining the diplomatic facade. Suppression (0.65): High. Significant barriers to dissent include proxy network subordination to state directives, internal opposition suppression in both Saudi and Iran, and smaller states' economic dependency on larger powers. Barrier reduction is unlikely without major structural shifts. The rapprochement increases suppression capacity by consolidating state control and reducing proxy autonomy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival gaps. The Saudi and Iranian states see themselves as solving a genuine coordination problem (Rope). Proxy militias see themselves as trapped pawns subordinated to diplomatic deals made without their input (Snare). Smaller Gulf states benefit from reduced regional violence but lose agency in shaping outcomes (Tangled Rope). The US alliance system sees itself as persistently relevant but increasingly performative (Piton). China sees itself as the emerging arbiter and mediation beneficiary (Rope). Internal opposition and hardline factions see forced consensus that violates their identity commitments (Snare with identity_locked dynamics). The civilizational analytical observer risks seeing this as immutable geopolitical law but the structural data — Chinese mediation dependency, petrodollar exchange rates, internal factional politics — reveals this as a contingent institutional arrangement, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is computed from beneficiary/victim status + exit options. Saudi Arabia and Iran (institutional/arbitrage) → d ≈ 0.15 (beneficiaries with exit capacity) → low χ experienced by these actors. Proxy networks (powerless/trapped) → d ≈ 0.95 (victims with no exit) → very high χ experienced. Smaller Gulf states (moderate/constrained) → d ≈ 0.70 (secondary victims with constrained exit) → high χ. Internal opposition (powerless/identity_locked) → d ≈ 0.88 (trapped by identity frame, not material barriers) → high χ. The identity_locked exit option for internal opposition is crucial: these actors could theoretically exit opposition and accept the rapprochement, but doing so would require abandoning identities constituted through pro-war or hardline positioning. The state can suppress them more efficiently than if they faced material barriers because the suppression operates on cognitive frames (delegitimization, narrative control) rather than on external constraints that might mobilize counter-movements. This is why identity_locked enables efficient extraction in institutional contexts: it converts external suppression into self-imposed silence.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint is Tangled Rope because (1) it has genuine coordination function — Saudi and Iran solving the 40-year proxy-war problem with measurable security gains (reduced Houthi attacks, normalized shipping); (2) it has asymmetric extraction — beneficiary states (Saudi/Iran) capture gains while victim actors (proxy networks, smaller states, internal opposition) lose options with no compensation; (3) it requires active enforcement — the rapprochement must be maintained through diplomatic pressure, proxy subordination, and suppression of internal opposition. The three gates are satisfied. The theater ratio (0.58) indicates that the coordination function is partially obscured by performative diplomacy, but the underlying structure is not purely theatrical (which would be Piton). The extractiveness (0.52) is moderate because the extraction, while real, is not maximal — smaller Gulf states still benefit from de-escalation; proxy networks retain some operational capacity; internal opposition can theoretically exit through accepting the consensus. If extractiveness rose to 0.70+, the constraint would approach Snare (pure extraction with minimal coordination). If theater ratio rose to 0.75+, it would approach Piton (performative structure). The current metrics place it squarely in Tangled Rope territory: hybrid coordination-extraction with active enforcement requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permanence_vs_tactical_pause,
    'Is the rapprochement a structural shift toward permanent regional multipolarization or a tactical pause in long-term competition?',
    '5-10 year tracking of bilateral trade volume, military spending, proxy activity resumption, Iranian nuclear program acceleration, and Saudi-China vs Saudi-US military commitments. Sustained trade growth and reduced proxy activity indicate structural shift; resurgence of proxy militias and military spending growth indicate tactical pause.',
    'If structural shift: classification remains Rope/Tangled Rope from beneficiary/victim perspectives, constraint is stabilizing. If tactical pause: classification shifts toward Snare for trapped actors (proxy networks), constraint is temporary extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_vs_tactical_pause, empirical, 'Whether rapprochement is permanent structural shift or tactical pause').

omega_variable(
    chinese_mediation_dependency,
    'Does the rapprochement create structural dependency on Chinese mediation that becomes itself extractive?',
    'Analysis of dispute resolution: if future Saudi-Iran conflicts require Chinese mediation or result in Chinese concessions extraction, Chinese leverage is asymmetric. Track whether China uses mediation role to extract trade concessions, military base access, or diplomatic deference.',
    'If dependency: creates new tangled rope constraint where Saudi and Iran are secondary beneficiaries but face extraction by Chinese leverage. If independent equilibrium: rapprochement is stable coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chinese_mediation_dependency, empirical, 'Whether Chinese mediation creates extractive dependency').

omega_variable(
    proxy_network_institutionalization,
    'Have proxy networks (Houthis, PMU, etc.) become institutionalized enough that state-level rapprochement cannot actually constrain them?',
    'Behavioral tracking: measure proxy activity (attacks, operations) independent of state directives. If proxy activity declines → states maintain enforcement capacity. If activity continues → networks have autonomy and the constraint is performative.',
    'If autonomous: constraints on proxy networks are theater (piton classification), and actual extraction continues despite diplomatic agreement. If state-controlled: constraint is binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proxy_network_institutionalization, empirical, 'Whether proxy networks are state-controllable or autonomous').

omega_variable(
    internal_opposition_suppression,
    'Does the rapprochement enable internal suppression of opposition factions within Saudi and Iranian states?',
    'Human rights monitoring: track arrests, detention, torture allegations targeting pro-war factions, hardline militias, and opposition groups post-rapprochement. Increase in suppression indicates rapprochement is enabling domestic coercion.',
    'If suppression increases: rapprochement becomes a snare for internal opposition (victims with no exit). If suppression stable or decreases: rapprochement may be genuinely stabilizing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internal_opposition_suppression, empirical, 'Whether rapprochement enables internal opposition suppression').

omega_variable(
    us_alliance_realignment,
    'Does the rapprochement trigger realignment of US security commitments and force withdrawal from the Gulf?',
    'Track US military posture: force levels, base agreements, arms sales volume, joint exercise frequency. Sustained commitment indicates US alliance persists parallel to rapprochement. Withdrawal indicates structural realignment.',
    'If US withdrawal: piton (degraded alliance system) may become active snare as US military umbrella disappears. If US maintains presence: piton status stable — performative alliance persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_alliance_realignment, empirical, 'Whether rapprochement triggers US security realignment').

omega_variable(
    theater_maintenance_cost,
    'What proportion of diplomatic activity post-rapprochement is performative ritual vs. substantive coordination?',
    'Analyze state speeches, media narratives, multilateral organization activity, arms sales, and actual constraint enforcement. Theater ratio measures: percentage of diplomatic activity that is symbolic vs. functional.',
    'If theater_ratio > 0.65: rapprochement is partially degraded (Piton elements). If theater_ratio < 0.40: genuine coordination structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_maintenance_cost, empirical, 'Theater ratio of post-rapprochement diplomatic activity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(saudi_iran_rapprochement_2023, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(saudiran_tr_t0, saudi_iran_rapprochement_2023, theater_ratio, 0, 0.42).
narrative_ontology:measurement(saudiran_tr_t6, saudi_iran_rapprochement_2023, theater_ratio, 6, 0.55).
narrative_ontology:measurement(saudiran_tr_t12, saudi_iran_rapprochement_2023, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(saudiran_be_t0, saudi_iran_rapprochement_2023, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(saudiran_be_t6, saudi_iran_rapprochement_2023, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(saudiran_be_t12, saudi_iran_rapprochement_2023, base_extractiveness, 12, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(saudi_iran_rapprochement_2023, enforcement_mechanism).
narrative_ontology:affects_constraint(saudi_iran_rapprochement_2023, yemeni_civil_war_structure).
narrative_ontology:affects_constraint(saudi_iran_rapprochement_2023, iraqi_pmf_autonomy).
narrative_ontology:affects_constraint(saudi_iran_rapprochement_2023, us_middle_east_strategic_posture).
narrative_ontology:affects_constraint(saudi_iran_rapprochement_2023, gulf_shipping_security).
narrative_ontology:affects_constraint(saudi_iran_rapprochement_2023, iranian_nuclear_program_constraints).

% DUAL FORMULATION NOTE:
% The rapprochement can be understood as a single constraint operating across multiple observables or as a constraint family decomposing into trade coordination, military constraint, proxy subordination, and regional balance-of-power recalibration. The current story treats it as a unified tangled rope with multiple beneficiary and victim classes. Alternative decomposition would create separate stories for: (1) Saudi-Iran trade coordination (lower ε, Rope-dominant), (2) proxy network subordination (higher ε, Snare-dominant), (3) US alliance degradation (moderate ε, Piton-dominant). The network links indicate that downstream constraints (Yemeni civil war, Iraqi PMF autonomy) are directly affected by changes in the parent constraint's enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(saudi_iran_rapprochement_2023, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
