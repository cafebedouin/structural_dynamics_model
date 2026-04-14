% ============================================================================
% CONSTRAINT STORY: sotu_1958_eisenhower_strategic_air_command_deterrent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1958_eisenhower_strategic_air_command_deterrent, []).

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
 *   constraint_id: sotu_1958_eisenhower_strategic_air_command_deterrent
 *   human_readable: Strategic Air Command Retaliatory Capacity as Deterrent to Soviet Aggression (1958)
 *   domain: military/geopolitical/strategic_deterrence
 *
 * SUMMARY:
 *   In his 1958 State of the Union address, President Eisenhower presented
 *   Strategic Air Command's retaliatory bomber capacity as the cornerstone of
 *   American defense strategy and the shield protecting the entire NATO
 *   alliance. The constraint is elegantly stated: SAC's ability to inflict
 *   'virtual annihilation' on any aggressor even after absorbing a surprise
 *   attack makes aggression prohibitively expensive for the Soviet Union,
 *   thereby protecting all free nations under the security umbrella and
 *   enabling diplomatic and economic cooperation. This narrative establishes
 *   a deterrent mechanism that functions as both coordination (enabling
 *   allied peace-building) and extraction (imposing costs on Soviet expansion
 *   and on all populations held hostage to the mutual annihilation calculus).
 *   The constraint exhibits all six classification types from different
 *   structural positions, making it a diagnostic exemplar for how indexical
 *   classification reveals hidden asymmetries in security arrangements. The
 *   extractiveness metric increases over the measurement interval (0.42 to
 *   0.68) as the initial asymmetry persists despite Soviet nuclear
 *   development, and the theater ratio rises correspondingly (0.50 to 0.75)
 *   as the functional deterrent logic yields to ritual maintenance of Cold
 *   War posturing. By the 1970s, the constraint transitions toward piton
 *   status—persisting through institutional inertia and strategic doctrine
 *   even as submarine-based deterrence (with its genuine second-strike
 *   invulnerability) becomes technically superior.
 *
 * KEY AGENTS:
 *   - Strategic Air Command (US military): Primary enforcer (institutional/arbitrage) — maintains retaliatory capacity, derives institutional power and budget justification from the deterrent mission
 *   - NATO Alliance Members: Primary beneficiaries (institutional/arbitrage) — gain security umbrella enabling economic reconstruction and diplomatic flexibility; experience constraint as Rope (coordination)
 *   - Soviet Leadership: Constrained party (organized/constrained) — faces suppression of conventional military options and forced investment in nuclear counter-capacity; experiences constraint as Tangled Rope (mixed coordination and extraction)
 *   - Soviet Satellite States: Trapped victim (powerless/trapped) — forced into bloc alignment, denied independent foreign policy, experience extraction through geopolitical subordination backed by deterrent threat; experience constraint as Snare
 *   - Civilian Populations (US, NATO, Soviet): Hostage victims (powerless/trapped) — protection for Western civilians is contingent on military command decisions; Soviet civilians are directly threatened; both are hostages to deterrent credibility; experience constraint as Snare
 *   - Developing Nations and Non-Aligned States: Peripheral victims (moderate/constrained) — face pressure toward bloc alignment or non-interference with US security interests; unclear whether deterrent suppresses Soviet expansion or extracts alignment; experience constraint as Tangled Rope or Snare depending on position
 *   - Analytical Observer (civilizational): Sees natural law (analytical/analytical) — risks naturalizing contingent institutional choices (bomber-based deterrent, continuous alert posture, alliance extension) as inevitable laws of nuclear physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1958_eisenhower_strategic_air_command_deterrent, 0.58).
domain_priors:suppression_score(sotu_1958_eisenhower_strategic_air_command_deterrent, 0.72).
domain_priors:theater_ratio(sotu_1958_eisenhower_strategic_air_command_deterrent, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1958_eisenhower_strategic_air_command_deterrent, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1958_eisenhower_strategic_air_command_deterrent, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_1958_eisenhower_strategic_air_command_deterrent, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1958_eisenhower_strategic_air_command_deterrent, tangled_rope).
narrative_ontology:human_readable(sotu_1958_eisenhower_strategic_air_command_deterrent, "Strategic Air Command Retaliatory Capacity as Deterrent to Soviet Aggression (1958)").
narrative_ontology:topic_domain(sotu_1958_eisenhower_strategic_air_command_deterrent, "military/geopolitical/strategic_deterrence").

domain_priors:requires_active_enforcement(sotu_1958_eisenhower_strategic_air_command_deterrent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_strategic_air_command_deterrent, nato_alliance_members).
narrative_ontology:constraint_beneficiary(sotu_1958_eisenhower_strategic_air_command_deterrent, us_strategic_dominance).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_strategic_air_command_deterrent, soviet_satellite_states).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_strategic_air_command_deterrent, nuclear_hostage_populations).
narrative_ontology:constraint_victim(sotu_1958_eisenhower_strategic_air_command_deterrent, developing_nations_periphery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET SATELLITE STATE (SNARE) — Trapped in geographic proximity and ideological block assignment. Faces maximal extraction: cannot pursue independent foreign policy, cannot align with West, cannot exit alliance structure. SAC deterrent operates as pure suppression of alternatives — the satellite experiences it as annihilation threat contingent on disobedience. No coordination benefit; maximum coercion. The constraint's extractiveness manifests as political subordination backed by nuclear threat cascade.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: CIVILIAN POPULATION (SNARE) — Both Western and Soviet populations are hostages to the deterrent system. Western civilians are protected by SAC but also held as targets in the mutual annihilation calculus. Soviet civilians are directly threatened. All civilian populations face existential extraction: their survival is contingent on military command decisions outside democratic control. Theater is maximal — the civilian protection narrative masks a deterrent system that requires civilians as hostages for credibility.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NATO ALLIANCE (ROPE) — Experiences the deterrent as pure coordination. SAC capacity enables alliance members to pursue diplomatic and economic cooperation with security assured. No perceived extraction — the alliance views SAC as a collective good, a shared shield enabling lower military spending and diplomatic initiative. The beneficiary perspective with high arbitrage — European NATO members can free-ride on US nuclear umbrella while investing in economic reconstruction. Extraction is low from this position because the alliance has agency: it can negotiate its security relationship and maintain diplomatic flexibility.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET LEADERSHIP (TANGLED ROPE) — Faces mixed coordination and extraction. The deterrent creates a stable (if perilous) coordination framework: both sides benefit from reduced war probability compared to a world of conventional military competition. But extraction is asymmetric and severe — Soviet leadership's options are constrained. SAC capacity forces the Soviets to maintain expensive nuclear counter-capacity and forgo conventional military dominance. The constraint coordinates mutual restraint but at a cost differentially borne by the constrained party. Theater is moderate — the 'mutual deterrence' rhetoric masks asymmetric power (US SAC operational in 1958; Soviet ICBM capability still developing).
constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL SYSTEM (TANGLED ROPE) — The deterrent creates both coordination and extraction at the system level. Coordination benefit: reduced great-power war probability, enabling diplomatic and economic interdependence. Extraction cost: global politics is structured around US-Soviet confrontation; smaller states are forced into bloc alignment; the threat of annihilation becomes the background radiation of international relations. The system is constrained by the deterrent logic but also coordinated by it — the rules of engagement during the Cold War are established through SAC's shadow. Theater ratio reflects that much diplomatic activity performs the deterrent narrative rather than advancing its stated goals.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DETERRENT RITUAL SYSTEM (PITON) — From a civilizational timescale, the deterrent's functional role (reducing war probability) persists while the mechanism becomes increasingly theater. As Soviet nuclear capability matures (1960s onward), the asymmetric power base erodes, yet the deterrent narrative persists through institutional inertia. The ritual of SAC alert, the bomber-based deterrent, the 'second-strike capability' narrative continues to structure military spending, alliance politics, and strategic doctrine long after the specific technical advantage fades. By the 1970s-80s, the strategic bomber is economically and technologically superseded by submarines (true second-strike capability), yet SAC maintains its institutional primacy. The piton classification captures this degradation: the mechanism persists through organizational momentum and political theater, not because it remains the most effective deterrent.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an analytical civilizational perspective, nuclear deterrence appears as an immutable structural feature of the post-1945 world: the logic of Mutually Assured Destruction (MAD) follows inexorably from nuclear physics and military technology. The deterrent is not a contingent policy choice but a natural consequence of the technological landscape. No exit is possible; no alternatives exist; the constraint is unchangeable as a physical law. However, this perspective risks naturalizing what is actually a series of contingent institutional choices: the decision to base deterrence on bombers (vs. submarines, vs. ICBMs), the decision to keep forces continuously alert, the decision to publicize retaliatory capacity rather than keep it secret, the decision to extend the umbrella to allies. Each is a choice, not a law. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1958_eisenhower_strategic_air_command_deterrent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1958_eisenhower_strategic_air_command_deterrent, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1958_eisenhower_strategic_air_command_deterrent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1958_eisenhower_strategic_air_command_deterrent, TR),
    TR >= 0.70.

:- end_tests(sotu_1958_eisenhower_strategic_air_command_deterrent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, rising to 0.68): The constraint exhibits significant extraction that increases over time as the initial asymmetry of SAC operational capacity versus Soviet ICBM development persists. In 1958, SAC holds overwhelming advantage; the Soviet Union is forced to accept massive defense spending and political subordination of satellite states to maintain strategic parity. The extraction is asymmetric: NATO benefits from the security umbrella, Soviet leadership pays the cost, satellite states and civilian populations bear the highest cost (hostage status). The rising trajectory reflects that as the constraint persists despite Soviet nuclear development, the institutional arrangements and political subordinations it enabled become entrenched—the extraction mechanism becomes self-reinforcing. Suppression (0.72): Very high. The deterrent works through suppression: it blocks Soviet options (conventional military expansion, satellite state independence, non-aligned nations' autonomy). The mechanism requires maintained threat of annihilation and the credible ability to inflict it on civilian populations. Theater ratio (0.68, rising to 0.75): High and increasing. The deterrent narrative—'massive retaliation,' 'balance of terror,' 'peace through strength'—is performative in multiple dimensions. The public discourse about SAC's invincibility must be maintained for deterrent credibility, yet the actual technical capabilities are often classified. The ritual of alert postures, bomber deployments, and strategic announcements performs the deterrent narrative to allies and adversaries. As submarine-based deterrence becomes technically superior (true second-strike invulnerability) but politically less dramatic than bomber imagery, the SAC-centered doctrine persists through theater—institutional momentum and strategic narrative override technical optimization.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal how a single constraint structure produces divergent classifications across the observation spectrum. NATO experiences Rope: coordination mechanism enabling security-backed alliance. Soviet Leadership experiences Tangled Rope: mixed coordination (mutual restraint reduces war risk) and extraction (forced defense spending, satellite subordination). Soviet Satellites experience Snare: pure suppression with no coordination benefit. Civilian Populations experience Snare: hostage status with no democratic control. Deterrent Ritual System experiences Piton: institutional theater persisting through inertia despite technical obsolescence. Analytical Observer at civilizational scale risks Mountain: naturalizing the deterrent as inevitable consequence of nuclear physics. The perspectival gaps reflect structural asymmetries in the constraint: it genuinely coordinates (reduces war probability, enables prosperity behind the shield) while genuinely extracting (forces military spending, blocks alternatives, holds populations hostage). Different agents benefit from different aspects—NATO from the shield, US military from institutional power, Soviet Union from mutual restraint, satellite states from neither, civilian populations from reduced war probability but not from the logic of mutually assured destruction itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from the agent's structural position in the extraction flow. NATO (institutional/arbitrage): d ≈ 0.05-0.15. Beneficiary with high exit options (economic reconstruction possible without deterrent, could pursue neutrality or independent defense). Derived d is low (beneficiary); effective extraction χ is negative (the constraint subsidizes this agent). Soviet Leadership (organized/constrained): d ≈ 0.55-0.65. Victim of constrained options (cannot abandon nuclear competition, cannot allow satellite independence without deterrent threat eroding). Derived d is moderate-high (asymmetric cost bearer); effective extraction χ is positive and moderate. Soviet Satellites (powerless/trapped): d ≈ 0.90-0.95. Victim with no exit (geopolitically trapped, politically subordinated, no way to exit bloc without deterrent threat materializing). Derived d is very high (full target); effective extraction χ is maximum. Civilian Populations (powerless/trapped): d ≈ 0.92-0.98. Victims with no exit (hostages to deterrent credibility, no democratic voice in targeting decisions, survival contingent on military command choices). Derived d is very high; effective extraction χ is maximum. Deterrent Ritual System (institutional/arbitrage): d ≈ 0.02-0.10. Beneficiary with high arbitrage (bureaucratic agencies, defense contractors, military leadership derive power and resources from deterrent mission). Derived d is very low; constraint subsidizes this actor. The perspectival gap reflects that beneficiaries experience negative or very low χ (gain from the constraint), while trapped and powerless agents experience maximum χ (full extraction cost). The organized constrained party (Soviet leadership) occupies the middle—experiences extraction but has some agency and some coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy (prohibition on simultaneously calling a mechanism pure coordination and pure extraction) by showing that it is genuinely both. The resolution is perspectival: from NATO's position, it is coordination (Rope). From the Soviet position, it is extraction plus coordination (Tangled Rope). From the satellite position, it is pure extraction (Snare). From the analytical civilizational position, it risks being mislabeled as natural law (Mountain) when it is actually a contingent institutional arrangement. The mandatrophy is resolved not by choosing one type but by recognizing that the indexical tuple (power, time, exit, scope) determines which aspects of the constraint's structure become salient to the agent. An agent with arbitrage options experiences the constraint's coordination function. An agent with trapped options experiences the extraction function. The constraint's essence is Tangled Rope at the systemic level: it coordinates mutual restraint while extracting costs asymmetrically from trapped parties. The institutional theater (Piton perspective) emerges as the constraint persists beyond its functional prime, maintained through bureaucratic momentum and strategic narrative rather than technical optimization. This is not instability but predictable lifecycle drift: genuine coordination mechanisms can decay into theater when the institutional actors benefit more from maintaining the narrative than from achieving the stated function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_credibility_mechanism,
    'Is the deterrent''s effectiveness based on the Soviet leadership''s rational cost-benefit calculation, or does it rely on irrational/uncontrollable escalation dynamics?',
    'Analysis of Soviet strategic doctrine statements; comparison of Soviet behavior in crises where deterrent threat was implicit vs. explicit; game-theoretic modeling vs. empirical response patterns',
    'If rational calculation: deterrent is a rope-like coordination mechanism with genuine mutual benefit (reduced war probability). If escalation dynamics: deterrent is a snare-like extraction mechanism with risk of catastrophic failure. Classification shifts from Tangled Rope toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_credibility_mechanism, conceptual, 'Whether deterrent credibility rests on rational calculation or escalation dynamics').

omega_variable(
    extraction_asymmetry_duration,
    'As Soviet nuclear capability approaches parity with SAC, does the extraction asymmetry persist, or does the constraint transition to genuine mutual deterrence (rope)?',
    'Temporal tracking of Soviet ICBM deployment (1960-1975), analysis of NATO strategy documents showing shift from massive retaliation to flexible response, measurement of US-Soviet strategic force symmetry timelines',
    'If asymmetry persists despite parity: constraint remains Tangled Rope with entrenched institutional extraction. If asymmetry resolves: constraint becomes Rope (mutual coordination). Affects long-term classification stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_duration, empirical, 'Whether extraction asymmetry persists as Soviet nuclear capability reaches parity').

omega_variable(
    suppression_vs_coordination_weighting,
    'What proportion of SAC''s constraint effect is achieved through suppression (blocking Soviet options) versus coordination (enabling alliance diplomacy)?',
    'Counterfactual analysis: model scenarios of SAC deployment vs. removal; compare NATO alliance cohesion, diplomatic flexibility, and economic cooperation in SAC-enabled period vs. pre-nuclear period; analyze Eisenhower administration documents quantifying the shield function',
    'If suppression dominates (>70%): constraint is primarily snare-like extraction. If coordination dominates (>60%): constraint is primarily rope-like mutual benefit. Current classification at 58% extractiveness assumes rough balance; resolved value would tighten classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_coordination_weighting, empirical, 'Proportion of constraint effect from suppression vs. coordination function').

omega_variable(
    third_world_coercion_intensity,
    'Does SAC deterrent extend to non-aligned third-world states, and if so, does it function as pure suppression (preventing Soviet-aligned aggression) or as extraction (forcing alignment)?',
    'Analysis of Cold War conflicts in third world (Korea, Vietnam, Middle East); examination of US military intervention patterns; assessment of non-aligned nations'' strategic autonomy during SAC-dominant period',
    'If pure suppression: some third-world victims benefit from reduced Soviet-backed aggression. If coercive extraction: SAC extends NATO''s extraction asymmetry globally. Affects characterization of victim groups and spatial scope of constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_world_coercion_intensity, empirical, 'Whether SAC deterrent suppresses third-world aggression or coerces alignment').

omega_variable(
    piton_transition_timeline,
    'When does SAC transition from functional deterrent (Tangled Rope) to degraded theater (Piton)? Does this occur at ICBM parity, submarine parity, or is it a gradual institutional drift?',
    'Tracking of SIOP (Single Integrated Operational Plan) updates; analysis of strategic force composition shifts (bomber vs. missile reliance); examination of budget allocations to SAC vs. missile forces; institutional history of SAC in post-parity period',
    'If sharp transition at parity: SAC degrades to Piton by 1970. If gradual drift: Piton classification applies retroactively from 1975-1980 onward. Affects temporal measurement trajectory and mandatrophy resolution for downstream constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(piton_transition_timeline, empirical, 'Timeline and mechanism of SAC transition from functional deterrent to degraded institutional theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1958_eisenhower_strategic_air_command_deterrent, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sac_det_tr_t0, sotu_1958_eisenhower_strategic_air_command_deterrent, theater_ratio, 0, 0.5).
narrative_ontology:measurement(sac_det_tr_t5, sotu_1958_eisenhower_strategic_air_command_deterrent, theater_ratio, 5, 0.58).
narrative_ontology:measurement(sac_det_tr_t10, sotu_1958_eisenhower_strategic_air_command_deterrent, theater_ratio, 10, 0.64).
narrative_ontology:measurement(sac_det_tr_t15, sotu_1958_eisenhower_strategic_air_command_deterrent, theater_ratio, 15, 0.7).
narrative_ontology:measurement(sac_det_tr_t20, sotu_1958_eisenhower_strategic_air_command_deterrent, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(sac_det_be_t0, sotu_1958_eisenhower_strategic_air_command_deterrent, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sac_det_be_t5, sotu_1958_eisenhower_strategic_air_command_deterrent, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sac_det_be_t10, sotu_1958_eisenhower_strategic_air_command_deterrent, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sac_det_be_t15, sotu_1958_eisenhower_strategic_air_command_deterrent, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(sac_det_be_t20, sotu_1958_eisenhower_strategic_air_command_deterrent, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1958_eisenhower_strategic_air_command_deterrent, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_strategic_air_command_deterrent, cuban_missile_crisis_standoff).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_strategic_air_command_deterrent, nuclear_weapons_proliferation_control).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_strategic_air_command_deterrent, warsaw_pact_alliance_subordination).
narrative_ontology:affects_constraint(sotu_1958_eisenhower_strategic_air_command_deterrent, mutual_assured_destruction_credibility).

% DUAL FORMULATION NOTE:
% SAC deterrent as enforcement mechanism is structurally distinct from the broader Cold War constraint system. SAC represents the technical backbone enabling US strategic dominance; the Cold War system is the geopolitical structure built on that dominance. This story focuses on the deterrent mechanism itself (why SAC capacity structures global politics). Downstream stories examine specific consequences: Cuban Missile Crisis (deterrent credibility test), nuclear proliferation (deterrent extension logic), Warsaw Pact cohesion (deterrent-backed satellite subordination), MAD credibility (deterrent logic in mutual parity). All upstream to SAC deterrent: American nuclear weapons development, NATO formation, Soviet ICBM development.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1958_eisenhower_strategic_air_command_deterrent, institutional, 0.08).
constraint_indexing:directionality_override(sotu_1958_eisenhower_strategic_air_command_deterrent, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
