% ============================================================================
% CONSTRAINT STORY: microwave_weapon_1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_microwave_weapon_1, []).

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
 *   constraint_id: microwave_weapon_1
 *   human_readable: Chinese Anti-Satellite Microwave Weapon
 *   domain: technological/political
 *
 * SUMMARY:
 *   The Chinese anti-satellite microwave weapon represents a structural
 *   extraction constraint operating across multiple levels: technological (RF
 *   physics weaponization), military (asymmetric space denial), geopolitical
 *   (coercive deterrence), and environmental (orbital commons degradation).
 *   The weapon creates a snare constraint for non-Chinese satellite operators
 *   and space-dependent economies: they are trapped in dependence on orbital
 *   infrastructure while facing an existential threat they cannot effectively
 *   defend against in the short term and cannot exit. The constraint exhibits
 *   different character from different structural positions: for Chinese
 *   military command, it is a coordination benefit (solving the space denial
 *   problem); for the US space architecture, it is tangled rope (forced into
 *   costly defensive competition); for the international space law regime, it
 *   is a degraded institutional framework (piton) maintained through symbolic
 *   compliance while functionally circumvented; for the orbital commons and
 *   sustainability advocates, it is a civilizational snare with potential for
 *   cascading, irreversible damage. The theater ratio is low (0.35) compared
 *   to the example constraint because the weapon's function is direct and
 *   concrete (electromagnetic damage to satellites) rather than performative.
 *   The extractiveness has increased over the interval (0.35→0.68) as the
 *   weapon technology matured and deployment credibility increased. The
 *   suppression is high (0.78) because alternative exit routes (technical
 *   hardening, constellation redundancy, ground alternatives) all require
 *   years to decades of investment and international coordination, while the
 *   weapon's deployment could occur within months.
 *
 * KEY AGENTS:
 *   - Chinese Military/Space Command: Primary beneficiary (organized/arbitrage) — develops and controls weapon; gains space denial capability and strategic asymmetry
 *   - Non-Chinese Satellite Operators: Primary victim (powerless/trapped) — operate commercial/civil satellites; face existential threat with no immediate defense or exit
 *   - Space-Dependent Global Economy: Primary victim (moderate/constrained) — finance, communications, agriculture, disaster response depend on satellite infrastructure; trapped by dependency and constrained by defense timelines
 *   - US Space Deterrence Architecture: Secondary actor (powerful/mobile) — perceives both deterrence benefit (justifies space spending) and extraction cost (forced into costly competition); has greatest mobility but highest vulnerability
 *   - Orbital Commons/Space Sustainability: Abstract victim (powerless/trapped) — stable orbital environment and long-term space access for all actors; faces irreversible degradation from debris cascades
 *   - International Space Law Regime: Institutional actor (institutional/constrained) — Outer Space Treaty signatories; maintains ceremonial compliance while lacking enforcement capability; functionally degraded (piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing military technology choices as inevitable physics outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microwave_weapon_1, 0.68).
domain_priors:suppression_score(microwave_weapon_1, 0.78).
domain_priors:theater_ratio(microwave_weapon_1, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microwave_weapon_1, extractiveness, 0.68).
narrative_ontology:constraint_metric(microwave_weapon_1, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(microwave_weapon_1, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(microwave_weapon_1, snare).
narrative_ontology:human_readable(microwave_weapon_1, "Chinese Anti-Satellite Microwave Weapon").
narrative_ontology:topic_domain(microwave_weapon_1, "technological/political").

% --- Structural relationships ---
narrative_ontology:constraint_victim(microwave_weapon_1, space_dependent_economies).
narrative_ontology:constraint_victim(microwave_weapon_1, non_chinese_satellite_operators).
narrative_ontology:constraint_victim(microwave_weapon_1, global_communication_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CHINESE SATELLITE OPERATORS (SNARE) — Cannot exit orbital infrastructure; vulnerable to weapon deployment with no defensive recourse except hardening (costly, delayed). Face extraction via capability asymmetry and coercion through existential threat to assets. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.71.
constraint_indexing:constraint_classification(microwave_weapon_1, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SPACE-DEPENDENT GLOBAL ECONOMY (SNARE) — Trapped by dependence on satellite infrastructure for communications, GPS, weather, finance, agriculture. Constrained ability to migrate to ground-based alternatives in short timeframe. Faces coercion through threat of cascading infrastructure failure. d≈0.88, f(d)≈1.35, σ=1.2 → χ≈0.71.
constraint_indexing:constraint_classification(microwave_weapon_1, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE MILITARY/SPACE COMMAND (ROPE) — Benefits from coordination benefit: weapon development solves a real military coordination problem (asymmetric space denial), demonstrates technological capability, deters adversary space operations. Experiences constraint as enabling coordination, not extraction. d≈0.15, f(d)≈0.02, σ=0.9 → χ≈0.01.
constraint_indexing:constraint_classification(microwave_weapon_1, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US SPACE DETERRENCE ARCHITECTURE (TANGLED ROPE) — Both benefits (justifies space control spending, accelerates space superiority initiatives) and bears extraction (faces credible threat to strategic assets, forced to harden infrastructure, locked into costly space race). Mobile exit option (orbital defense systems, distributed constellation) but within-system mobility is expensive. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(microwave_weapon_1, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ORBITAL COMMONS/SPACE SUSTAINABILITY (SNARE) — Abstract collective good (stable orbital environment, debris mitigation, sustainable space access) faces extraction via weapon deployment. No agency, no exit. Weapon use creates debris cascades that degrade orbital commons for centuries. d≈0.98, f(d)≈1.43, σ=1.0 → χ≈0.97.
constraint_indexing:constraint_classification(microwave_weapon_1, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL SPACE LAW REGIME (PITON) — Outer Space Treaty formally prohibits weapons of mass destruction in orbit but is silent on directed energy weapons against specific satellites. The regime persists through institutional inertia (ratified by 115 parties) but functional verification and enforcement is degraded: no inspection mechanism, no verification protocol for compliance, no enforcement against great-power treaty violation. theater_ratio≈0.65 (high ceremonial adherence, low actual constraint on state action). Regime is maintained through symbolic compliance and diplomatic rhetoric rather than functional deterrence.
constraint_indexing:constraint_classification(microwave_weapon_1, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a first-principles analysis, directed energy weapons in space represent an inevitable consequence of dual-use electromagnetic physics: radio frequency systems exist for telecommunications; scaling and focusing RF energy for damage is a straightforward engineering application. The physics of RF propagation in vacuum imposes no prohibition on weaponization. This perspective risks naturalizing contingent military technology choices as immutable laws of physics. The structural data (ε=0.68, suppression=0.78) indicates this is NOT a mountain — the weapon is a political-military choice, not a natural law. Perspective represents a false summit: confusing 'technically inevitable' with 'naturally inevitable'.
constraint_indexing:constraint_classification(microwave_weapon_1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microwave_weapon_1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(microwave_weapon_1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(microwave_weapon_1, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(microwave_weapon_1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(microwave_weapon_1, TR),
    TR >= 0.70.

:- end_tests(microwave_weapon_1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting the weapon's capacity to extract significant value from non-Chinese satellite operators through coercive asymmetry. The base rate is not maximal (0.90+) because the weapon's deployment is probabilistic (not certain) and some actors have non-zero defense options within the interval. However, the certainty of the weapon's existence and the difficulty of defending against it justify the high score. The trajectory shows increasing extractiveness (0.35→0.68) as the weapon matured from prototype to operational capability. Suppression (0.78): High. Non-Chinese actors face severe constraints: (1) Technical barriers to rapid hardening (engineering cycles, testing, qualification take years), (2) Economic barriers (defensive constellation redundancy costs billions), (3) Coordination barriers (multinational actors cannot quickly unify response), (4) Institutional barriers (no enforcement mechanism for international space law). The suppression reflects genuine structural inability to exit quickly, not mere cost. Theater ratio (0.35): Low. The microwave weapon's function is direct electromagnetic damage, not performative. This contrasts with military posturing or ceremonial arms race dynamics. The weapon either works or it doesn't; the constraint is functional rather than theatrical. Claimed type (snare): Fits the canonical profile — high extractiveness (0.68 ≥ 0.46), high suppression (0.78 ≥ 0.60), effective extraction χ ranges from 0.71-0.97 across victim perspectives (≥0.66 snare threshold).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The Chinese military sees rope/coordination (solving space denial). The US sees tangled rope (forced competition but with some agency). Non-Chinese satellite operators see pure snare (trapped, powerless, no agency). The orbital commons sees catastrophic snare (civilizational-scale irreversible harm). The international space law regime sees piton (ceremonial framework with degraded enforcement). The analytical observer risks seeing mountain (naturalizing military technology as physical inevitability), but the structural data reveals this as false summit — the constraint is a contingent geopolitical choice, not a law of physics. The perspectival gaps widen under crisis conditions: if deployment threat becomes imminent, all non-Chinese perspectives shift toward snare; if deployment occurs, the orbital commons perspective becomes catastrophic.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Chinese satellite operators: Victim + trapped → d≈0.92, f(d)≈1.40. Near-maximum extraction due to trapped exit and zero arbitrage options. Space-dependent economy: Victim + constrained → d≈0.88, f(d)≈1.35. High extraction due to constrained exit (defense timelines exceed crisis probability). Chinese military: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Net beneficiary; weapon solves a strategic coordination problem. US space architecture: Both victim and beneficiary, mobile exit → d≈0.52, f(d)≈0.68. Moderate extraction due to mobile exit option (orbital defense, constellation redundancy) and offsetting deterrence benefits. Orbital commons: Victim + trapped → d≈0.98, f(d)≈1.43. Near-maximum extraction; abstract collective with zero agency. International space law regime: Institutional + constrained → d≈0.55, f(d)≈0.75. Moderate-high extraction reflecting institutional capture by great powers and degraded enforcement capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by showing that the weapon is NOT a coordination mechanism (rope) for the global space ecosystem — it is a pure extraction mechanism (snare) from the perspective of actors who cannot deploy it or defend against it. The Chinese military's coordination benefit (solving space denial) does NOT extend to non-Chinese actors. The constraint is a clear zero-sum game: Chinese gain (space denial capability, strategic asymmetry) = non-Chinese loss (satellite vulnerability, coercive exposure). The temptation to mislabel this as 'coordination' (everyone gets space superiority aspirations) is false — only the possessor gains; everyone else bears extraction. The orbitalcommons perspective is maximally snare because the weapon creates irreversible negative externalities (debris) that cannot be compensated through coordination mechanisms. No amount of treaties or agreements restores the orbital environment once cascades begin. This is a clean mandatrophy resolution: the constraint is a snare, not rope, not tangled rope, because its function is pure asymmetric extraction with irreversible environmental harm. The piton perspective (international space law regime) is degraded because the regime lacks enforcement mechanisms — it persists through symbolic participation, not actual constraint on great-power behavior. The false mountain perspective (naturalizing the weapon as inevitable physics) is identified and rejected: the constraint is a political-military choice, not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weapon_deployment_threshold,
    'What event or condition triggers actual deployment of the microwave weapon? Is the extraction mechanism dependent on deployment threat (deterrence) or actual use (kinetic)?',
    'Analysis of Chinese military doctrine, wargame scenarios, crisis simulation data; comparison of coercive pressure under ''threat known'' vs ''deployment imminent'' vs ''post-use debris'' scenarios',
    'If threat-dependent: extraction is behavioral (fear-driven), and defensive hardening reduces χ significantly. If use-dependent: extraction is structural (infrastructure destruction), and deterrence cannot reduce χ. If deterrence fails: immediate shift to civilizational snare (orbital commons irreversibly degraded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weapon_deployment_threshold, empirical, 'Whether extraction is threat-based or deployment-based').

omega_variable(
    chinese_strategic_escalation_logic,
    'Does the weapon serve deterrence (preventing US space operations) or coercion (forcing policy concessions), or both? Under what political conditions would China deploy?',
    'PLA strategy documents, military exercise data, crisis simulation outcomes; analysis of signaling consistency and commitment credibility; historical precedent for space weapons use by great powers',
    'If pure deterrence: constraint is stabilizing (mutually acknowledged vulnerability → negotiation). If coercion: constraint is destabilizing (China can impose costs on non-compliant actors). If both: classification remains snare but severity depends on Chinese political objectives at time of crisis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chinese_strategic_escalation_logic, preference, 'Chinese strategic intent: deterrence vs coercion').

omega_variable(
    defense_feasibility_and_timeline,
    'Can non-Chinese actors achieve credible defense (hardening, redundancy, distributed constellation, ground-based alternatives) before political crisis makes deployment likely?',
    'Technical assessment of hardening timelines (years to harden critical satellites); economic analysis of constellation redundancy costs; analysis of ground-based alternative deployment schedules; correlation with geopolitical tension indicators and military exercise frequency',
    'If defense achievable before crisis: non-Chinese actors transition from snare (trapped) to tangled_rope (constrained but with mobile exit). If defense impossible in timeframe: snare persists, and escalation risk grows. Orbital commons remains at civilizational snare risk regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_feasibility_and_timeline, empirical, 'Timeline for achieving defensive capability vs crisis probability').

omega_variable(
    debris_cascade_irreversibility,
    'If weapon is deployed and creates debris, does the resulting cascade render entire orbital regions unusable for decades, or are cleanup/mitigation options viable?',
    'Orbital mechanics modeling of cascade scenarios; assessment of debris removal technology feasibility and cost; historical debris evolution data from previous fragmentation events',
    'If cascade is irreversible on human timescale: orbital commons shifts from snare to catastrophic externality (all actors eventually trapped in degraded orbital environment). If mitigation viable: snare remains bounded. Either way, space-dependent economy''s effective exit options degrade over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debris_cascade_irreversibility, empirical, 'Whether debris cascade causes reversible or irreversible orbital degradation').

omega_variable(
    international_response_coordination,
    'Can non-Chinese space-dependent actors coordinate a unified response (sanctions, reciprocal weapon development, coalition deterrence) or do they remain fragmented?',
    'Institutional analysis of NATO space coordination, Five Eyes space agreement coverage, commercial satellite operator consortium cohesion; game-theoretic assessment of coalition stability under threat',
    'If coordination succeeds: non-Chinese actors transition from isolated snare (powerless) to organized coalition (organized/constrained), potentially increasing f(d) but also enabling collective response. If fragmented: snare persists, and China''s extraction via divide-and-conquer logic increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_response_coordination, conceptual, 'Whether non-Chinese actors can coordinate collective defense').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(microwave_weapon_1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mw_tr_t0, microwave_weapon_1, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mw_tr_t5, microwave_weapon_1, theater_ratio, 5, 0.3).
narrative_ontology:measurement(mw_tr_t10, microwave_weapon_1, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(mw_be_t0, microwave_weapon_1, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mw_be_t5, microwave_weapon_1, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mw_be_t10, microwave_weapon_1, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(microwave_weapon_1, enforcement_mechanism).
narrative_ontology:affects_constraint(microwave_weapon_1, us_space_deterrence_architecture).
narrative_ontology:affects_constraint(microwave_weapon_1, orbital_debris_cascade).
narrative_ontology:affects_constraint(microwave_weapon_1, outer_space_treaty_enforcement).
narrative_ontology:affects_constraint(microwave_weapon_1, satellite_hardening_investment).

% DUAL FORMULATION NOTE:
% The microwave weapon constraint decomposes into multiple structural constraints: (1) the weapon's technical capacity (physics of RF damage, ε≈0.08, mountain from engineering perspective), (2) the weapon's coercive capacity (geopolitical threat, ε≈0.68, snare from operator perspective), and (3) the weapon's environmental consequences (debris cascades, ε≈0.85, civilizational snare for orbital commons). This story focuses on the coercive-geopolitical constraint (ε=0.68). The technical physics constraint (ε≈0.08) is a separate upstream story that affects this one — the weapon's political impact depends on the solved engineering problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(microwave_weapon_1, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
