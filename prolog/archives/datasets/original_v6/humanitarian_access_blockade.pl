% ============================================================================
% CONSTRAINT STORY: humanitarian_access_blockade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humanitarian_access_blockade, []).

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
 *   constraint_id: humanitarian_access_blockade
 *   human_readable: Humanitarian Access Blockade in Conflict Zones
 *   domain: geopolitical/humanitarian
 *
 * SUMMARY:
 *   The humanitarian access blockade in conflict zones represents a
 *   structural mechanism that extracts maximum cost from civilian populations
 *   while maintaining military strategic rationale. The constraint exhibits
 *   snare characteristics from the perspective of trapped civilians with zero
 *   exit options, while appearing as coordination (rope) from the blockading
 *   military perspective and mixed coordination-extraction (tangled rope)
 *   from humanitarian organizations that mediate access. The blockade
 *   enforces territorial control by denying civilians food, medicine, fuel,
 *   and other essentials, simultaneously denying these resources to organized
 *   resistance. The extractiveness has increased over time (0.45 to 0.68) as
 *   enforcement mechanisms intensify, while theater ratio remains moderate
 *   (0.35 to 0.45) because the blockade's operation is relatively transparent
 *   — the enforcement is visible rather than hidden. The constraint's
 *   suppression (0.78) reflects severe barriers to exit: civilians cannot
 *   leave the geographic area under blockade, underground resistance networks
 *   face lethal enforcement, humanitarian organizations face authorization
 *   gatekeeping. The blockade functions simultaneously as military strategy
 *   (coordination from the blockading power's perspective) and as collective
 *   punishment (extraction from civilians' perspective), making it a
 *   diagnostic case for perspectival classification and the mandatrophy
 *   problem.
 *
 * KEY AGENTS:
 *   - Civilian Population: Primary victim (powerless/trapped) — cannot exit geographic boundaries, cannot negotiate access to essentials, zero agency in blockade design or enforcement
 *   - Blockading Military Command: Primary beneficiary (institutional/arbitrage) — maintains territory control, denies resources to adversaries, can revoke blockade at strategic discretion
 *   - Humanitarian Organization: Secondary actor (moderate/constrained) — coordinates survival logistics under military authorization; benefits from operational legitimacy; bears extraction through fees, delays, conditional access, mission compromise
 *   - Organized Resistance Movement: Secondary victim (organized/constrained) — coordinates underground supply networks; faces severe enforcement action but retains organizational agency
 *   - International Sanctions Regime: Institutional framework (organized/mobile) — blockade as enforcement mechanism for broader sanctions; has explicit sunset clause tied to political settlement
 *   - International Legal Framework: Institutional observer (institutional/arbitrage) — International Humanitarian Law framework exists but enforcement capacity is degraded; maintains performative compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humanitarian_access_blockade, 0.68).
domain_priors:suppression_score(humanitarian_access_blockade, 0.78).
domain_priors:theater_ratio(humanitarian_access_blockade, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humanitarian_access_blockade, extractiveness, 0.68).
narrative_ontology:constraint_metric(humanitarian_access_blockade, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(humanitarian_access_blockade, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humanitarian_access_blockade, snare).
narrative_ontology:human_readable(humanitarian_access_blockade, "Humanitarian Access Blockade in Conflict Zones").
narrative_ontology:topic_domain(humanitarian_access_blockade, "geopolitical/humanitarian").

domain_priors:requires_active_enforcement(humanitarian_access_blockade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humanitarian_access_blockade, blockading_state_military).
narrative_ontology:constraint_beneficiary(humanitarian_access_blockade, blockading_coalition_leadership).
narrative_ontology:constraint_victim(humanitarian_access_blockade, civilian_population).
narrative_ontology:constraint_victim(humanitarian_access_blockade, humanitarian_organizations).
narrative_ontology:constraint_victim(humanitarian_access_blockade, medical_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped within geographic boundaries with no exit. Cannot obtain food, medicine, or fuel. Bears maximum extraction cost with zero agency or alternatives. The blockade is enforced through military control of all entry/exit points. Classification emerges from complete structural immobility: civilians cannot negotiate, organize, or escape.
constraint_indexing:constraint_classification(humanitarian_access_blockade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATION (TANGLED ROPE) — Constrained by military authorization requirements, security risks, and bureaucratic access negotiations, yet coordinates essential survival logistics. Benefits from operational legitimacy and partial access granted by blockading authority; simultaneously bears extraction through fees, delays, conditional access, and mission compromise. Moderate power enables some negotiation; constrained exit means costs of withdrawal are high (organizational mission failure, staff safety liability) but theoretically possible.
constraint_indexing:constraint_classification(humanitarian_access_blockade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BLOCKADING MILITARY COMMAND (ROPE) — Experiences the constraint as pure coordination of military logistics. The blockade enforces territorial control and denies resources to adversaries. From this perspective, it is a coordination mechanism for war strategy: communicating capability (blockade is enforced), maintaining supply lines, and preventing enemy resupply. Institutional power and arbitrage exit (can revoke blockade at strategic discretion) produce the rope classification.
constraint_indexing:constraint_classification(humanitarian_access_blockade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED RESISTANCE (SNARE) — Underground supply networks face constant military interdiction. Organized enough to coordinate black-market relief but constrained by severe enforcement action (arrest, execution). The blockade simultaneously attempts to suppress and is suppressed by organized resistance. Chi remains high despite organized power because suppression is equally severe — enforcement mechanisms match organizational capacity.
constraint_indexing:constraint_classification(humanitarian_access_blockade, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL SANCTIONS REGIME (SCAFFOLD) — Blockade functions as enforcement mechanism for broader sanctions architecture; has explicit sunset clause tied to political settlement negotiations. Sanctions are framed as temporary coercive measure to force diplomatic capitulation, with declared endpoint (peace agreement, regime change, or policy concession). Organized international actors see exit pathway and sunset timeline, reducing experienced extraction relative to civilians' view.
constraint_indexing:constraint_classification(humanitarian_access_blockade, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL FRAMEWORK (PITON) — Blockade persists despite significant International Humanitarian Law violations (collective punishment, destruction of medical infrastructure, food denial). The legal framework exists (Geneva Conventions, UN Charter) but has degraded enforcement capacity. Theater ratio is high (statements of concern, UN resolutions, humanitarian appeals) while functional constraint enforcement is low (blockade continues without enforcement action). The framework maintains legitimacy through performative compliance declarations while the underlying violation persists.
constraint_indexing:constraint_classification(humanitarian_access_blockade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRATEGIC NECESSITY VIEW (MOUNTAIN) — From civilizational perspective, some actors claim the blockade is immutable law of war: successful military campaigns require denying adversary resources; this is structural necessity, not contingent policy choice. However, structural data reveals this as false naturalization — blockades are chosen policy instruments with alternative strategies available (surgical strikes, targeted sanctions, territorial control without collective punishment).
constraint_indexing:constraint_classification(humanitarian_access_blockade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humanitarian_access_blockade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(humanitarian_access_blockade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(humanitarian_access_blockade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(humanitarian_access_blockade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(humanitarian_access_blockade, TR),
    TR >= 0.70.

:- end_tests(humanitarian_access_blockade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The blockade imposes maximum cost on civilians (food denial, medical supply denial, fuel scarcity) while the blockading power captures strategic benefits (territorial control, adversary degradation, negotiation leverage). The trajectory shows accumulation: initial blockade (0.45) tightens enforcement over time, increasing civilian impact. Suppression (0.78): Very high. Enforcement mechanisms are severe: military control of all entry/exit points, lethal interdiction of black-market supply, arrest and execution of resistance participants. Civilians have no practical exit option — the only alternatives are displacement (internal) or death. Theater ratio (0.45): Low-moderate. The blockade is operationally transparent — its enforcement is visible, not hidden. However, some performative content exists: humanitarian appeals, UN resolutions, and legal framework statements create theater around enforcement. The theater is lower than institutional blockades that maintain legitimacy through deception; the blockade is often defended explicitly as necessary military tactic. Claimed type (Snare): From civilians' perspective, this is maximal extraction with no coordination benefit. The blockade does not solve a collective action problem for civilians — it imposes one-way cost extraction.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (civilians) and rope perspective (military command) represent maximum divergence in experienced classification. Both perspectives measure the same constraint (resource denial via military enforcement) but with opposite directionality. Civilians experience extraction because d approaches 1.0 (full target). Military command experiences coordination because d approaches 0.0 (full beneficiary). The tangled rope perspective (humanitarian organizations) represents the moderate middle ground where both coordination (logistics management) and extraction (conditionality, fees) occur simultaneously. The scaffold perspective (international sanctions regime) introduces time: blockade appears temporary with sunset clause, reducing experienced extraction. The piton perspective (international legal framework) reveals degradation: the framework exists (Geneva Conventions prohibit collective punishment) but enforcement is performative. The analytical mountain perspective risks naturalizing a policy choice as immutable law of war.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (blockading military): Low d (0.05-0.20), deriving from institutional power + arbitrage exit + beneficiary status. The military command can exit the blockade at strategic discretion; they benefit from its continuation. Victim directionality (civilians): High d (0.92-0.98), deriving from powerless status + trapped exit + victim status. Civilians cannot exit; they bear extraction cost. Humanitarian organization directionality (moderate): Medium d (0.50-0.65), deriving from moderate power + constrained exit + mixed beneficiary/victim status. Organizations benefit from operational legitimacy but bear extraction through conditionality. Organized resistance directionality (organized): High d (0.75-0.85) despite organized power, because suppression is equally severe (enforcement matches organizational capacity). The engine computes chi = epsilon × f(d) × sigma(scope), amplifying extraction experienced by trapped/powerless agents while dampening extraction experienced by beneficiaries with arbitrage exits.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates the mandatrophy's core issue — whether a constraint that extracts severely is extractive by nature or by political choice. The civilian perspective sees snare (inherent extraction). The military perspective sees rope (inherent coordination). Both cannot be simultaneously correct about the constraint's objective type — one is false naturalization. The resolution: the constraint IS a snare from the civilian's structural position (trapped, powerless, zero exit, zero coordination benefit). The military's rope perspective is false naturalization (claiming strategic necessity as immutable law). The Snare classification prevails because it captures the constraint from the perspective of those most constrained by it. The mandatrophy is resolved by recognizing that indexical classification is not about finding 'the true type' but about measuring what the constraint actually is from each structural position. The snare is the most honest read because it acknowledges the asymmetry: civilians cannot exit, cannot coordinate, cannot negotiate. The rope is a perspective manufactured by power asymmetry, not a structural feature of the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    blockade_strategic_necessity,
    'Is the blockade a necessary military tactic or a policy choice with available alternatives?',
    'Comparative analysis of comparable conflicts: which achieve military objectives without full blockades? What is the actual military payoff of blocking civilian goods vs. only blocking military supplies?',
    'If necessary: classification shifts toward mountain (immutable constraint). If policy choice: snare classification is correct (contingent extractive mechanism). Determines whether blockade is reframeable as coordination problem or inherent to conflict structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blockade_strategic_necessity, empirical, 'Whether blockade is strategic necessity or policy choice').

omega_variable(
    humanitarian_access_negotiation_leverage,
    'Does humanitarian access conditionality create negotiating leverage for the blockading power or does it primarily extract humanitarian resources?',
    'Analysis of humanitarian negotiations: do conditions imposed by blockading power result in documented policy concessions? Or do they primarily delay aid without strategic payoff?',
    'If leverage is genuine: blockade has coordination function (tangled rope strengthens). If leverage is primarily performative: extraction dominates and snare classification deepens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_access_negotiation_leverage, empirical, 'Whether humanitarian conditionality creates negotiation leverage').

omega_variable(
    civilian_death_extraction_mechanism,
    'Is civilian mortality a side effect of blockade enforcement or a deliberate extraction mechanism (collective punishment incentivizing political capitulation)?',
    'Documentation analysis: do blockading forces document civilian impact assessments? Evidence of intentionality from military communications or policy statements. Comparative analysis: does mortality rate exceed what would occur from military operations alone?',
    'If side effect: extractiveness remains high but suppression may be reframed as war cost rather than extraction. If deliberate: snare classification confirmed with maximal moral weight — extraction is explicitly coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_death_extraction_mechanism, empirical, 'Whether civilian mortality is side effect or deliberate extraction').

omega_variable(
    black_market_threshold_sustainability,
    'At what blockade enforcement intensity does black-market supply become structurally unable to sustain civilian population?',
    'Economic modeling: ratio of black-market supply capacity to population demand; comparison across historical blockades (Leningrad, Warsaw Ghetto, Gaza, Yemen). Identification of critical enforcement thresholds.',
    'If black markets can sustain population indefinitely: exit option exists (constrained rather than trapped). If black markets collapse under enforcement: trapped classification confirmed, snare deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_threshold_sustainability, empirical, 'Black-market sustainability threshold').

omega_variable(
    humanitarian_organization_mission_compromise,
    'Does humanitarian organization participation in blockade-managed access constitute aid delivery or legitimization of extraction?',
    'Post-conflict analysis: do humanitarian organizations'' presence reduce net harm or enable blockade persistence by providing veneer of legitimacy? Organizational exit analysis: what happens if organizations withdraw?',
    'If organizations reduce harm: tangled rope justified (genuine coordination function alongside extraction). If organizations enable extraction: classification should shift toward snare for organizations (their exit option is mobile/higher power, but their choice to participate extends blockade).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_organization_mission_compromise, conceptual, 'Whether humanitarian participation legitimizes blockade').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humanitarian_access_blockade, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humanitarian_access_blockade, theater_ratio, 0, 0.35).
narrative_ontology:measurement(huma_tr_t6, humanitarian_access_blockade, theater_ratio, 6, 0.42).
narrative_ontology:measurement(huma_tr_t12, humanitarian_access_blockade, theater_ratio, 12, 0.45).
narrative_ontology:measurement(huma_tr_t18, humanitarian_access_blockade, theater_ratio, 18, 0.48).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humanitarian_access_blockade, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(huma_be_t6, humanitarian_access_blockade, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(huma_be_t12, humanitarian_access_blockade, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(huma_be_t18, humanitarian_access_blockade, base_extractiveness, 18, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humanitarian_access_blockade, resource_allocation).
narrative_ontology:affects_constraint(humanitarian_access_blockade, territory_control_mechanism).
narrative_ontology:affects_constraint(humanitarian_access_blockade, organized_resistance_suppression).

% DUAL FORMULATION NOTE:
% The humanitarian access blockade is upstream of both territory control mechanisms (which depend on blockade enforcement) and organized resistance suppression (which depends on blockade's denial of supply lines). The blockade is a structural mechanism enabling both parent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humanitarian_access_blockade, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
