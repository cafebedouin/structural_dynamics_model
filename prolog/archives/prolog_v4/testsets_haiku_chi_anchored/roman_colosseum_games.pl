% ============================================================================
% CONSTRAINT STORY: roman_colosseum_games
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_colosseum_games, []).

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
 *   constraint_id: roman_colosseum_games
 *   human_readable: The Spectacle of the Roman Colosseum
 *   domain: political/social
 *
 * SUMMARY:
 *   The Roman Colosseum games represent a paradigmatic example of a pure
 *   extraction constraint masked as public coordination. From 72 CE
 *   (construction) through the 4th century, the games functioned
 *   simultaneously as a legitimacy mechanism for the imperial elite, a
 *   pacification tool for the urban plebeian population, and a
 *   resource-extraction machine operating on enslaved persons and subjugated
 *   provinces. The 'bread and circuses' (panem et circenses) arrangement
 *   created a cycle of political suppression: the state subsidized grain and
 *   funded spectacle, the population remained sedated and politically
 *   neutral, and resources from conquered territories flowed toward arena
 *   maintenance and slave acquisition. Over the constraint's lifespan, the
 *   theater ratio rises (games become increasingly performative as functional
 *   control mechanisms decline), while extractiveness remains stable despite
 *   diminishing marginal returns—a signature of institutional inertia and
 *   piton degradation. The constraint reaches its peak visibility around 150
 *   CE (mid-imperial period) and begins a slow theatricalization through late
 *   antiquity as military crises and resource scarcity reduce the state's
 *   capacity to maintain the spectacle, yet cultural investment in the games
 *   persists through momentum.
 *
 * KEY AGENTS:
 *   - Enslaved Gladiators: Primary victims (powerless/trapped) — conscripted from war captives, condemned criminals, or purchased; forced into combat with near-certain death; no legal recourse or alternative
 *   - Urban Plebeian Population: Primary victims (powerless/trapped) — sedated by grain subsidy and spectacle; politically neutralized; cannot exit the constraint or organize collective action
 *   - Subjugated Provinces: Secondary victims (powerful/trapped) — forced to supply enslaved persons, tribute animals, and economic resources; no alternative but rebellion (costly and suppressed)
 *   - Imperial Elite: Primary beneficiary (institutional/arbitrage) — captures political legitimacy, prestige, patronage distribution, and consolidation of power; experiences games as coordination mechanism for empire maintenance
 *   - State Administration: Secondary beneficiary (institutional/arbitrage) — manages resource flows and public order; maintains the spectacle infrastructure; benefits from plausible deniability ('this is entertainment, not control')
 *   - Analytical Observer: Institutional position (analytical/analytical) — observes the constraint's structure from outside; risks naturalizing the arrangement as inherent to empire or human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_colosseum_games, 0.68).
domain_priors:suppression_score(roman_colosseum_games, 0.72).
domain_priors:theater_ratio(roman_colosseum_games, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_colosseum_games, extractiveness, 0.68).
narrative_ontology:constraint_metric(roman_colosseum_games, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(roman_colosseum_games, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_colosseum_games, snare).
narrative_ontology:human_readable(roman_colosseum_games, "The Spectacle of the Roman Colosseum").
narrative_ontology:topic_domain(roman_colosseum_games, "political/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_colosseum_games, imperial_elite).
narrative_ontology:constraint_victim(roman_colosseum_games, enslaved_combatants).
narrative_ontology:constraint_victim(roman_colosseum_games, subjugated_provinces).
narrative_ontology:constraint_victim(roman_colosseum_games, urban_plebeian_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED GLADIATOR (SNARE) — Captured, purchased, or condemned to combat arena. Zero exit options; death is the likely outcome. Extracted from without consent or compensation. d≈0.98, f(d)≈1.48, σ=0.9 → χ≈0.96. Maximum extraction under pure coercion.
constraint_indexing:constraint_classification(roman_colosseum_games, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN PLEBEIAN POPULATION (SNARE) — Sedated by spectacle and subsistence grain dole ('bread and circuses'). Politically neutralized through mandatory entertainment and attention capture. Cannot exit the constraint or organize alternative governance. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.94. Extraction through suppression of political consciousness and agency.
constraint_indexing:constraint_classification(roman_colosseum_games, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SUBJUGATED PROVINCE (SNARE) — Supplies enslaved persons, tribute, and spectacle animals (lions, bears) for arena use. Extraction is political (forced provision of captives as entertainment), economic (resource drain), and symbolic (proof of Rome's dominance over foreign peoples). d≈0.88, f(d)≈1.30, σ=0.9 → χ≈0.79. High extraction with military enforcement.
constraint_indexing:constraint_classification(roman_colosseum_games, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: IMPERIAL ELITE (ROPE) — Games provide coordination mechanism: public spectacle unifies Rome's diverse population under imperial ideology, channels surplus wealth into public infrastructure (the Colosseum itself serves as symbol of Rome's power), and distributes patronage/prestige to elite sponsors. From this perspective, games are coordination that solves collective action problem of maintaining empire. d≈0.02, f(d)≈-0.18, σ=1.0 → χ≈-0.12. Net subsidy to the elite through coordination benefits.
constraint_indexing:constraint_classification(roman_colosseum_games, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL OBSERVER (PITON) — By late empire (3rd-4th centuries), games persist primarily through theater and institutional inertia despite massive resource drain and declining military capacity. Elite no longer see games as necessary coordination; they persist because alternatives for public legitimacy (spectacle, tradition, architectural monumentality) have atrophied but not been replaced. theater_ratio=0.81 reflects that much of the spectacle has become performative legitimacy-theater rather than functional governance. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.12, but piton gate (theater≥0.70) fires.
constraint_indexing:constraint_classification(roman_colosseum_games, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN — FALSE SUMMIT) — Risk of naturalizing games as inevitable feature of empire or human nature ('all societies need spectacle'). However, base properties (ε=0.68, suppression=0.72, theater=0.81) violate mountain gates. Engine flags as false summit: games are contingent institutional arrangement, not immutable law. Analyzes the naturalization attempt itself as part of the constraint's reproductive logic.
constraint_indexing:constraint_classification(roman_colosseum_games, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_colosseum_games_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_colosseum_games, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_colosseum_games, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_colosseum_games, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_colosseum_games, TR),
    TR >= 0.70.

:- end_tests(roman_colosseum_games_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High. The constraint extracts resources from three victim groups simultaneously—enslaved combatants lose their lives and labor; plebians lose political agency and productive economic output (tied to grain subsidy rather than wages); provinces lose military manpower, animals, and tribute. The extraction is stable (~0.58→0.68 over 300 years) despite declining functional necessity, indicating institutional entrenchment. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) gladiators have no legal personhood and no exit; (2) plebians are sedated by grain and spectacle, reducing political consciousness; (3) provinces face military suppression if they resist. Suppression is structural (lack of alternatives) and active (enforcement). Theater ratio (0.81): High and rising. In the early imperial period (0-150 CE), games may have served genuine coordination functions (coordinating elite patronage networks, creating public spectacle for mass legitimacy). By late antiquity (250-400 CE), games persist primarily through institutional inertia and performative investment in tradition, with minimal functional contribution to governance. The rising theater ratio reflects Goodhart drift: the metric of 'successful spectacle' (crowd size, expense, elaborateness) replaces the original goal (political pacification via distraction). Resources are expended not because games effectively control populations but because the tradition persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. Enslaved gladiators and plebians experience it as pure snare (trapment, extraction, zero alternatives). Subjugated provinces experience snare (military enforcement, resource extraction, no recourse). The imperial elite experience rope (coordination mechanism, legitimacy, prestige). The late-imperial observer sees piton (degraded, theatrical, inertial). The analytical observer risks false mountain (naturalizing spectacle as universal human need). The gap between the beneficiary's 'coordination' reading and the victim's 'extraction' reading is fundamental: the same institution appears as a solution from above (legitimacy, unified empire) and as a problem from below (pacification, suppression). The snare classification dominates because the victims vastly outnumber the beneficiaries and experience totalizing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Enslaved gladiators: Victim + trapped → d≈0.98, f(d)≈1.48. Maximum extraction. Complete lack of alternatives; certain death or survival as permanent captive. Plebians: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction. Sedated by grain, politically disabled; sedition is rare and crushed. Subjugated provinces: Victim + trapped (despite military power) → d≈0.88, f(d)≈1.30. High extraction. Military enforcement ensures compliance; rebellion is option but costs exceed benefits. Imperial elite: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.18. Net subsidy. Experiences games as coordination that solves the problem of maintaining legitimacy and preventing elite factionalism. State administration: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net subsidy. Derives authority and organizational coherence from spectacle administration. Historical observer (late empire): Institutional + arbitrage, but theater_ratio high → piton gate fires despite low d. Late empire sees the spectacle as inertial (persists through tradition, not function).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED (ε=0.68 > 0.70 threshold requires resolution): The constraint resolves the mandatrophy by explicitly differentiating between the elite's genuine coordination function (games do coordinate imperial factions and distribute prestige) and the victims' structural extraction (games extract labor, life, agency, and resources without compensation or alternatives). The resolution hinges on the observation that the coordinate function is *real* (elites genuinely benefit from the legitimacy and prestige) but *asymmetric*—only a tiny population benefits while the vast majority bears costs. This is the definition of a snare: the constraint has a real function, but the function is distributed so asymmetrically that it amounts to pure extraction for the victimized population. The rope perspective (elite view) and snare perspective (victim view) are not contradictory; they are structurally accurate descriptions of the same institution from opposite positions. The mandate is resolved by showing that 'coordination' and 'extraction' are not mutually exclusive—the same institution can coordinate one population while extracting from another. The imperial elite are coordinating their own legitimacy and power; the gladiators and plebians are experiencing pure extraction. Mandatrophy does not require a single classification; it requires accurate perspectival differentiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grain_dole_dependency,
    'Did grain subsidy enable political pacification or vice versa? Did games and bread arrive together as a coordinated suppression strategy, or did games become central to legitimacy because grain proved insufficient for control?',
    'Chronological analysis of grain dole initiation vs. Colosseum construction (Colosseum: 72-80 CE; large-scale grain dole: earlier Gracchi reforms ~130 BCE, regularized later). Correlation analysis of grain availability fluctuations with spectacle frequency and intensity.',
    'If games created dependency: strong snare (pure extraction). If games responded to grain scarcity: tangled rope (coordination + extraction mix). Classification sensitivity: high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grain_dole_dependency, empirical, 'Whether grain dole and spectacle functioned as coordinated suppression or independent mechanisms').

omega_variable(
    elite_coordination_necessity,
    'Were games functionally necessary for elite coordination and legitimacy, or were they performative luxury that elite maintained through path dependence?',
    'Comparative analysis: did empires without spectacle infrastructure (Parthian, Chinese dynasties) face equivalent legitimacy crises? Did Roman elites maintain games when military/economic crises would have allowed redirection of resources?',
    'If functionally necessary: rope perspective is accurate, and classification shifts toward mixed tangled_rope (coordination + extraction). If performative: piton perspective is accurate (institutional inertia, theater), and ε should be lower (games are theater, not functional extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_coordination_necessity, conceptual, 'Whether games were functionally necessary for elite coordination').

omega_variable(
    subjugated_populations_exit_option,
    'Did subjugated provinces have any meaningful exit option short of open rebellion, or was the constraint truly totalizing?',
    'Historical analysis of provincial tax resistance, depopulation, bandit activity, and organized rebellion rates. Correlation with spectacle demand (more resources demanded → more resistance). Analysis of whether provinces could refuse gladiator supply without military intervention.',
    'If some exit option existed: provinces classify as constrained rather than trapped; d lowers from 0.88 to ~0.70; χ drops to ~0.65. If truly trapped: snare classification confirmed. Affects assessment of whether constraint achieves suppression through lack of alternatives (snare) or through active enforcement (potentially tangled_rope if elites claim coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjugated_populations_exit_option, empirical, 'Whether subjugated provinces had meaningful exit options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_colosseum_games, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colosseum_tr_t0, roman_colosseum_games, theater_ratio, 0, 0.55).
narrative_ontology:measurement(colosseum_tr_t150, roman_colosseum_games, theater_ratio, 150, 0.72).
narrative_ontology:measurement(colosseum_tr_t300, roman_colosseum_games, theater_ratio, 300, 0.81).

% Extraction over time
narrative_ontology:measurement(colosseum_be_t0, roman_colosseum_games, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(colosseum_be_t150, roman_colosseum_games, base_extractiveness, 150, 0.65).
narrative_ontology:measurement(colosseum_be_t300, roman_colosseum_games, base_extractiveness, 300, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_colosseum_games, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_colosseum_games, roman_grain_dole).
narrative_ontology:affects_constraint(roman_colosseum_games, slavery_roman_economy).
narrative_ontology:affects_constraint(roman_colosseum_games, imperial_legitimacy_crisis).

% DUAL FORMULATION NOTE:
% The Colosseum games are downstream of the grain dole (bread component) and of slavery as an economic system (circuses component). Upstream: the state's need for legitimacy and population control. Downstream: the degradation of games into performative theater by late antiquity. The network links reflect that games cannot be understood in isolation from their resource dependencies and political functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
