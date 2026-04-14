% ============================================================================
% CONSTRAINT STORY: roman_colosseum_games
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The Roman Colosseum games (panis et circenses, 'bread and circuses')
 *   exemplify a constraint system that oscillates between genuine
 *   coordination and pure extraction. The games functioned simultaneously as
 *   military training mechanism, political legitimation apparatus, crowd
 *   management tool, and economic extraction engine. From the imperial
 *   perspective, spectacles solved genuine collective action problems: how to
 *   bind a militarized autocracy's diverse populations into a shared polity,
 *   how to channel violent impulses and surplus military capacity into
 *   controlled venues, how to distribute legitimacy to a regime lacking
 *   electoral consent. From the enslaved and condemned perspective, the games
 *   were pure snare — extraction toward death with zero exit options. From
 *   the plebeian perspective, the games appeared as participation (cheering
 *   crowd, imperial responsiveness to applause) but operated as suppressed
 *   extraction (resource redirection, political agency channeling, loyalty
 *   enforcement). Over the 400-year interval from early imperial expansion
 *   (roughly 50 BCE) to late empire fragmentation (350 CE), the constraint's
 *   functional content degraded while its theater increased: military
 *   training detached from martial utility, animal spectacles shifted from
 *   strategic resource management to exotic consumption, and crowd management
 *   devolved from political negotiation to faction control. By the late
 *   empire, the games were maintained primarily through institutional inertia
 *   — the residual spectacle institution persisting because alternative
 *   legitimation pathways had not yet crystallized. This progression from
 *   tangled_rope (mixed coordination-extraction) toward piton (degraded
 *   theater) is the constraint's primary temporal feature.
 *
 * KEY AGENTS:
 *   - Imperial Administration: Primary beneficiary (institutional/arbitrage) — captures legitimacy monopoly, crowd consolidation, and political power concentration through spectacle sponsorship
 *   - Gladiators and Condemned Prisoners: Primary victim (powerless/trapped) — extracted toward certain death; zero exit options; no coordination benefit
 *   - Urban Plebeian Population: Secondary victim (moderate/constrained) — subjected to resource extraction (spectacle funding), political agency channeling (crowd acclamation), and loyalty enforcement; constrained exit options
 *   - Military Officer Corps: Mixed (organized/constrained) — benefits from military training utility and career advancement through patronage; also bears suppression costs (career penalties for questioning expenditures)
 *   - Aristocratic Elite: Secondary beneficiary (powerful/arbitrage) — can sponsor spectacles for prestige and political advancement; have superior exit options compared to plebeians
 *   - Residual Spectacle Institution: Institutional actor (institutional/arbitrage) — late-empire piton perspective; maintains games through inertia after functional justification erodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_colosseum_games, 0.58).
domain_priors:suppression_score(roman_colosseum_games, 0.68).
domain_priors:theater_ratio(roman_colosseum_games, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_colosseum_games, extractiveness, 0.58).
narrative_ontology:constraint_metric(roman_colosseum_games, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(roman_colosseum_games, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_colosseum_games, tangled_rope).
narrative_ontology:human_readable(roman_colosseum_games, "The Spectacle of the Roman Colosseum").
narrative_ontology:topic_domain(roman_colosseum_games, "political/social").

domain_priors:requires_active_enforcement(roman_colosseum_games).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_colosseum_games, imperial_administration).
narrative_ontology:constraint_beneficiary(roman_colosseum_games, aristocratic_elite).
narrative_ontology:constraint_beneficiary(roman_colosseum_games, military_apparatus).
narrative_ontology:constraint_victim(roman_colosseum_games, enslaved_populations).
narrative_ontology:constraint_victim(roman_colosseum_games, political_stability_commons).
narrative_ontology:constraint_victim(roman_colosseum_games, resource_allocation_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLADIATOR AND CONDEMNED (SNARE) — Enslaved combatants and condemned criminals have zero exit options. They are extracted toward certain death in exchange for fleeting glory or terror avoidance. The constraint operates via coercion: they cannot refuse participation; suppression is near-total. No coordination benefit to this agent — pure extraction toward maximum experienced extractiveness.
constraint_indexing:constraint_classification(roman_colosseum_games, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: URBAN PLEBEIANS (SNARE) — Free urban populace experiences high-extraction constraint disguised as benefit. Exit options are constrained: refusing spectacle attendance signals disloyalty; political participation is channeled exclusively through crowd acclamation in the arena. The games consume resources (grain allocations, labor, military capacity) that could fund basic services. The spectacle extracts political agency and redirects it toward regime legitimation. Suppression is high — bread supply is conditional on circuses.
constraint_indexing:constraint_classification(roman_colosseum_games, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MILITARY OFFICERS (TANGLED ROPE) — Officer corps experiences mixed extraction and coordination. The games serve genuine security function: managing surplus military capacity (gladiatorial training doubles as combat conditioning), channeling violent capacity away from civil conflict, and consolidating unit cohesion through shared spectacle attendance. Officers also extract prestige and promotion opportunities through sponsorship and patronage of games. Suppression is high (officers face career penalties for questioning spectacle expenditures), but coordination benefits are real — the games solve a collective action problem of violence management.
constraint_indexing:constraint_classification(roman_colosseum_games, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL ADMINISTRATION (ROPE) — From the perspective of the imperial apparatus, the Colosseum games function as pure coordination mechanism: distributing political legitimacy, managing social stability, concentrating attention, and channeling dissent into controlled venues. The administration benefits from first-mover advantage in spectacle production (monopoly on gladiatorial supply, animal imports, venue access). The games solve genuine coordination problems: how to maintain consent in a militarized autocracy, how to aggregate urban factions into a unified crowd, how to prevent rival power centers from organizing. Theater is high but serves coordination function. This perspective sees low effective extraction — the games are subsidized as public goods, not revenue sources.
constraint_indexing:constraint_classification(roman_colosseum_games, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RESIDUAL SPECTACLE INSTITUTION (PITON) — Over the interval (100 BCE to 400 CE), the games gradually transitioned from genuine military and political coordination mechanism to pure theater maintained by institutional inertia. By the late empire, gladiatorial training had detached from military utility, animal spectacles had become exotic consumer goods rather than civic infrastructure, and the arena's political function had devolved to faction managing (Blue vs Green riots). The theater ratio climbed from 0.50 (early empire: real coordination) to 0.85 (late empire: pure performance). The institution persisted through imperial sponsorship even after its functional justification eroded — classical piton signature. The final constraint's extractiveness value (0.58) represents the intermediate state; late-empire instantiation would be higher (0.75+).
constraint_indexing:constraint_classification(roman_colosseum_games, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURALIZATION (MOUNTAIN) — A naive analytical perspective might classify the games as natural law: 'Autocratic regimes naturally maintain control through spectacle; crowd management is an unchangeable feature of mass society.' This naturalizes what are contingent institutional choices (choice to fund spectacles, choice to concentrate dissent, choice to embed military training in entertainment). The framework detects this as a false summit: accessibility_collapse is 0.45 (many exit paths exist for alternative governance structures), resistance is 0.38 (the constraint is regularly challenged by reform movements), and it does NOT emerge naturally (it requires active enforcement and resource commitment). The mountain classification is a failure of analytical framing, not a property of the constraint.
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
    constraint_indexing:constraint_classification(roman_colosseum_games, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The games extract resources (grain allocations, military labor, craftspeople) that could be redirected to productive infrastructure or administration. They extract political agency by channeling participation into controlled crowd reactions. They extract competitive reputation by monopolizing the venue for public spectacle. However, extractiveness is not maximal (0.75+) because the games do solve genuine coordination problems: the imperial regime genuinely needs to manage military capacity, distribute legitimacy, and bind urban populations into a stable hierarchy. The extractiveness value reflects the mixed nature — significant extraction layered on real coordination function. Suppression (0.68): High. The constraint operates through severe barriers to refusal: attendance at spectacles is effectively mandatory for plebeians (absence signals disloyalty); gladiatorial participation is coerced (enslaved and condemned status); and alternative forms of political participation are suppressed (Senate is ceremonial; provincial councils lack real power). However, suppression is not absolute (0.85+) because some exit channels exist (provincial relocation for the wealthy, individual refusal for those accepting social cost, military service as alternative identity). Theater ratio (0.80): High and increasing over the interval. Traditional understanding treats spectacles as entertainment; analytical treatment reveals they are mechanisms for attention control and political aggregation. The theater increased over time as the genuine functional content (military training, emergency animal management) detached from the spectacles and entertainment function became primary.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The gladiator sees pure snare (snare classification, d ≈ 0.95). The plebeian sees snare disguised as participation (snare classification, d ≈ 0.85). The military officer sees mixed function (tangled_rope classification, d ≈ 0.55). The imperial administration sees coordination mechanism (rope classification, d ≈ 0.15). The late-empire spectacle institution sees its own degradation (piton classification, d ≈ 0.20, theater dominant). The naive analytical observer risks seeing natural law (false mountain, which the structural data rebuts). This perspectival gap reflects genuine structural differentiation in exit options and benefit distribution — the games really do function differently for enslaved combatants (no exit, pure extraction) versus imperial administrators (monopoly exit, coordination benefits) versus declining institutions (inertial theater). The gap is not observational or framing-dependent; it reflects real asymmetries in structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status combined with exit options. Enslaved gladiators: victims + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Plebeians: victims + constrained → d ≈ 0.80 → f(d) ≈ 1.20 → high extraction. Military officers: mixed (beneficiary from patronage, victim from suppression) + constrained → d ≈ 0.55 → f(d) ≈ 0.75 → moderate extraction. Imperial administration: beneficiaries + arbitrage → d ≈ 0.10 → f(d) ≈ -0.08 → negative extraction (they experience the games as subsidized public goods, not costs). The scope modifier σ(S) scales extractiveness by spatial scope: local scope (0.8) dampens, national scope (1.0) neutral, global scope (1.2) amplifies. The games operate at regional-to-national scope (arena in Rome, impact on empire-wide legitimation), so σ(S) ≈ 1.0. The beneficiary and victim structure maps clearly: imperial administration, aristocratic elite, military apparatus as beneficiaries; enslaved populations, plebeians, and the abstract political-stability commons as victims.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint is classified as tangled_rope at base (ε=0.58, suppression=0.68, genuine coordination function + asymmetric extraction), and mandatrophy is resolved by showing that the perspectival gap is legitimate structural differentiation, not misclassification. The imperial perspective (rope) and the gladiator perspective (snare) are NOT contradictory; they reflect genuinely different structural positions. The games really do provide coordination benefits to the regime (solving the 'how do we aggregate diverse subjects into stable hierarchy?' problem) and real extraction to enslaved combatants (captured toward death with no exit). Both are true. The tangled_rope classification captures this: it requires both a genuine coordination function (the games solve the empire's aggregation problem) AND asymmetric extraction (the solution is built on the backs of the enslaved and suppressed plebeian agency). The mandatrophy is further resolved by the temporal degradation: the games transition from 'mixed coordination-extraction' (early empire, 0.38 extractiveness) toward 'degraded theater' (late empire, 0.58 extractiveness, theater rising to 0.80). As the functional coordination content erodes and the institutional inertia dominates, the constraint approaches piton. The progression is observable, measurable, and theoretically coherent: tangled_rope → [degradation over 400 years] → piton. This is exactly the lifecycle the framework predicts for constraints that lose their coordination function while remaining institutionalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_legitimacy_boundary,
    'At what threshold does spectacle spending shift from legitimate political coordination to pure extraction disguised as public good?',
    'Historical analysis of spectacle expenditure as percentage of imperial budget correlated with civil unrest, military readiness, and political satisfaction metrics from period sources',
    'If threshold < 5% budget: early Rome classified as rope from administrative perspective. If threshold > 15%: extractive intent becomes undeniable even in administrative framing. Current assessment uses 8-12% as ambiguity zone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_legitimacy_boundary, empirical, 'Spectacle spending threshold distinguishing coordination from extraction').

omega_variable(
    plebeian_agency_in_games,
    'Did urban plebeians experience the games as coerced entertainment (snare) or as genuine political participation vehicle with real power to influence outcomes (rope)?',
    'Analysis of period texts (Suetonius, Cassius Dio, amphitheater inscriptions) for evidence of crowd-directed outcomes, imperial concessions to crowd demands, and subjective framing of attendance',
    'If genuine participation: plebeian perspective shifts from snare to tangled_rope. If purely performative: snare classification confirmed, and suppression value increases to 0.78+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_agency_in_games, conceptual, 'Whether plebeian crowd participation was genuine political agency or pure theater').

omega_variable(
    military_utility_attestation,
    'Did gladiatorial training and spectacle attendance genuinely serve military conditioning and unit cohesion functions, or was this a post-hoc rationalization for entertainment expenditure?',
    'Comparative analysis of military readiness metrics (campaign success rates, unit cohesion in civil crises) for regimes with high vs low spectacle spending; correlation with officer training methodologies',
    'If genuine utility: military perspective tangled_rope classification confirmed. If purely rationalized: military perspective shifts to rope or even piton, and overall constraint extractiveness increases (loses mixed-benefit defense).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_utility_attestation, empirical, 'Whether gladiatorial spectacle served genuine military functions').

omega_variable(
    institutional_inertia_timeline,
    'When did the Colosseum games transition from functional coordination mechanism to inertial theater maintained by tradition and imperial patronage?',
    'Periodization of functional shift: analyzing evolution of gladiatorial training detachment from military curriculum, shift from utility animals (war elephants, lions) to exotic spectacle animals, and rise of faction-based crowd management',
    'Early transition (1st-2nd cent): piton classification applies to most of late empire. Late transition (3rd cent+): tangled_rope persists longer. Affects assessment of whether constraint was inherently extractive or became extractive through degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_inertia_timeline, empirical, 'Timeline of transition from functional coordination to institutional theater').

omega_variable(
    alternative_legitimation_pathways,
    'Could the imperial regime have maintained comparable political stability without gladiatorial spectacles, using alternative coordination mechanisms (Senate engagement, provincial devolution, military reforms)?',
    'Comparative analysis of non-spectacle regimes (Hellenistic monarchies, later Byzantine system, Islamic caliphates) and their stability metrics relative to Rome',
    'If viable alternatives exist: games are extractive choice, not coordination necessity. If games were uniquely efficient: coordination framing strengthened, extractiveness reduced. Current assessment assumes viable alternatives, supporting tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_legitimation_pathways, preference, 'Whether alternatives to spectacle-based legitimation were viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_colosseum_games, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colosseum_tr_t0, roman_colosseum_games, theater_ratio, 0, 0.5).
narrative_ontology:measurement(colosseum_tr_t50, roman_colosseum_games, theater_ratio, 50, 0.65).
narrative_ontology:measurement(colosseum_tr_t100, roman_colosseum_games, theater_ratio, 100, 0.8).

% Extraction over time
narrative_ontology:measurement(colosseum_be_t0, roman_colosseum_games, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(colosseum_be_t50, roman_colosseum_games, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(colosseum_be_t100, roman_colosseum_games, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_colosseum_games, enforcement_mechanism).
narrative_ontology:affects_constraint(roman_colosseum_games, roman_military_capacity_management).
narrative_ontology:affects_constraint(roman_colosseum_games, plebeian_political_participation_systems).
narrative_ontology:affects_constraint(roman_colosseum_games, imperial_legitimacy_apparatus).

% DUAL FORMULATION NOTE:
% The Colosseum games constraint can be decomposed into three structural claims: (1) Military training and capacity management (genuine coordination, lower ε), (2) Crowd aggregation and political legitimacy distribution (mixed coordination-extraction, moderate ε ≈ 0.50), and (3) Entertainment consumption and institutional theater (pure performance, higher ε ≈ 0.70). This story models the integrated constraint at the intermediate level (ε=0.58, claimed_type=tangled_rope) capturing all three functions. Decomposition into separate constraint stories would separate the military function (rope), the political-legitimacy function (tangled_rope), and the residual-theater function (piton), but the integrated system operates through coupling of all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
