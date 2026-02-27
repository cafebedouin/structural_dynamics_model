% ============================================================================
% CONSTRAINT STORY: olympic_legacy_curling_investment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_olympic_legacy_curling_investment, []).

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
 *   constraint_id: olympic_legacy_curling_investment
 *   human_readable: Olympic Games Legacy Investment in Curling Clubs
 *   domain: economic/sports_infrastructure
 *
 * SUMMARY:
 *   Olympic legacy investment in curling clubs represents a structural
 *   constraint that combines genuine coordination benefits (infrastructure
 *   improvement, international competition pathways for elite athletes) with
 *   extractive mechanisms (displacement of recreational users, resource
 *   concentration on narrow elite programs, unsustainable municipal
 *   maintenance burdens). The constraint emerges from the tension between the
 *   Olympic movement's legacy commitment rhetoric and the actual incentive
 *   structure of post-Games investment allocation. Elite curling programs
 *   capture disproportionate resources because they are directly linked to
 *   medal performance and international visibility, while recreational
 *   curling—which uses the same infrastructure—experiences displacement and
 *   cost inflation. The theater ratio rises over time as initial investment
 *   ceremony gives way to the reality of sustained maintenance failures and
 *   recreational exclusion. This is a diagnostic case for understanding how
 *   legitimate coordination mechanisms can be layered with extraction: the
 *   infrastructure genuinely enables elite development AND genuinely
 *   displaces recreational access. Both effects are real; the constraint's
 *   classification depends on which agent's perspective is privileged.
 *
 * KEY AGENTS:
 *   - Elite Curling Programs: Primary beneficiary (institutional/arbitrage) — capture dedicated funding, elite facility access, athlete stipends; experience constraint as pure coordination
 *   - Recreational Curling Participants: Primary victim (powerless/trapped) — lose ice-time access and pay higher fees; cannot exit without abandoning sport locally
 *   - Provincial Curling Federation: Secondary institutional actor (organized/constrained) — mandate to serve both elite and recreational; constrained by non-Olympic winter sports competition for resources
 *   - Host City Sports Authority: Secondary institutional actor (organized/constrained) — receives legacy investment and visibility; trapped by long-term maintenance cost burden
 *   - Non-Olympic Winter Sports Communities: Indirect victim (moderate/mobile) — compete for legacy infrastructure funding and arena scheduling; higher exit optionality than recreational curlers
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing Olympic resource concentration as inevitable law of attention economics rather than policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_legacy_curling_investment, 0.52).
domain_priors:suppression_score(olympic_legacy_curling_investment, 0.48).
domain_priors:theater_ratio(olympic_legacy_curling_investment, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, extractiveness, 0.52).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(olympic_legacy_curling_investment, tangled_rope).
narrative_ontology:human_readable(olympic_legacy_curling_investment, "Olympic Games Legacy Investment in Curling Clubs").
narrative_ontology:topic_domain(olympic_legacy_curling_investment, "economic/sports_infrastructure").

domain_priors:requires_active_enforcement(olympic_legacy_curling_investment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, elite_curling_programs).
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, host_city_sports_authorities).
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, recreational_curling_participants).
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, non_olympic_winter_sports).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECREATIONAL CURLER (SNARE) — Local recreational curling clubs depend on ice time and facility access. Post-Olympic legacy funding flows to elite programs and facility upgrades targeted at competition training, displacing recreational league schedules. Recreational members cannot exit the constraint without abandoning the sport locally; ice time becomes commodified for elite use. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROVINCIAL CURLING FEDERATION (TANGLED ROPE) — Benefits from legacy infrastructure investment and increased grassroots participation interest following Olympic visibility. Simultaneously trapped by mandate to serve both elite and recreational curlers with limited resources, and constrained by competition for non-Olympic winter sports funding. Experiences both coordination (sharing elite-development pathways) and extraction (elite programs capturing disproportionate resources).
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE NATIONAL CURLING PROGRAM (ROPE) — Direct beneficiary of Olympic legacy investment. Captures dedicated funding, upgraded facilities, athlete stipends, and international competition opportunities. Experiences the constraint as pure coordination: legacy funding enables national team development and international competitiveness. High exit optionality — program can access alternative funding sources (sponsorships, international federation support). Net beneficiary with minimal extraction cost.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HOST CITY SPORTS AUTHORITY (TANGLED ROPE) — Receives legacy investment mandate and visibility boost from hosting Olympics. Simultaneously constrained by requirement to maintain facilities at elevated operational cost beyond initial funding period, and extraction of local tax revenue and facility access for elite programs. Coordination function: legacy infrastructure becomes multi-use community asset. Extraction function: long-term maintenance burden on municipal budget; elite programs reduce recreational access windows.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OLYMPIC LEGACY COMMITMENT RITUAL (PITON) — The public commitment to invest in host-sport infrastructure following the Olympics has become largely performative. Initial funding is visible and symbolic; long-term maintenance and actual grassroots integration often fail or fade. The ritual persists (Olympic Agenda 2020 legacy commitments, IOC partnership pledges) despite poor historical track record of sustained investment. Theater ratio high: ceremony and ribbon-cuttings precede infrastructure sustainability. Institutional inertia maintains the commitment despite degraded function.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, the concentration of sports resources on Olympic sports is an inevitable consequence of attention economics: Olympic visibility generates funding, and funding follows visibility. This appears as a natural law of resource allocation under attention scarcity. However, the structural data reveals this as naturalization: Olympic funding concentration is a policy choice, not a law of nature. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(olympic_legacy_curling_investment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(olympic_legacy_curling_investment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(olympic_legacy_curling_investment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(olympic_legacy_curling_investment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(olympic_legacy_curling_investment, TR),
    TR >= 0.70.

:- end_tests(olympic_legacy_curling_investment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. Initial post-Games investment (t=0, ε=0.28) appears as pure coordination—facilities are upgraded, programs expand, participation surges. Over 3-5 years (t=3, ε=0.42), extraction becomes visible: recreational ice-time shrinks, user fees rise, maintenance burden falls on local taxpayers. By year 7 (ε=0.52), the constraint has stabilized as Tangled Rope—genuine coordination function (elite pathways, infrastructure quality) persists alongside clear extraction (recreational displacement, municipal cost burden). Suppression (0.48): Moderate. Recreational curlers cannot exit without leaving the sport, but their suppression is not absolute—some communities maintain dual-track facilities or negotiate ice-time sharing. Elite programs have high exit optionality (sponsorships, federation support) while recreational communities have low optionality. Theater ratio (0.62): High and rising. Post-Games ceremony emphasizes 'legacy for all' and 'grassroots development,' but actual allocation concentrates resources on elite. Initial theater (t=0, θ=0.35) reflects genuine facility expansion. Rising theater (t=7, θ=0.62) reflects divergence between promised legacy access and actual recreational displacement.
 *
 * PERSPECTIVAL GAP:
 *   Elite programs see Rope—infrastructure enables their development and represents minimal constraint. Recreational curlers see Snare—they bear full displacement cost with no exit. Provincial federations see Tangled Rope—they must serve both constituencies with constrained resources and cannot arbitrage between commitments. Host city sees Tangled Rope—benefits from initial investment visibility but trapped by maintenance burden. The Olympic ritual sees Piton—legacy commitments are maintained ceremonially despite poor sustained outcomes. The analytical observer risks seeing Mountain—treating Olympic resource concentration as inevitable—but the structural data (rising theater, rising extractiveness over time, clear beneficiary/victim structure) contradicts natural law classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Elite programs (institutional/arbitrage) have the lowest d value: they are primary beneficiaries with high exit optionality (sponsorship, federation alternatives), deriving negative effective extraction (χ < 0). Recreational curlers (powerless/trapped) have maximum d: they are victims with zero exit options, experiencing high effective extraction despite moderate base extractiveness ε. Provincial federations and host cities (organized/constrained) occupy the middle: they benefit from initial investment but are constrained by long-term obligations, producing d ≈ 0.50-0.55 and moderate χ. The constraint's Tangled Rope classification emerges because: (1) there are genuine beneficiaries (elite programs) with coordination benefits (infrastructure, pathways), (2) there are clear victims (recreational users, municipal budgets), (3) active enforcement is required to maintain resource allocation favoring elites, and (4) suppression of recreational alternatives is substantial (0.48) but not total.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the Tangled Rope classification is defensible: genuine coordination (elite athlete development, infrastructure improvement) coexists with asymmetric extraction (displacement of recreational users). The Snare perspective from the recreational user is also valid—they experience pure extraction. The Rope perspective from elite programs is also valid—they experience only coordination. The mandatrophy resolution is perspectival: there is no single 'correct' type. The constraint IS a Tangled Rope at the system level (mixed coordination and extraction coexist in the same mechanism), while simultaneously being experienced as Snare by some agents and Rope by others. The theater ratio rising from 0.35 to 0.62 indicates that the 'legacy for all' framing increasingly misrepresents actual allocation patterns—Goodhart drift where the performance metric (legacy commitment) decouples from the actual function (equitable access).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legacy_maintenance_sustainability,
    'What percentage of Olympic legacy curling infrastructure remains actively maintained and accessible 10+ years post-Games?',
    'Post-Games facility audits; comparison of maintenance budgets to initial legacy commitments; tracking of ice-time allocation across user groups over time',
    'If > 80% sustained: legacy investment functions as promised (Rope/Scaffold from more perspectives). If < 50% sustained: legacy investment is primarily symbolic extraction (Snare/Piton confirmed). If 50-80%: genuine mixed coordination-extraction (Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legacy_maintenance_sustainability, empirical, 'Percentage of Olympic legacy curling infrastructure sustained 10+ years post-Games').

omega_variable(
    recreational_displacement_timing,
    'How quickly do recreational curling programs experience ice-time displacement and cost increases following Olympic facility upgrades?',
    'Tracking of recreational league participation rates, ice-time availability, and user fees in 5-year windows before and after Games; direct interviews with recreational club operators',
    'If displacement occurs within 1-2 years post-Games: snare classification confirmed for recreational users. If displacement is gradual (5+ years): victims have exit options and tangled rope is more accurate. If no displacement: constraint is rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recreational_displacement_timing, empirical, 'Timeline of recreational displacement following Olympic facility upgrades').

omega_variable(
    elite_athlete_long_term_outcomes,
    'Do Olympic legacy curling investments demonstrably increase long-term elite athlete performance and international medal outcomes?',
    'Comparison of elite curling performance trajectories in host vs non-host nations post-Games; attribution analysis for improvements in international standings',
    'If strong correlation: legacy investment is genuine coordination (Rope/Scaffold confirmed for elite perspective). If weak/absent: investment is primarily extraction and theatrical display (Snare/Piton confirmed). If mixed: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_athlete_long_term_outcomes, empirical, 'Whether Olympic legacy investments improve elite curling outcomes').

omega_variable(
    grassroots_participation_surge_sustainability,
    'Does the post-Olympic ''participation surge'' in curling persist beyond 3-5 years, or is it a transient spike that reverts to baseline?',
    'Longitudinal tracking of junior curling enrollment, club membership, and facility usage across multiple Olympic cycles; comparison with non-host regions',
    'If sustained: legacy investment has genuine coordination function for grassroots (Rope/Scaffold confirmed). If transient: investment is extraction with theatrical ''legacy'' framing (Snare/Piton confirmed). Affects whether beneficiaries include ''grassroots_curling_access'' or only ''elite_programs''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grassroots_participation_surge_sustainability, empirical, 'Whether Olympic-driven participation surge in curling sustains beyond 5 years').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(olympic_legacy_curling_investment, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(olcurl_tr_t0, olympic_legacy_curling_investment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(olcurl_tr_t3, olympic_legacy_curling_investment, theater_ratio, 3, 0.55).
narrative_ontology:measurement(olcurl_tr_t7, olympic_legacy_curling_investment, theater_ratio, 7, 0.62).

% Extraction over time
narrative_ontology:measurement(olcurl_be_t0, olympic_legacy_curling_investment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(olcurl_be_t3, olympic_legacy_curling_investment, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(olcurl_be_t7, olympic_legacy_curling_investment, base_extractiveness, 7, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(olympic_legacy_curling_investment, resource_allocation).
narrative_ontology:affects_constraint(olympic_legacy_curling_investment, olympic_host_infrastructure_debt).
narrative_ontology:affects_constraint(olympic_legacy_curling_investment, elite_sports_funding_concentration).
narrative_ontology:affects_constraint(olympic_legacy_curling_investment, recreational_access_displacement).

% DUAL FORMULATION NOTE:
% Olympic legacy curling investment decomposes into three distinct constraints: (1) the infrastructure coordination mechanism (moderate ε, genuine Rope), (2) the recreational displacement mechanism (high ε, genuine Snare), and (3) the municipal debt burden (moderate ε, genuine Tangled Rope for cities). This story treats the system-level constraint; the three downstream constraints capture the specific mechanisms from different structural positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
