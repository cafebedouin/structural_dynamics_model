% ============================================================================
% CONSTRAINT STORY: olympic_legacy_curling_investment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   domain: economic/sports_policy
 *
 * SUMMARY:
 *   Post-Olympic legacy investment in curling creates a structural tension
 *   between elite athlete development and grassroots sport participation.
 *   Following a Winter Olympics, host nations typically receive dedicated
 *   funding streams for featured sports like curling, creating a 5-8 year
 *   window of elevated investment. This constraint operates as both a
 *   coordination mechanism (enabling elite competitive development and
 *   infrastructure modernization) and an extraction mechanism (concentrating
 *   funding in elite pathways, reducing accessibility for grassroots players,
 *   and creating unsustainable facility cost structures that disadvantage
 *   non-Olympic communities). The constraint exhibits a classic tangled rope
 *   structure: it solves a real coordination problem (how to build
 *   world-competitive curling teams) while simultaneously extracting from
 *   grassroots participation by directing capital and governance attention
 *   toward elite pathways. The theater ratio (0.58) reflects that
 *   post-Olympic legacy reporting is substantially performative —
 *   participation statistics and facility metrics are highlighted, but the
 *   concentration of investment in elite centers and the decline in
 *   grassroots club accessibility are downplayed or absent from official
 *   legacy narratives.
 *
 * KEY AGENTS:
 *   - Grassroots Curlers: Primary victims (powerless/trapped) — local club players face reduced facility access, higher costs, and community club closures as funding concentrates in elite centers
 *   - Elite Curling Athletes: Primary beneficiaries (organized/arbitrage) — receive enhanced training infrastructure, coaching, and pathway support during post-Olympic window
 *   - Olympic Organizing Committees: Secondary beneficiary (institutional/arbitrage) — benefit from legacy narrative and measurable sport development metrics justifying Olympic spending
 *   - National Curling Federation: Organized coordinator (organized/constrained) — enforces Olympic investment prioritization; constrained by governance requirements tied to funding
 *   - Regional Sports Councils: Powerful secondary actors (powerful/mobile) — can redirect legacy funds or advocate for dual-pathway investment; can exit legacy constraints if political will materializes
 *   - Local Curling Clubs: Victim organizations (powerless/trapped) — face facility closures, membership migration to elite centers, and funding reallocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(olympic_legacy_curling_investment, 0.38).
domain_priors:suppression_score(olympic_legacy_curling_investment, 0.42).
domain_priors:theater_ratio(olympic_legacy_curling_investment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, extractiveness, 0.38).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(olympic_legacy_curling_investment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(olympic_legacy_curling_investment, tangled_rope).
narrative_ontology:human_readable(olympic_legacy_curling_investment, "Olympic Games Legacy Investment in Curling Clubs").
narrative_ontology:topic_domain(olympic_legacy_curling_investment, "economic/sports_policy").

domain_priors:requires_active_enforcement(olympic_legacy_curling_investment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, elite_curling_athletes).
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, host_city_sports_infrastructure).
narrative_ontology:constraint_beneficiary(olympic_legacy_curling_investment, olympic_organizing_committees).
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, grassroots_curling_participation).
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, non_olympic_winter_sports).
narrative_ontology:constraint_victim(olympic_legacy_curling_investment, local_club_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRASSROOTS CURLER (SNARE) — Local club members are trapped by infrastructure and funding concentration. Post-Olympic investment flows to elite training centers and high-visibility facilities, while small neighborhood clubs lose capacity and membership. Cannot exit; forced to travel to elite venues or abandon the sport. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.43.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: OLYMPIC ORGANIZING COMMITTEE (ROPE) — Benefits from legacy narrative and political capital. Post-Olympic investment appears as pure coordination: funding curling creates measurable sport participation metrics and justifies Olympic spending to taxpayers. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary through political arbitrage.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NATIONAL CURLING FEDERATION (TANGLED ROPE) — Organized but constrained. Benefits from increased elite athlete funding and training infrastructure (coordination function), but trapped in enforcement of Olympic prioritization criteria that disadvantage non-elite pathways. Must maintain governance structure supporting post-Olympic investment streams. d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.25.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL SPORTS COUNCILS (SCAFFOLD) — Powerful actors with capacity to redirect legacy funds toward broader participation. See post-Olympic investment as temporary boost with a sunset: dedicated Olympic funding has explicit time windows (typically 4-8 years post-Games). Can leverage this window strategically to build sustainable grassroots infrastructure before funding contracts. d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.11.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY SPORT DEVELOPMENT BUREAUCRACY (PITON) — Maintains post-Olympic curling investment apparatus through institutional inertia. The legacy funding infrastructure persists long after the Games despite marginal effectiveness in increasing grassroots participation. theater_ratio=0.58 reflects substantial performative activity: progress reports, legacy committee meetings, and participation statistics mask the reality that funding concentration has degraded accessibility. The system continues because bureaucratic structures self-perpetuate, not because it optimally serves curlers.
constraint_indexing:constraint_classification(olympic_legacy_curling_investment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational economic perspective, post-Olympic investment concentration may appear as an inevitable consequence of capital allocation toward visible, media-relevant infrastructure. The 'sports follow Olympic glory' pattern appears universal across Games. However, structural data (ε=0.38, suppression=0.42, theater=0.58) contradicts mountain classification — this is a contingent policy choice, not an immutable law. The false summit reveals the naturalization of a specific investment model.
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
    constraint_indexing:constraint_classification(olympic_legacy_curling_investment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(olympic_legacy_curling_investment, TR),
    TR >= 0.70.

:- end_tests(olympic_legacy_curling_investment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Post-Olympic investment provides real elite athlete benefits and infrastructure modernization (coordination component), but systematically disadvantages grassroots pathways by concentrating capital in high-visibility, high-cost facilities designed for elite training. The extraction is not coercive (no mechanism directly prevents grassroots participation) but structural — funding prioritization and facility design implicitly favor elite pathways. Suppression (0.42): Moderate. Barriers to grassroots alternatives include: (1) facility location concentration in elite centers rather than distributed grassroots clubs, (2) cost structures designed for elite training rather than casual participation, (3) governance structures that enforce Olympic prioritization, and (4) migration of experienced coaches and players to elite programs. But suppression is incomplete — grassroots curling persists outside legacy frameworks. Theater ratio (0.58): Moderate-high. Legacy reporting emphasizes participation growth statistics and facility modernization while obscuring the concentration of those gains in elite pathways and the decline in small-club accessibility. Official legacy narratives present investment as broadly beneficial sport development; actual distribution is bifurcated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival range from elite beneficiaries to grassroots victims. The elite athlete sees a Rope (coordination enabling competitive development). The organizing committee sees a Rope (political and metrics justification). The grassroots curler sees a Snare (trapped in degraded access). The regional council sees a Scaffold (temporary funding window that can be strategically leveraged). The legacy bureaucracy sees a Piton (institutional structure persisting through inertia). The civilizational observer risks seeing a Mountain (inevitable Olympic concentration pattern) — but the structural data reveals this as a false summit: the investment model is a contingent policy choice, not an immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Elite athletes: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit to other nations if investment conditions change. Grassroots curlers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; local clubs cannot exit if elite concentration removes funding and membership pathways. National federation: Enforcer + constrained → d≈0.52, f(d)≈0.65. Moderate extraction; must maintain Olympic governance but constrained by federation membership needs. Regional councils: Powerful + mobile → d≈0.35, f(d)≈0.28. Low extraction; can redirect or reframe legacy investment toward grassroots if political conditions permit. Olympic committees: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through political capital and narrative legitimation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that post-Olympic curling investment IS a genuine tangled rope: it genuinely coordinates elite athlete development (Rope component) while simultaneously extracting from grassroots participation (Snare component). The resolution is NOT to deny the coordination function or to treat the constraint as pure extraction. Rather, it is to observe that a single policy mechanism (post-Olympic funding concentration) serves both functions asymmetrically — benefiting organized elites while harming dispersed grassroots communities. The false summit (mountain perspective) naturalizes this as inevitable Olympic concentration; the analytical corrective reveals it as a policy choice with restructurable components. A dual-pathway post-Olympic investment model (mandating percentage allocation to grassroots accessibility alongside elite development) would reduce the extractiveness component while preserving the coordination function — moving the constraint toward a lower-χ Scaffold or Rope rather than the current higher-χ Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_versus_participation_tradeoff,
    'Is post-Olympic elite investment in curling a necessary tradeoff for building competitive national teams, or does it represent misaligned incentives that could be restructured to support both elite and grassroots pathways?',
    'Comparative analysis of post-Olympic funding distributions across Winter Games cohorts; correlation between elite investment and grassroots participation growth; case studies of Games that mandated dual-pathway funding',
    'If tradeoff is necessary: constraint is unavoidable (mountain/rope from more perspectives). If misaligned: constraint is extractive policy choice (snare/tangled rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_versus_participation_tradeoff, empirical, 'Whether elite investment is necessary tradeoff or policy misalignment').

omega_variable(
    infrastructure_reallocation_capacity,
    'Can Olympic-built curling facilities be effectively repurposed for grassroots participation post-Olympics, or are they structurally designed (cost, scheduling, location) for elite training only?',
    'Audit of facility design specifications and cost structures; tracking of facility utilization rates pre- and post-Olympics; accessibility analysis (geographic distribution, membership fees, scheduling for casual players)',
    'If repurposable: legacy investment is coordination problem (Scaffold gate confirmed). If structurally elite-only: investment is extractive by design (Snare/Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_reallocation_capacity, empirical, 'Whether Olympic facilities can be repurposed for grassroots participation').

omega_variable(
    counterfactual_baseline_investment,
    'Would equivalent capital investment in non-Olympic-year curling infrastructure (grassroots clubs, regional training centers, accessibility programs) produce greater long-term participation and competitive depth than post-Olympic legacy spending patterns?',
    'Historical comparison of baseline grassroots participation trends vs post-Olympic surges; econometric analysis of participation elasticity with respect to investment type (elite vs grassroots); longitudinal tracking of athlete pipeline from both investment models',
    'If grassroots-focused investment is superior: legacy constraint represents misallocation (Snare perspective confirmed). If Olympic-focused model is superior: constraint reflects legitimate strategic prioritization (Rope perspective confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_baseline_investment, empirical, 'Whether grassroots-focused investment would produce better long-term outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(olympic_legacy_curling_investment, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(olycurl_tr_t0, olympic_legacy_curling_investment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(olycurl_tr_t2, olympic_legacy_curling_investment, theater_ratio, 2, 0.48).
narrative_ontology:measurement(olycurl_tr_t5, olympic_legacy_curling_investment, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(olycurl_be_t0, olympic_legacy_curling_investment, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(olycurl_be_t2, olympic_legacy_curling_investment, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(olycurl_be_t5, olympic_legacy_curling_investment, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(olympic_legacy_curling_investment, resource_allocation).
narrative_ontology:affects_constraint(olympic_legacy_curling_investment, olympic_host_city_debt_accumulation).
narrative_ontology:affects_constraint(olympic_legacy_curling_investment, winter_sport_equipment_accessibility).

% DUAL FORMULATION NOTE:
% Post-Olympic curling investment is downstream of broader Olympic legacy policy frameworks. The upstream constraint (Olympic host city resource allocation) determines the overall capital available; post-Olympic curling investment represents a specific instantiation of that constraint with different ε (0.38) reflecting the sport-specific pathway concentration effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(olympic_legacy_curling_investment, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
