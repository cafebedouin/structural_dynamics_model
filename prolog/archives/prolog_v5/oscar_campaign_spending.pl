% ============================================================================
% CONSTRAINT STORY: oscar_campaign_spending
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_oscar_campaign_spending, []).

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
 *   constraint_id: oscar_campaign_spending
 *   human_readable: Oscar Campaign Spending Limits
 *   domain: social/entertainment_industry
 *
 * SUMMARY:
 *   The informal spending limit in Oscar campaigns represents a structural
 *   constraint where the absence of explicit rules creates extractive
 *   behavioral patterns. Industry custom establishes an unwritten floor
 *   ($500k–$2M range) below which competitive visibility becomes impossible,
 *   yet no ceiling exists; films with production budgets exceeding $100M can
 *   spend indefinitely on campaigns. This creates asymmetric pressure:
 *   established studios with profit margins can absorb campaign costs as a
 *   line item; independent producers face zero-sum resource allocation
 *   between production quality and campaign visibility. The constraint
 *   exhibits all six DR types from different perspectives. For major studios,
 *   the spending norm is pure coordination—it enables prestige signaling and
 *   talent recruitment. For independent producers, it is pure
 *   extraction—spending becomes mandatory rent paid to the visibility
 *   gatekeepers. For the Academy institution itself, the spending-based
 *   winnowing is increasingly performative (theater_ratio 0.65): ceremonial
 *   voting processes maintain the appearance of merit-based selection while
 *   campaign resources drive material outcomes. The reform coalition sees a
 *   temporary failure mode with clear sunset paths (transparency,
 *   accessibility, blind screening). The analytical observer risks
 *   naturalizing what is contingent: spending-prestige correlation is not
 *   inherent to film quality or cultural value, but to specific institutional
 *   bottlenecks.
 *
 * KEY AGENTS:
 *   - Major Studios: Primary beneficiaries (institutional/arbitrage) — absorb campaign costs as operational expenses; capture prestige and talent recruitment benefits; can exit into streaming if awards become uneconomical
 *   - Independent Producers: Primary victims (powerless/trapped) — cannot afford competitive spending floors; accept obscurity or external financing; no exit from the system without abandoning award recognition
 *   - Mid-Tier Studios: Secondary actors (moderate/constrained) — constrained by both participation incentives (prestige value) and extraction costs (spending floors); experience mixed coordination and asymmetric extraction
 *   - Academy Institution: Institutional degradation (institutional/arbitrage) — maintains award credibility through ceremonial procedures while material spending drives outcomes; sees own process as declining in legitimacy (piton classification)
 *   - Academy Reform Coalition: Organized resistance (organized/constrained) — pushing spending transparency, campaign limits, blind screening, voting accessibility; building alternative pathways; represents structured sunset mechanism
 *   - Film Critics and Awards Observers: Analytical perspective (analytical/analytical) — document the spending-outcome correlation; risk naturalizing contingent institutional arrangements as inherent properties of merit recognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(oscar_campaign_spending, 0.52).
domain_priors:suppression_score(oscar_campaign_spending, 0.48).
domain_priors:theater_ratio(oscar_campaign_spending, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(oscar_campaign_spending, extractiveness, 0.52).
narrative_ontology:constraint_metric(oscar_campaign_spending, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(oscar_campaign_spending, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(oscar_campaign_spending, tangled_rope).
narrative_ontology:human_readable(oscar_campaign_spending, "Oscar Campaign Spending Limits").
narrative_ontology:topic_domain(oscar_campaign_spending, "social/entertainment_industry").

domain_priors:requires_active_enforcement(oscar_campaign_spending).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(oscar_campaign_spending, industry_insiders).
narrative_ontology:constraint_beneficiary(oscar_campaign_spending, established_studios).
narrative_ontology:constraint_victim(oscar_campaign_spending, independent_producers).
narrative_ontology:constraint_victim(oscar_campaign_spending, emerging_artists).
narrative_ontology:constraint_victim(oscar_campaign_spending, award_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING INDEPENDENT PRODUCER (SNARE) — Trapped by resource constraints. Cannot afford competitive campaign spending to achieve visibility. Industry norms establish an unwritten spending floor that determines eligibility. No realistic exit from the system without abandoning awards recognition. Full extraction: the constraint forces choice between competing on financiers' terms or accepting obscurity.
constraint_indexing:constraint_classification(oscar_campaign_spending, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER STUDIO (TANGLED ROPE) — Constrained by both coordination needs (must participate in industry award system for prestige) and extraction costs (spending floors limit profitability). Benefits from participation in the award ecosystem through marketing and talent credibility; costs from campaign spending requirements. Mixed experience: genuine coordination function with asymmetric extraction imposed on those with fewer resources.
constraint_indexing:constraint_classification(oscar_campaign_spending, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR STUDIO (ROPE) — Institutional actor with arbitrage capacity. Campaign spending is a budgeted line item; the constraint appears as pure coordination (communicating film excellence to voters). Net beneficiary through prestige capture and talent recruitment. Can exit into direct-to-consumer streaming if awards system becomes uneconomical. Experiences the spending limit as a coordination mechanism rather than extraction.
constraint_indexing:constraint_classification(oscar_campaign_spending, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMY REFORM COALITION (SCAFFOLD) — Organized actors (critics, guild members, diversity advocates) pushing spending transparency and campaign limits. See the current constraint as a temporary failure mode being actively reformed. Reform initiatives (campaign spending caps, voting accessibility, blind screening) represent structured exit paths. Sunset logic: as transparency norms mature and voting accessibility increases, the spending-driven advantage decays. Estimated sunset: 5-10 years as digital screening and decentralized voting become standard.
constraint_indexing:constraint_classification(oscar_campaign_spending, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMY AWARD INSTITUTION (PITON) — The ceremonial prestige function persists through institutional inertia despite degraded legitimacy. Academy maintains award credibility through theatrical voting procedures and honor rituals, but the core selection mechanism (campaign spending as proxy for worthiness) is substantially performative. Theater ratio 0.65 reflects: ceremonial voting broadcasts, strategic screening events, and publicized voting processes that maintain the appearance of merit-based selection while material resources drive outcomes. The institution sees its own process as declining in function but maintains it through tradition.
constraint_indexing:constraint_classification(oscar_campaign_spending, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a civilizational/global perspective, some correlation between resource allocation and prestige distribution appears inherent to human social hierarchies: scarcity of status and visibility rewards those who can command resources. This perspective naturalizes spending-based winnowing as immutable. However, structural data contradicts the mountain classification — the engine identifies this as a false summit. The spending-prestige correlation is contingent on specific institutional arrangements (closed voting, limited media pathways, concentration of resources), not on natural law. Alternative social technologies exist that decouple prestige from spending.
constraint_indexing:constraint_classification(oscar_campaign_spending, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(oscar_campaign_spending_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(oscar_campaign_spending, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(oscar_campaign_spending, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(oscar_campaign_spending, TR),
    TR >= 0.70.

:- end_tests(oscar_campaign_spending_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The spending constraint functions as extraction for agents without resource liquidity (independent producers) and as coordination for institutional actors (major studios). The base value reflects that spending is neither optional (communication costs are real) nor uniformly devastating (some films succeed with limited campaigns). The value has risen from 0.35 over the 20-year interval as: (a) campaign costs have inflated faster than production budgets for mid-tier films, (b) digital marketing options have proliferated (increasing the floor), and (c) industry consolidation has reduced the number of viable alternative distribution pathways. Suppression (0.48): Moderate. Barriers to independent entry include: (a) limited access to campaign financing, (b) concentrated media buying power favoring established studios, (c) social proof effects (films with visible campaigns get presumed credibility), (d) institutional bias in voting pools. However, suppression is not total—some independent films do succeed without major budgets (approximately 15-20% of nominations), and digital discovery mechanisms are creating new visibility pathways. Theater ratio (0.65): Moderate-high. The constraint operates substantially through performative mechanisms: ceremonial voting broadcasts, strategic press events, publicized 'campaign moments' that serve to legitimize outcomes rather than determine them. The theater has increased from 0.45 as: (a) campaigns have become more cinematic and event-focused (less substance, more spectacle), (b) voting procedures have become more ceremonial (to counter perceived bias), and (c) the correlation between campaign visibility and actual voter preference has weakened (suggesting the campaign is increasingly decorative rather than determinative).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Major studios see Rope—pure coordination enabling prestige signaling. The Academy sees Piton—a degraded institution maintaining form while substance erodes. Independent producers see Snare—mandatory rent extraction with no exit. The reform coalition sees Scaffold—a temporary problem being actively solved through transparency and accessibility reforms. Mid-tier studios see Tangled Rope—mixed benefits (prestige access) and costs (mandatory spending). The civilizational analytical observer risks seeing Mountain (spending-prestige correlation as natural), but the structural data reveals this as a false summit. The perspectival gap between major studios and independent producers is 4-5 classification steps (Rope → Tangled Rope → Snare), reflecting complete inversion of whether the spending limit is beneficial (coordination) or harmful (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: (1) Major studios as beneficiaries with arbitrage capacity → low d (~0.15) → low/negative χ → experience Rope. (2) Independent producers as victims with trapped exit → high d (~0.90) → high χ → experience Snare. (3) Mid-tier studios as mixed (partial beneficiary, partial victim) with constrained exit → moderate d (~0.55) → moderate χ → experience Tangled Rope. (4) Reform coalition as organized actors building exit pathways → constrained exit with agency → moderate d (~0.45) → moderate χ → experience Scaffold (with sunset via reforms). (5) Academy institution as degraded beneficiary maintaining form → d (~0.20) → low χ but high theater → experience Piton (functional decay, institutional inertia). (6) Analytical observer has no structural position in the extraction flow → d (~0.72) → high analytical χ → analytical perspective that risks naturalizing contingent arrangements as natural law. The directionality derivation explains why identical base metrics (extractiveness, suppression) produce divergent classifications: the observer's structural relationship to the constraint determines how they experience it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spending_floor_threshold,
    'What campaign spending level constitutes a binding floor below which competitive viability collapses?',
    'Historical analysis of nomination outcomes correlated with campaign spending by tier; threshold identification through logistic regression of spend-to-nomination probability',
    'If threshold < $500k: many worthy films filtered out. If threshold > $2M: spending becomes pure theater. Determines whether constraint is extraction (trapped agents cannot reach floor) or coordination (floor reflects legitimate communication costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spending_floor_threshold, empirical, 'Campaign spending floor for competitive viability').

omega_variable(
    voter_spending_awareness,
    'To what degree do Academy voters actually condition their votes on knowledge of campaign spending vs. on perceived film quality?',
    'Exit polling, voter surveys, blind comparison screening (same films presented with and without campaign metadata); correlation analysis between spend and votes controlling for film characteristics',
    'If voters ignore spending: constraint is informational (spending communicates but doesn''t determine). If voters heavily weight spending: constraint is extractive (spending dominates merit signals). Determines classification gap between major studios (who see coordination) and independents (who see extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voter_spending_awareness, empirical, 'Voter responsiveness to campaign spending signals').

omega_variable(
    alternative_distribution_viability,
    'Can emerging films achieve award visibility through mechanisms other than traditional campaign spending (festival circuits, digital discovery, guild outreach)?',
    'Comparative outcome analysis: films nominated via traditional campaigns vs. festival-to-nomination pathway vs. digital/grassroots discovery; cost-effectiveness analysis of alternative visibility mechanisms',
    'If alternatives viable: spending constraint is contingent institutional arrangement (potential scaffold exit path exists). If traditional pathway monopolizes visibility: spending is structural bottleneck (snare from emerging producer perspective). Determines whether sunset clause is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_distribution_viability, empirical, 'Viability of non-spending-dependent award pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(oscar_campaign_spending, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oscar_tr_t0, oscar_campaign_spending, theater_ratio, 0, 0.45).
narrative_ontology:measurement(oscar_tr_t10, oscar_campaign_spending, theater_ratio, 10, 0.58).
narrative_ontology:measurement(oscar_tr_t20, oscar_campaign_spending, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(oscar_be_t0, oscar_campaign_spending, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oscar_be_t10, oscar_campaign_spending, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(oscar_be_t20, oscar_campaign_spending, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(oscar_campaign_spending, resource_allocation).
narrative_ontology:affects_constraint(oscar_campaign_spending, film_festival_prestige_hierarchy).
narrative_ontology:affects_constraint(oscar_campaign_spending, entertainment_industry_access_inequality).
narrative_ontology:affects_constraint(oscar_campaign_spending, award_system_voter_diversity).

% DUAL FORMULATION NOTE:
% The Oscar spending constraint is upstream of festival prestige hierarchies and downstream of broader entertainment industry inequality. The constraint decomposes into two structural claims: (1) Campaign Spending Floor (ε~0.30, Rope from industry view, Scaffold from reform view) — legitimate communication costs for visibility. (2) Spending-Outcome Correlation (ε~0.68, Snare from outsider view, Piton from institution view) — the material coupling of spending to nominations, driven by concentrated media buying and social proof effects. These are distinct ε values reflecting different structural mechanisms. Both are captured in the unified story via perspectival variance in experienced χ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(oscar_campaign_spending, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
