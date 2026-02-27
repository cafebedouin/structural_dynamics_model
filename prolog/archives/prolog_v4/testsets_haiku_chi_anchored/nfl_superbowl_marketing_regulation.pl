% ============================================================================
% CONSTRAINT STORY: nfl_superbowl_marketing_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nfl_superbowl_marketing_regulation, []).

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
 *   constraint_id: nfl_superbowl_marketing_regulation
 *   human_readable: NFL Super Bowl Advertising Regulations
 *   domain: economic/sports_marketing
 *
 * SUMMARY:
 *   The NFL Super Bowl advertising regulations represent a structural
 *   constraint on marketing competition that combines genuine coordination
 *   benefits (audience aggregation, content curation, broadcast quality) with
 *   significant extraction mechanisms (monopoly pricing, suppression of
 *   non-licensed competitors, rent-seeking through exclusive licensing). The
 *   constraint operates through intellectual property law, trademark
 *   enforcement, and contractual exclusivity—all officially justified as
 *   brand protection and quality control. The core tension is between the
 *   NFL's legitimate role as event organizer and audience aggregator
 *   (coordination) and its use of monopoly power over the Super Bowl brand
 *   and broadcast distribution to extract rents from advertisers and exclude
 *   competitors (extraction). The constraint has intensified over 40 years as
 *   Super Bowl advertising has become a premium cultural moment, with the NFL
 *   systematically increasing advertising rates and tightening control over
 *   the advertising ecosystem.
 *
 * KEY AGENTS:
 *   - NFL League Office: Institutional beneficiary (institutional/arbitrage) — controls broadcast rights, ad inventory, and licensing; extracts rent through monopoly control
 *   - Team Owners: Institutional beneficiary (institutional/arbitrage) — receive revenue share from licensing and broadcast fees
 *   - Official Broadcasters (CBS/Fox): Institutional actor (institutional/arbitrage) — licensed monopoly on broadcast distribution; benefits from ad exclusivity
 *   - Licensed Advertisers (major brands): Institutional victim and beneficiary (institutional/arbitrage) — pay premium rates but benefit from prestige and audience reach
 *   - Non-Licensed Competitors: Powerless victim (powerless/trapped) — completely excluded from authorized advertising pathways
 *   - Marginal Advertisers: Moderate victim (moderate/constrained) — face binary choice: pay monopoly rate or exit premium market
 *   - Consumers/Viewers: Organized agents (organized/constrained) — benefit from curated, high-quality ads but face suppressed choice and indirect cost pass-through
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nfl_superbowl_marketing_regulation, 0.58).
domain_priors:suppression_score(nfl_superbowl_marketing_regulation, 0.68).
domain_priors:theater_ratio(nfl_superbowl_marketing_regulation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nfl_superbowl_marketing_regulation, tangled_rope).
narrative_ontology:human_readable(nfl_superbowl_marketing_regulation, "NFL Super Bowl Advertising Regulations").
narrative_ontology:topic_domain(nfl_superbowl_marketing_regulation, "economic/sports_marketing").

domain_priors:requires_active_enforcement(nfl_superbowl_marketing_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, nfl_league_office).
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, nfl_team_owners).
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, official_broadcasters).
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, licensed_advertisers).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, non_licensed_competitors).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, marginal_advertisers).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, consumer_choice_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED COMPETITOR (SNARE) — Non-licensed brands cannot access the Super Bowl marketing opportunity regardless of willingness to pay; no legal pathway exists for unauthorized advertising. Trapped exit + victim status → d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.79. Pure extraction with suppression of alternatives.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL ADVERTISER (SNARE) — Mid-sized firms face binary choice: pay monopoly rate for Super Bowl spot or exit the premium advertising market entirely. Constrained exit + victim status → d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.63. High extraction, moderate suppression.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LICENSED ADVERTISER (ROPE) — Major brands (Pepsi, Apple, etc.) experience the constraint as coordination: the NFL maintains broadcast quality, audience size, and brand prestige that justifies premium rates. Arbitrage exit + beneficiary status → d≈0.12, f(d)≈0.02, σ=1.0 → χ≈0.01. Near-zero effective extraction; coordination benefit outweighs cost.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BROADCASTER (TANGLED ROPE) — CBS/Fox benefits from NFL exclusivity (coordination value: premium content, high ad rates) but also extracts through the constraint by being the sole channel for this audience reach. Arbitrage exit + mixed beneficiary/victim → d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.19. Moderate extraction embedded in coordination relationship.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSUMERS (TANGLED ROPE) — Viewers enjoy curated ad experience (coordination: higher-quality creative, premium-brand content) but also face suppressed choice (constraint: cannot access non-licensed competitor ads, premium pricing passed through to goods). Constrained exit + mixed beneficiary/victim → d≈0.54, f(d)≈0.68, σ=1.0 → χ≈0.39. Mixed: benefits from quality coordination but extraction from suppressed alternatives.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NFL LEAGUE OFFICE (ROPE) — Sees the constraint as pure coordination mechanism: managing ad inventory, enforcing brand quality standards, and aggregating viewer attention creates value for all parties. Arbitrage exit + beneficiary status → d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Negative effective extraction; institutional perspective naturalizes extraction as coordination overhead.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits both genuine coordination (audience aggregation, brand quality) and structural extraction (rent-seeking through monopoly control, suppression of alternatives). d≈0.63, f(d)≈0.92, σ=1.2 → χ≈0.63. Clear tangled rope signature: requires_active_enforcement (intellectual property, trademark law, NFL Rules), beneficiaries (league, broadcasters, licensed advertisers), victims (excluded competitors, marginal firms, consumer choice).
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nfl_superbowl_marketing_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nfl_superbowl_marketing_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nfl_superbowl_marketing_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The NFL charges advertising rates ($7M+ for 30-second spot as of 2023) that exceed what market-clearing rates would be if the market were truly competitive. The extraction reflects monopoly control over the Super Bowl brand and broadcast distribution. However, some of the premium reflects genuine coordination value (largest audience in US media annually, curated content quality, cultural prestige). The 0.58 value reflects that roughly 50% of the rate premium is coordination-justified; the remaining 50% is extraction rent. Suppression (0.68): High. Non-licensed brands cannot legally advertise during the broadcast; trademark enforcement and broadcast licensing agreements explicitly prohibit unauthorized ads. Marginal advertisers face binary choice (pay or exit) with no legal way to negotiate lower rates or alternative pathways. This is high structural suppression—alternatives are not just expensive but legally foreclosed. Theater ratio (0.42): Moderate-low. The NFL's quality control justification for exclusivity is mostly functional, not performative. Broadcast standards, advertiser vetting, and brand prestige maintenance serve real coordination purposes; there is less pure theater here than in degraded institutions. The ratio is not low (0.20) because much of the exclusivity framing is post-hoc justification for revenue extraction.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence across this constraint. The NFL league office sees pure coordination (Rope)—they are aggregating the largest audience and providing premium venue. Licensed major advertisers see mostly coordination with moderate rent (Rope); they value the prestige and reach. The broadcaster sees mixed extraction-coordination (Tangled Rope); they benefit from exclusive rights. Consumers see mixed benefits and suppressed choice (Tangled Rope). Marginal advertisers see extraction with no real alternative (Snare). Excluded competitors see pure suppression—they cannot participate at any price (Snare). The analytical observer sees the true structure: the constraint combines real coordination value with systematic extraction that is justified through false natural-law framing ('the Super Bowl is inherently premium'). The perspectival range from Rope (beneficiary) to Snare (excluded competitor) is the full spectrum.
 *
 * DIRECTIONALITY LOGIC:
 *   NFL League Office: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; institution experiences constraint as coordination mechanism it controls. Licensed Advertisers: Mixed beneficiary/victim + arbitrage → d≈0.30, f(d)≈0.24. Pay premium but benefit from audience and prestige; exit is available at cost. Marginal Advertisers: Victim + constrained → d≈0.78, f(d)≈1.08. High extraction; alternatives are expensive or non-existent. Excluded Competitors: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no legal pathway exists for participation. Consumers: Organized + constrained → d≈0.54, f(d)≈0.68. Mixed experience; benefits from coordination but faces suppressed choice. Broadcaster: Institutional + arbitrage → d≈0.35, f(d)≈0.32. Moderate extraction embedded in coordination relationship (exclusive broadcasting rights).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how the same institutional mechanism (Super Bowl exclusivity) serves both coordination and extraction purposes. The NFL's Rope perspective (pure coordination) is partially correct—audience aggregation and broadcast curation do create genuine value. But the Snare perspective from excluded competitors is also correct—the constraint completely forecloses their participation regardless of willingness to pay. The Tangled Rope classification at the analytical level captures the full structure: the constraint requires active enforcement (trademark law, broadcast licensing agreements), has clear beneficiaries (NFL, broadcasters, licensed advertisers) and victims (excluded competitors, marginal advertisers), and combines coordination benefit (large audience, quality curation) with asymmetric extraction (monopoly pricing, suppression of alternatives). The mandatrophy is resolved by accepting that institutional actors can simultaneously benefit from and enforce extraction mechanisms—there is no contradiction. The NFL genuinely coordinates audience aggregation AND genuinely extracts monopoly rent. Both perspectives are valid; the indexed classification system simply disambiguates which aspects are seen from which structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_advertiser_exit_cost,
    'What percentage of mid-sized advertisers have genuine exit options beyond Super Bowl premium advertising, or is the constraint truly monopolistic for brands seeking national reach?',
    'Analysis of alternative marketing channels (streaming, targeted digital, regional campaigns) effectiveness and cost-benefit vs Super Bowl rates; survey of advertiser decision-making',
    'If alternatives exist with 80%+ effectiveness at <50% cost: constraint is Rope or Scaffold, not Snare. If no true alternatives: constraint is Snare with near-total extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_advertiser_exit_cost, empirical, 'Whether marginal advertisers have genuine exit alternatives').

omega_variable(
    brand_prestige_vs_monopoly_rent,
    'How much of the Super Bowl premium rate reflects genuine coordination benefit (audience reach, prestige, content quality) vs pure monopoly extraction?',
    'Advertising ROI analysis: compare cost-per-impression at Super Bowl rates vs alternative premium placements; measure brand lift attribution',
    'If 70%+ is genuine coordination value: rope classification dominates. If 50%+ is extraction rent: tangled rope confirmed across more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(brand_prestige_vs_monopoly_rent, empirical, 'Decomposition of premium rate into coordination vs extraction components').

omega_variable(
    unauthorized_advertising_harm,
    'Does unauthorized advertising during Super Bowl broadcasts actually damage brand prestige or viewer experience, or is the suppression purely protective of NFL revenue?',
    'Historical analysis of Super Bowl ad bans; international comparison to markets with less restrictive advertising rules; viewer preference studies',
    'If harm is real: suppression is coordination-justified, classification shifts toward Rope. If harm is manufactured: suppression is pure extraction justification, classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unauthorized_advertising_harm, empirical, 'Whether suppression of unauthorized ads protects real value or extracts pure rent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nfl_superbowl_marketing_regulation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nfl_sb_tr_t0, nfl_superbowl_marketing_regulation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(nfl_sb_tr_t20, nfl_superbowl_marketing_regulation, theater_ratio, 20, 0.4).
narrative_ontology:measurement(nfl_sb_tr_t40, nfl_superbowl_marketing_regulation, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(nfl_sb_be_t0, nfl_superbowl_marketing_regulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nfl_sb_be_t20, nfl_superbowl_marketing_regulation, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(nfl_sb_be_t40, nfl_superbowl_marketing_regulation, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nfl_superbowl_marketing_regulation, resource_allocation).
narrative_ontology:affects_constraint(nfl_superbowl_marketing_regulation, broadcast_monopoly_sports_media).
narrative_ontology:affects_constraint(nfl_superbowl_marketing_regulation, athlete_compensation_asymmetry).
narrative_ontology:affects_constraint(nfl_superbowl_marketing_regulation, sports_intellectual_property_licensing).

% DUAL FORMULATION NOTE:
% The Super Bowl advertising regulation is downstream of broader broadcast monopoly dynamics in professional sports. The upstream constraint (broadcast_monopoly_sports_media) has ε≈0.65 and reflects the NFL's structural control over distribution. The advertising regulation (ε≈0.58) is a specific extraction mechanism enabled by that broader broadcast control. These are distinct constraints linked by structural dependence: the advertising regulation would collapse if broadcast distribution were truly competitive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nfl_superbowl_marketing_regulation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
