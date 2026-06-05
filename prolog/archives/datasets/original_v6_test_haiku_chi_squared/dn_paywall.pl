% ============================================================================
% CONSTRAINT STORY: dn_paywall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dn_paywall, []).

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
 *   constraint_id: dn_paywall
 *   human_readable: Dagens Nyheter Digital Subscription Paywall
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Dagens Nyheter's digital paywall represents a classic tension between
 *   sustainable news funding (a genuine coordination problem in the
 *   post-advertising digital economy) and extractive rent-seeking from
 *   readers. The paywall was introduced as a metered model (5-10 free
 *   articles per month) to balance public access with revenue requirements.
 *   Over the 10-year interval, extractiveness has increased from 0.28
 *   (minimal paywall, mostly free) to 0.52 (stricter metering), while theater
 *   has remained moderate at 0.45 (paywall is technically enforced but has
 *   well-known workarounds). The constraint exhibits all six classification
 *   types from different perspectives: publishers experience Rope
 *   (coordination for sustainable funding), casual readers experience Snare
 *   (trapped without resources), premium subscribers experience Tangled Rope
 *   (both access and cost), open news platforms experience Scaffold (sunset
 *   as alternatives mature), aggregators experience Tangled Rope (mutual
 *   dependence), and the legacy publishing system experiences Piton (ritual
 *   inertia). The core mandatrophy is whether the paywall is a necessary
 *   funding mechanism (Rope) or opportunistic extraction (Snare)—a question
 *   that depends on elasticity, market structure, and the viability of
 *   alternatives.
 *
 * KEY AGENTS:
 *   - Dagens Nyheter Publishers: Primary beneficiary (institutional/arbitrage) — captures subscription revenue and maintains control over content distribution; exit options abundant (can adjust paywall depth, pursue ads, merge with competitors)
 *   - Casual Readers: Primary victim (powerless/trapped) — previously accessed DN freely; now face metered access without subscription resources; no viable exit
 *   - Budget-Conscious Subscribers: Secondary victim (moderate/constrained) — must choose between subscription cost and other necessities; face trade-off between access and financial strain
 *   - Premium Subscribers: Secondary beneficiary (moderate/mobile) — gain exclusive access and support quality journalism; mobile enough to switch to competitors if value deteriorates
 *   - Open News Platforms: Organized alternative (organized/constrained) — SVT, Blocket, digital-native outlets, public broadcasting; represent structured alternative path with sunset logic
 *   - News Aggregators: Organized third party (organized/constrained) — Google News, Apple News, social platforms; benefit from DN content but constrained by paywall restrictions
 *   - Legacy Publishing System: Institutional observer (institutional/arbitrage) — maintains paywall as performative ritual; sees own mechanism as degraded (high workaround rates)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dn_paywall, 0.52).
domain_priors:suppression_score(dn_paywall, 0.68).
domain_priors:theater_ratio(dn_paywall, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dn_paywall, extractiveness, 0.52).
narrative_ontology:constraint_metric(dn_paywall, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dn_paywall, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dn_paywall, tangled_rope).
narrative_ontology:human_readable(dn_paywall, "Dagens Nyheter Digital Subscription Paywall").
narrative_ontology:topic_domain(dn_paywall, "economic/technological").

domain_priors:requires_active_enforcement(dn_paywall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dn_paywall, dn_publishers).
narrative_ontology:constraint_beneficiary(dn_paywall, premium_subscriber_base).
narrative_ontology:constraint_victim(dn_paywall, casual_readers).
narrative_ontology:constraint_victim(dn_paywall, low_income_users).
narrative_ontology:constraint_victim(dn_paywall, news_aggregators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CASUAL READER (SNARE) — Previously accessed DN freely; now faces hard paywall after 5 articles/month. No viable exit without abandoning primary news source. Income constraints prevent subscription purchase. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73. Pure extraction: reader has no alternative path to DN content and no resources to pay.
constraint_indexing:constraint_classification(dn_paywall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDGET-CONSCIOUS SUBSCRIBER (TANGLED_ROPE) — Faces choice between subscription cost and other necessities. Paywall provides access to quality journalism (coordination benefit) but forces difficult trade-off. Constrained exit: subscription is optional but valuable. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55. Hybrid: both coordinated access and extraction present.
constraint_indexing:constraint_classification(dn_paywall, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DN PUBLISHERS (ROPE) — Paywall solves coordination problem: how to fund quality journalism in digital environment. Publishers experience paywall as coordination mechanism enabling revenue model. Exit options abundant: can adjust paywall depth, offer tiered access, pursue advertising alternatives. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary; negative effective extraction indicates net coordination benefit.
constraint_indexing:constraint_classification(dn_paywall, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPEN NEWS MOVEMENT (SCAFFOLD) — Alternative news aggregators, public broadcasters (SVT), and open-access journalism platforms (Blocket, TT) represent a structured alternative path. Sunset clause is implicit: as digital native competition and public broadcasting mature, DN's paywall extraction mechanism loses force. d≈0.45, f(d)≈0.48, σ=0.9 → χ≈0.22. Low effective extraction because coalition has agency and sees pathway forward.
constraint_indexing:constraint_classification(dn_paywall, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: NEWS AGGREGATORS (TANGLED_ROPE) — Google News, Apple News, social media platforms benefit from DN's content (coordination function: content distribution) but face paywall restrictions on linking and snippet depth. Constrained: cannot simply copy full articles but benefit from DN's brand and trust. d≈0.55, f(d)≈0.72, σ=1.0 → χ≈0.37. Mixed extraction and coordination: paywall constrains their aggregation model but creates mutual dependence.
constraint_indexing:constraint_classification(dn_paywall, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY PUBLISHING SYSTEM (PITON) — Paywall represents institutional inertia and theater: the paywall ritual persists despite weak enforcement (easy workarounds via private browsing, incognito mode, article forwarding) and declining effectiveness (younger readers abandon legacy media entirely). theater_ratio=0.45 is moderate but trending upward as paywall becomes performative obstacle. Publishers maintain paywall because alternatives haven't fully replaced it, not because it optimally solves the revenue problem. d≈0.12, f(d)≈0.02, σ=1.0 → χ≈0.02. Degraded constraint: original coordination function (sustainable news funding) not working as intended; paywall persists through institutional path-dependency.
constraint_indexing:constraint_classification(dn_paywall, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dn_paywall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dn_paywall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dn_paywall, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dn_paywall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dn_paywall, TR),
    TR >= 0.70.

:- end_tests(dn_paywall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over interval. Initial value 0.28 reflects light metering (10-15 free articles/month); current value 0.52 reflects stricter metering (5 articles/month or aggressive frequency capping). The increase reflects that DN has tightened the paywall as digital transition progressed and advertising revenue declined. Still below Snare threshold (0.66) because the metered model retains a free tier—full access is blocked only for heavy readers. Suppression (0.68): High. Paywalls are pure suppression mechanisms: they prevent access through access restriction (not persuasion or competition). Metered paywalls suppress access for readers exceeding monthly quotas. Secondary suppression: cost barrier suppresses low-income subscribers. Theater ratio (0.45): Moderate and rising. Paywall has technical enforcement (cookie-based metering) but well-documented workarounds exist: private browsing (clears cookie), incognito mode (ephemeral session), article forwarding to friends, cached versions, RSS readers. Rising trend (0.32→0.45) reflects that as workarounds proliferate, the paywall becomes increasingly performative—it filters casual browsers but sophisticated readers maintain access. Theater growth is slow because metering is still functionally enforced for most users.
 *
 * PERSPECTIVAL GAP:
 *   The publisher perspective sees Rope (coordination solution to news funding crisis), while casual readers see Snare (extraction without coordination benefit). This gap is the core mandatrophy. Premium subscribers see Tangled Rope (access benefit + cost burden). Aggregators see constrained Tangled Rope (paywall restricts full content use but creates interdependence). The open news coalition sees Scaffold (temporary paywall as alternative platforms mature). The legacy publishing system sees Piton (degraded ritual maintained by institutional inertia rather than functional necessity). The perspectival gap reflects competing claims: Is this a necessary coordination mechanism or unnecessary extraction? The resolution depends on empirical facts about demand elasticity and market structure.
 *
 * DIRECTIONALITY LOGIC:
 *   DN Publishers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative χ indicates coordinating institution (revenue model is necessary). Casual readers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit options; income constraints prevent subscription. Budget-conscious subscribers: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but constrained choice (subscription is optional but valuable). Premium subscribers: Beneficiary + mobile → d≈0.25, f(d)≈0.15. Low extraction; mobile enough to switch platforms; willingly pay for perceived value. News aggregators: Victim + constrained → d≈0.55, f(d)≈0.72. Moderate extraction; constrained by paywall restrictions but interdependent with DN. Open news coalition: Victim + constrained (but organized) → d≈0.45, f(d)≈0.48. Organized agents; see clear pathway forward (alternatives to paywall); low effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The mandatrophy—is DN's paywall Rope (necessary funding) or Snare (opportunistic extraction)?—is empirically resolvable through three key metrics. (1) DEMAND ELASTICITY: If elasticity of subscription demand is >1.5 (high sensitivity to price), the paywall extracts beyond what funding requirements demand—excess would support Snare classification. If elasticity is <0.8, the paywall is necessary to maintain subscriber base—supports Rope. Current estimate: elasticity ≈1.2 (moderate), suggesting paywall is partly necessary, partly extractive. (2) THEATER LEVEL TREND: If theater_ratio exceeds 0.60, paywall is largely performative (readers circumvent easily)—supports Piton over Snare. Current trend 0.32→0.45 suggests theater is rising but still functional. (3) MARKET CONSOLIDATION: If digital news market consolidates toward legacy paywall dominance, Snare classification holds. If market fragments toward open alternatives, Scaffold sunset logic strengthens. Current trajectory: SVT+ (public broadcasting) and digital-native outlets are gaining share, supporting Scaffold. RESOLUTION OUTCOME: Tangled Rope classification (ε=0.52, active enforcement, beneficiaries+victims) captures the hybrid nature: DN's paywall genuinely solves a coordination problem (sustainable journalism funding in digital economy) but also extracts from readers who lack alternatives. As market consolidates and alternatives mature (SVT+, Blocket, digital natives), the constraint will shift from Tangled Rope toward Scaffold (sunset logic) or degrade to Piton (theatrical inertia) depending on whether DN's paywall remains functionally superior to alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paywall_elasticity_demand,
    'What is the actual price elasticity of demand for DN subscriptions, and does it support the claimed paywall extraction model?',
    'Historical subscription data analysis: correlation between paywall depth changes, pricing adjustments, and subscriber acquisition/churn rates. Comparison with competitor pricing (SVT, GP, Aftonbladet paywall models).',
    'If elasticity is high (>1.5): paywall is extractive beyond coordination needs (supports Snare classification for readers). If elasticity is low (<0.8): paywall is genuinely necessary coordination mechanism (supports Rope from publisher perspective). Uncertain elasticity is core driver of mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paywall_elasticity_demand, empirical, 'Price elasticity of demand for DN subscriptions').

omega_variable(
    paywall_enforcement_effectiveness,
    'How effectively does DN''s metered paywall actually restrict access given available workarounds (private browsing, incognito mode, article forwarding, cached versions)?',
    'Access pattern analysis: fraction of articles accessed via paywall-circumvention methods; comparison of reader metrics pre-paywall vs post-paywall; survey data on reader awareness of workarounds.',
    'If workarounds capture >40% of would-be blocked traffic: paywall is theater (Piton). If workarounds capture <15%: paywall has meaningful enforcement (supports Snare/Tangled Rope). Theater level directly affects classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paywall_enforcement_effectiveness, empirical, 'Effectiveness of paywall enforcement against common workarounds').

omega_variable(
    news_market_consolidation_trajectory,
    'Is the digital news market consolidating toward monopoly/oligopoly by legacy publishers, or fragmenting toward open alternatives?',
    'Market share tracking: DN, GP, TT, SVT, Blocket, and digital-native outlets over 5-year horizon. Subscription penetration rates in Sweden by demographic. User retention and acquisition costs by outlet type.',
    'If consolidating toward legacy paywall dominance: paywall is sustainable extraction (Snare/Tangled Rope from readers'' perspective). If fragmenting toward alternatives: paywall''s extraction force declines, supporting Scaffold sunset logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(news_market_consolidation_trajectory, empirical, 'Direction of digital news market consolidation').

omega_variable(
    regulatory_intervention_likelihood,
    'Will Swedish media policy (EU Digital Services Act, national public broadcasting mandates) impose constraints on paywall depth or require open access to news?',
    'Policy analysis: EU DSA implementation timeline; Swedish government media policy statements; precedent from other Nordic countries; industry lobbying positions.',
    'If high-likelihood intervention: paywall has political sunset (Scaffold). If low-likelihood: paywall persists as long as publishers choose (Snare/Tangled Rope indefinitely).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_likelihood, preference, 'Likelihood of regulatory intervention on paywall restrictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dn_paywall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dnpw_tr_t0, dn_paywall, theater_ratio, 0, 0.32).
narrative_ontology:measurement(dnpw_tr_t5, dn_paywall, theater_ratio, 5, 0.38).
narrative_ontology:measurement(dnpw_tr_t10, dn_paywall, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(dnpw_be_t0, dn_paywall, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dnpw_be_t5, dn_paywall, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(dnpw_be_t10, dn_paywall, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dn_paywall, resource_allocation).
narrative_ontology:affects_constraint(dn_paywall, swedish_news_market_consolidation).
narrative_ontology:affects_constraint(dn_paywall, public_broadcasting_funding_model).
narrative_ontology:affects_constraint(dn_paywall, digital_journalism_sustainability).

% DUAL FORMULATION NOTE:
% DN paywall is downstream of the broader digital journalism sustainability constraint—the economic challenge of funding quality reporting without advertising revenue. The upstream constraint (digital journalism sustainability) has ε≈0.35 (coordination problem) while the paywall (downstream) has ε≈0.52 (partly extraction, partly coordination). The decomposition reflects that the general problem (how to fund news?) is distinct from the specific implementation (metered paywall). Network edges link DN paywall to competitor paywalls and alternative funding models.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dn_paywall, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
