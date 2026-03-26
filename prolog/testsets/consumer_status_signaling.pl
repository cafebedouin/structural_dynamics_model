% ============================================================================
% CONSTRAINT STORY: consumer_status_signaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consumer_status_signaling, []).

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
 *   constraint_id: consumer_status_signaling
 *   human_readable: Consumer Status Signaling Through Positional Goods
 *   domain: economic/social/behavioral
 *
 * SUMMARY:
 *   Consumer status signaling through positional goods creates a structural
 *   trap where individuals compete for relative social status through visible
 *   consumption, with the primary extraction directed at low-to-middle income
 *   consumers seeking status recognition. The constraint exhibits all six DR
 *   types from different structural positions: a pure extraction mechanism
 *   (Snare) for wage earners with no exit, a coordination mechanism (Rope)
 *   for luxury producers, a mixed coordination-extraction hybrid (Tangled
 *   Rope) for identity-locked professionals, a temporary problem with
 *   emerging alternatives (Scaffold) for organized status-alternative
 *   movements, a degraded institutional ritual (Piton) for traditional brand
 *   systems, and a potentially false natural law (Mountain) from the
 *   analytical observer perspective. The extractiveness has increased from
 *   0.42 to 0.58 over the interval as digital marketing and social comparison
 *   mechanisms have intensified status competition. The theater ratio has
 *   risen from 0.48 to 0.65 as brand storytelling (heritage narratives,
 *   aspirational marketing, lifestyle curation) has increasingly decoupled
 *   from functional product differences.
 *
 * KEY AGENTS:
 *   - Low-to-Middle Income Consumers: Primary victims (powerless/trapped) — face suppression through social visibility norms; disproportionate income extraction for status markers
 *   - Luxury Brand Producers: Primary beneficiaries (institutional/arbitrage) — capture scarcity rents and aspirational spending; experience constraint as voluntary market coordination
 *   - Aspirational Middle-Class Professionals: Secondary victims (moderate/identity_locked) — need status signals for career credibility; identity fused with consumption markers; cannot perceive exit despite structural mobility
 *   - Alternative Status Movements: Organized agents (organized/constrained) — building parallel status systems (skill-based, value-based, digital hierarchies) with sunset logic as generational cohorts mature
 *   - Brand Heritage Institutions: Institutional actor (institutional/arbitrage) — maintain positional pricing and scarcity rituals; see own function as degraded but persist through inertia (Piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional status systems as intrinsic to human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consumer_status_signaling, 0.58).
domain_priors:suppression_score(consumer_status_signaling, 0.68).
domain_priors:theater_ratio(consumer_status_signaling, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consumer_status_signaling, extractiveness, 0.58).
narrative_ontology:constraint_metric(consumer_status_signaling, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(consumer_status_signaling, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consumer_status_signaling, snare).
narrative_ontology:human_readable(consumer_status_signaling, "Consumer Status Signaling Through Positional Goods").
narrative_ontology:topic_domain(consumer_status_signaling, "economic/social/behavioral").

domain_priors:requires_active_enforcement(consumer_status_signaling).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consumer_status_signaling, luxury_brand_producers).
narrative_ontology:constraint_beneficiary(consumer_status_signaling, status_arbiters).
narrative_ontology:constraint_beneficiary(consumer_status_signaling, aspirational_marketing_apparatus).
narrative_ontology:constraint_victim(consumer_status_signaling, low_income_consumers).
narrative_ontology:constraint_victim(consumer_status_signaling, wage_earners_seeking_status).
narrative_ontology:constraint_victim(consumer_status_signaling, middle_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE EARNER TRAPPED IN STATUS CYCLE (SNARE) — Low-to-middle income consumers face overwhelming suppression through social visibility norms and signaling competition. Exit requires abandoning identity-based status markers that are publicly visible (clothing, vehicles, housing) and socially legible. The constraint extracts disproportionate income share relative to actual utility, with suppression enforced through peer observation and shame mechanisms. No material alternative exists for obtaining social recognition within local hierarchies.
constraint_indexing:constraint_classification(consumer_status_signaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LUXURY BRAND ECOSYSTEM (ROPE) — For producers and brand managers, status signaling solves a genuine coordination problem: how to allocate scarce prestige goods to those who value them most. The price mechanism signals authenticity and exclusivity; the scarcity is partly artificial but functionally stabilizes a market. From this perspective, the constraint enables profitable coordination with minimal active coercion — brand loyalty is experienced as voluntary participation in an aspirational community.
constraint_indexing:constraint_classification(consumer_status_signaling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ASPIRATIONAL MIDDLE CLASS (TANGLED ROPE) — Middle-income professionals experience the constraint as both coordination and extraction. The coordination function: status consumption enables career signaling (professional credibility requires visible markers). The extraction: they pay above-market prices for positional goods because status competition is zero-sum. Exit from status signaling would require abandoning professional identity markers that signal competence in their field. Identity-locked because occupational status and personal identity are fused through visible consumption.
constraint_indexing:constraint_classification(consumer_status_signaling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: ALTERNATIVE STATUS MOVEMENT (SCAFFOLD) — Organized coalitions (sustainable fashion communities, minimalist movements, skill-based status systems, digital-native status hierarchies) are building parallel signaling systems where status derives from values, competence, or sustainability rather than price. These have a sunset clause: as generational cohorts age out of conventional status obsession and alternative status markers mature, the extraction mechanism of positional consumption loses force. Low effective extraction because these organized agents have agency and see an exit path within their lifetime.
constraint_indexing:constraint_classification(consumer_status_signaling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STATUS SYMBOL INSTITUTION (PITON) — The ritual performance of status through luxury goods consumption persists partly through institutional inertia. Many brands maintain exclusive positioning and scarcity-driven pricing even as digital networks make status differentiation less dependent on expensive physical goods. The theater is high (brand storytelling, heritage narratives, aspirational marketing) relative to functional benefit (the goods often perform no better than mass-market equivalents). Piton classification reflects degraded function maintained by institutional momentum — the mechanism persists because it exists, not because it solves current status coordination needs efficiently.
constraint_indexing:constraint_classification(consumer_status_signaling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, status differentiation through positional goods might appear as an irreducible feature of human social hierarchies: all societies create status markers, all status markers require scarcity to function, and scarcity drives extraction. This perspective sees the constraint as natural law — intrinsic to status competition itself. However, the structural data contradicts the mountain classification: status signaling systems are historically contingent (brand-based status is <200 years old, varies radically across cultures), the extraction is enforced through social visibility and marketing rather than physical law, and alternative status systems demonstrably exist and function. The mountain classification is a false summit revealing naturalization of institutional arrangements as universal laws.
constraint_indexing:constraint_classification(consumer_status_signaling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_status_signaling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consumer_status_signaling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_status_signaling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consumer_status_signaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consumer_status_signaling, TR),
    TR >= 0.70.

:- end_tests(consumer_status_signaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Measured as the ratio of price premium paid for status versus utility-based value. Luxury goods average 300-500% price premiums over functionally equivalent mass-market alternatives. The 0.58 value reflects that extraction is substantial but not maximal — some value is legitimate (authenticity signals, durability, actual quality differences) and some consumers genuinely prefer the goods independent of status. Suppression (0.68): Moderate-high. Suppression is enforced through social visibility (clothing, vehicles, housing are publicly observable), workplace expectations (professional dress codes, credential displays), and peer-based status hierarchies. Exit costs are high because status markers are visible and withdrawal is socially legible. However, suppression is not total — alternative status communities exist and provide partial exit. Theater ratio (0.65): Moderate-high. Brand marketing creates aspirational narratives and heritage stories that are substantially decoupled from functional product attributes. The constraint's maintenance increasingly depends on storytelling and social comparison rather than genuine scarcity or utility differences. The theater has increased over the interval as digital marketing and social media have amplified status comparison mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between beneficiaries who experience Rope (genuine coordination through scarcity allocation) and victims who experience Snare (pure extraction through manufactured insecurity). The middle positions (Tangled Rope for professionals, Scaffold for alternatives) reveal the constraint's hybrid nature. The Piton perspective indicates institutional degradation — the mechanism persists through inertia. The Mountain risk indicates naturalization of what is actually a historically contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary classification (luxury producers, brand institutions) derives from their position as net recipients of status-seeking spending and their arbitrage exit options — they profit when status competition intensifies and can exit by shifting business models. Victim classification (low-income consumers, aspirational professionals) derives from their disproportionate extraction (higher income share devoted to status consumption) and limited exit options (trapped by social visibility for wage earners, identity-locked for professionals). The identity_locked exit for middle-class professionals reflects that their professional identity is constituted through status markers (clothing, credentials, workplace signals) and that exit would require abandoning not just consumption patterns but their occupational identity frame. This is distinguishable from trapped (external barriers are insurmountable) or constrained (high-cost but surmountable barriers): the barrier is cognitive/identity-based, not structural.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disambiguating the structure across perspectives. From the beneficiary's view (Rope), the constraint solves a genuine coordination problem: how to allocate positional goods to those who value them most through price-based signaling. From the victim's view (Snare), the constraint is pure extraction: artificial scarcity drives spending and status competition is zero-sum. The Tangled Rope perspective (professionals with identity-locked exit) reveals the hybrid mechanism: the coordination function is real (career signaling) but embedded in asymmetric extraction (professionals pay above-market prices because they cannot exit the status frame). The Scaffold perspective confirms the constraint's contingency: alternative status systems demonstrably exist and function, which proves the extraction is not inherent to status hierarchies but to this specific institutional form. The Piton perspective indicates the mechanism is degrading — maintained by inertia rather than real function. The false Mountain risk reveals the critical danger: naturalizing institutional arrangements as universal laws prevents recognition of alternative possibilities. Mandatrophy is resolved: the constraint is a Snare at the powerless perspective (maximum extraction, minimum exit), a Tangled Rope at the moderate perspective (mixed coordination and extraction with identity lock), and a Rope at the beneficiary perspective (genuine coordination with net benefit). No single type is 'correct' — the presheaf over observation positions IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    status_necessity_vs_constraint,
    'Is status signaling a necessary human need or a manufactured constraint amplified by marketing and social comparison?',
    'Cross-cultural comparison of status consumption intensity; controlled intervention studies on effect of social visibility on consumption choices; longitudinal tracking of status priorities across societies with and without brand-based status systems',
    'If necessary: constraint is closer to Mountain (inherent to human nature). If manufactured: constraint is pure Snare (extraction through artificial scarcity and manufactured insecurity). Current evidence favors manufactured/amplified interpretation — status consumption intensity correlates with advertising exposure, not with utility maximization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_necessity_vs_constraint, empirical, 'Whether status signaling is intrinsic human need or manufactured through marketing').

omega_variable(
    identity_lock_mechanism_depth,
    'For middle-class professionals, is the status-consumption identity lock truly cognitive/identity-based, or primarily constrained by rational career signaling requirements?',
    'Qualitative analysis distinguishing between: (a) agents who consume for career signaling rationale and could articulate alternative status systems, versus (b) agents who experience their professional identity as inseparable from brand consumption and cannot envision alternatives. Survey data on perceived necessity vs identity fusion of status consumption.',
    'If primarily constrained: reclassify aspirational middle class from identity_locked to constrained (high cost exit, not identity barrier). If primarily identity-fused: identity_locked classification holds and reveals cognitive capture mechanism. If mixed: declare two separate stories (one for career signaling, one for identity fusion) per ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_depth, empirical, 'Depth of identity fusion vs rational career signaling in middle-class status consumption').

omega_variable(
    suppression_internalization_ratio,
    'What proportion of measured suppression (0.68) is structural (external barriers to exit: social stigma, visible consumption norms, employer expectations) versus internalized (agent accepts status competition as legitimate)?',
    'Analysis of suppression persistence post-exit: agents who opt out of status consumption and assess whether suppression mechanisms (shame, social isolation) persist or dissolve. Distinction between structural suppression (external enforcement continues) and internalized suppression (agent carries constraints internally after leaving peer group).',
    'If suppression is primarily structural: barrier to exit is external visibility (can change by relocation or community choice). If primarily internalized: constraint is stronger than measured because agent replicates enforcement internally. Affects whether snare classification accounts for full extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ratio, empirical, 'Structural vs internalized suppression in status signaling mechanism').

omega_variable(
    luxury_brand_coordination_reality,
    'Does luxury brand pricing actually solve an allocation coordination problem, or is it pure extraction using scarcity as a cover story?',
    'Empirical comparison: (a) do luxury goods genuinely allocate to highest-value consumers (revealed preference test), or (b) do they simply extract from aspirational consumers willing to pay for status rather than utility? Analysis of actual scarcity vs artificial scarcity; price-elasticity testing.',
    'If genuine coordination: luxury brand perspective (Rope) is structurally accurate — high-value consumers sorted from low-value consumers through price. If pure extraction: rope classification is false and should be snare — scarcity is artificial, coordination function is theater, extraction is maximal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(luxury_brand_coordination_reality, empirical, 'Whether luxury pricing solves coordination or enables pure extraction').

omega_variable(
    alternative_status_systems_scalability,
    'Can alternative status systems (skill-based, value-based, digital-native hierarchies) scale to replace positional consumption, or are they structurally limited to subcommunities?',
    'Longitudinal tracking of alternative status communities; analysis of their size, growth rate, and generational cohesion; identification of barriers to scaling (network effects favoring established systems, cognitive accessibility of traditional status markers, integration with employment/credibility signaling)',
    'If scalable: scaffold sunset clause is real — constraint''s extraction mechanism will decline as alternative status systems mature. If limited to subcommunities: scaffold perspective is aspirational rather than structural — most agents will remain trapped in positional consumption indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_status_systems_scalability, empirical, 'Scalability of alternative status systems to replace positional consumption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_status_signaling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(css_tr_t0, consumer_status_signaling, theater_ratio, 0, 0.48).
narrative_ontology:measurement(css_tr_t5, consumer_status_signaling, theater_ratio, 5, 0.57).
narrative_ontology:measurement(css_tr_t10, consumer_status_signaling, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(css_be_t0, consumer_status_signaling, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(css_be_t5, consumer_status_signaling, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(css_be_t10, consumer_status_signaling, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consumer_status_signaling, identity_coordination).
narrative_ontology:affects_constraint(consumer_status_signaling, conspicuous_consumption_treadmill).
narrative_ontology:affects_constraint(consumer_status_signaling, social_comparison_status_anxiety).
narrative_ontology:affects_constraint(consumer_status_signaling, brand_identity_fusion).

% DUAL FORMULATION NOTE:
% Consumer status signaling is an umbrella constraint decomposable into three structurally distinct mechanisms: (1) conspicuous_consumption_treadmill (ε≈0.65, Snare) — pure extraction through positional competition, (2) social_comparison_status_anxiety (ε≈0.52, Tangled Rope) — mixed coordination of peer expectations and psychological extraction via comparison, (3) brand_identity_fusion (ε≈0.48, identity_locked Tangled Rope) — professional identity integration with status markers. Each has different mechanisms and targets. The 0.58 base extractiveness is the weighted average; higher precision requires decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consumer_status_signaling, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
