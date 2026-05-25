% ============================================================================
% CONSTRAINT STORY: attention_market_cannibalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_market_cannibalization, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: attention_market_cannibalization
 *   human_readable: The Cognitive Exhaustion Loop
 *   domain: economic/psychological/technological
 *
 * SUMMARY:
 *   The cognitive exhaustion loop represents a structural form of attention
 *   extraction where competing digital platforms optimize engagement metrics
 *   (time-on-app, interactions, watch duration) in ways that cannibalize the
 *   attentional bandwidth users require for primary life functions (work,
 *   health, relationships, civic participation). Unlike traditional monopoly
 *   or labor extraction, this constraint operates through algorithmic
 *   optimization, physiological reward hijacking (notification loops,
 *   infinite scroll, algorithmic relevance), and network effects that make
 *   exit economically and socially impossible despite being individually
 *   'voluntary.' The constraint exhibits Snare characteristics because
 *   suppression is structural: users cannot exit without significant
 *   life-function costs (professional isolation, information deprivation,
 *   social disconnection); platform operators have no competitive incentive
 *   to reduce extraction; and alternative coordination mechanisms
 *   (regulation, user coalitions, interoperable platforms) face coordination
 *   problems and regulatory capture. However, the constraint also exhibits
 *   tangled_rope characteristics from the regulatory perspective because
 *   extraction is bundled with genuine coordination benefits (connection,
 *   information access, economic opportunity), making pure suppression
 *   impossible without destroying the coordination function. The temporal
 *   measurement shows progressive increase in extractiveness (from 0.22 to
 *   0.58) driven by algorithmic improvements in engagement prediction,
 *   competitive platform proliferation, and migration of institutional
 *   functions (work, education, healthcare) onto platforms. Theater_ratio
 *   increases from 0.15 to 0.61, indicating that wellness interventions
 *   (screen time monitoring, digital detox narratives) have become
 *   increasingly performative and have not reduced underlying extraction.
 *
 * KEY AGENTS:
 *   - Attention Subject (Individual User): Primary victim (powerless/trapped) — faces algorithmic extraction with no meaningful exit due to social/economic necessity of platform participation
 *   - Cognitive Commons: Collective victim (powerless/trapped) — population-level epistemic capacity degraded by fragmentation and exhaustion; cannot organize due to attentional overwhelm
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture attention surplus via engagement optimization; benefit from advertising revenue and data extraction
 *   - Attention Arbitrageurs: Secondary beneficiary (powerful/arbitrage) — advertisers, data brokers, attention-trading markets that profit from the extracted and commodified attention
 *   - Regulatory Coalition: Mixed agent (organized/mobile) — states, consumer protection agencies, civil society organizations with both interest in coordination value (economic activity) and extraction costs (public health)
 *   - Attention Wellness Industry: Theatrical maintainer (organized/constrained) — digital detox apps, meditation platforms, wellness programs that perform attention-control without functional impact
 *   - Analytical Observer: Civilian vantage (analytical/analytical) — risks naturalizing contingent engineering choices as inevitable biological or economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_market_cannibalization, 0.58).
domain_priors:suppression_score(attention_market_cannibalization, 0.68).
domain_priors:theater_ratio(attention_market_cannibalization, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_market_cannibalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(attention_market_cannibalization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(attention_market_cannibalization, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_market_cannibalization, snare).
narrative_ontology:human_readable(attention_market_cannibalization, "The Cognitive Exhaustion Loop").
narrative_ontology:topic_domain(attention_market_cannibalization, "economic/psychological/technological").

domain_priors:requires_active_enforcement(attention_market_cannibalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_market_cannibalization, platform_operators).
narrative_ontology:constraint_beneficiary(attention_market_cannibalization, attention_arbitrageurs).
narrative_ontology:constraint_victim(attention_market_cannibalization, cognitive_commons).
narrative_ontology:constraint_victim(attention_market_cannibalization, primary_life_function_maintenance).
narrative_ontology:constraint_victim(attention_market_cannibalization, attention_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ATTENTION SUBJECT (SNARE) — Individual user facing algorithmic extraction with no meaningful exit. Trapped by social necessity (work collaboration, family contact, cultural participation all migrate to platforms), physiological reward hijacking (dopamine/notification loops), and competitive pressure from peers. Bears full cost of attention cannibalization while receiving only deteriorating primary-life-function maintenance. Maximum experienced extraction.
constraint_indexing:constraint_classification(attention_market_cannibalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE COGNITIVE COMMONS (SNARE) — Collective epistemic capacity (population-level ability to coordinate on factual matters, maintain shared reality, sustain attention for complex problems). Trapped by network effects — individual exit is impossible when institutions and social coordination require platform participation. Cannot organize because its 'members' (cognitive actors) are individually overwhelmed. Bears cost of fragmentation, epistemic degradation, and collective action paralysis.
constraint_indexing:constraint_classification(attention_market_cannibalization, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PLATFORM OPERATOR (ROPE) — Sees attention extraction as coordination: connecting users, facilitating information flow, enabling commerce. Net beneficiary with arbitrage options (can exit engagement optimization without shutting down platform, but chooses not to; can shift to subscription or other models). Experiences the constraint as pure coordination — the extraction mechanism is invisible to this agent because they designed it.
constraint_indexing:constraint_classification(attention_market_cannibalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REGULATORY COALITION (TANGLED ROPE) — States, consumer protection agencies, and organized civil society perceive both coordination value (platforms enable economic activity, communication, information access) and extraction (cognitive manipulation, undermining of human agency, public health externalities). Has exit options (regulation, taxation, interoperability mandates) but constrained by regulatory capture and complexity of digital markets. Benefits from platform tax revenue; bears cost of public mental health degradation and political polarization.
constraint_indexing:constraint_classification(attention_market_cannibalization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ATTENTION WELLNESS RITUAL (PITON) — Digital detox movements, 'screen time' monitoring apps, meditation practices, and wellness narratives. High theater_ratio (0.61): most effort is performative — temporary abstinence, app-mediated limitation, personal guilt management — with minimal functional impact on the underlying extraction mechanism. The wellness ritual persists through institutional inertia: apps that gamify time limits, meditation apps that compete for attention while claiming to reduce it, wellness programs sponsored by platform operators. Primary function (preventing cognitive exhaustion) has atrophied; theatrical maintenance (the appearance of personal control) remains.
constraint_indexing:constraint_classification(attention_market_cannibalization, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational vantage, cognitive exhaustion appears as an immutable property of human neurobiology: attention is finite, reward systems are exploitable, and competition for attention is inevitable. The narrative naturalizes: 'engagement optimization is just economics' or 'dopamine-driven behavior is just human nature.' However, the structural data contradicts mountain classification — the extractiveness (0.58), suppression (0.68), and video game aesthetics of algorithmic feeds are contingent design choices, not natural laws. The engine identifies this as false natural law / naturalized extraction.
constraint_indexing:constraint_classification(attention_market_cannibalization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_market_cannibalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_market_cannibalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_market_cannibalization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_market_cannibalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_market_cannibalization, TR),
    TR >= 0.70.

:- end_tests(attention_market_cannibalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. Platforms extract significant cognitive bandwidth through algorithmic optimization, but the extraction is not 100% of available attention — users maintain minimal work and relationship functioning (extractiveness would be 0.90+ if primary functions completely failed). The measurement trajectory from 0.22 to 0.58 over 10 time units reflects acceleration driven by three factors: (1) algorithmic improvements in engagement prediction (machine learning models trained on billions of engagement events now optimize reward loops with precision); (2) competitive platform proliferation (users juggle multiple feeds, each optimized for engagement); (3) institutional migration onto platforms (work coordination, education delivery, healthcare information, civic participation). Suppression (0.68): High. Structural barriers prevent meaningful exit: social necessity (peer networks, professional requirements, family coordination), physiological barriers (reward system hijacking through dopamine-contingent notifications), and network effects (individual exit is ineffective if institutional partners remain). Alternatives require collective action, but collective cognitive capacity is degraded by exhaustion itself. Theater_ratio (0.61): High and increasing. Wellness rituals (screen time app limits, digital detox, meditation) are performative — they require effort, create appearance of control, but do not disrupt the underlying extraction mechanism. Apps that gamify time limits compete for attention while claiming to reduce it. Wellness programs sponsored by platform operators embed extraction even in the 'resistance' layer. The ratio would be lower if interventions actually disrupted engagement optimization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival gap. The platform operator perceives pure coordination (Rope) — users voluntarily exchange personal data and attention time for access to network effects and information. The user perceives pure extraction (Snare) — algorithmic optimization is invisible and compulsive, choice is simulated, exit is impossible. The regulatory coalition perceives tangled_rope — they must preserve the coordination benefits (connection, commerce, information access) while reducing extraction, a structurally impossible task given current platform architectures. The wellness industry perceives piton — they maintain performative interventions that create the appearance of user control without disrupting the underlying mechanism. The analytical observer risks perceiving natural law (mountain) — 'attention economics are inevitable, dopamine is immutable, competition requires engagement optimization' — but these are contingent institutional arrangements. The gap arises because the beneficiary designed the constraint to be invisible to users while maximizing extraction. Platform operators do not experience the constraint because they built it to extract from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from power level, exit options, and beneficiary/victim status. The attention subject (powerless/trapped/victim) derives high d ≈ 0.95 → maximum f(d) ≈ 1.42 → high χ even at moderate ε. The cognitive commons (powerless/trapped/victim) similarly derives high d despite being an abstract collective. The platform operator (institutional/arbitrage/beneficiary) derives low d ≈ 0.05 → f(d) ≈ -0.12 → negative χ (they experience the constraint as beneficial coordination, not extraction). The regulatory coalition (organized/mobile/mixed) derives d ≈ 0.50-0.55 from mixed beneficiary/victim status and mobile exit options → f(d) ≈ 0.65-0.75 → moderate χ. The analytical observer (analytical/analytical/observer) derives d ≈ 0.72 from observer position. The directionality structure reveals why the beneficiary (platform) sees Rope while the victim sees Snare — their d values differ by nearly an order of magnitude despite operating within the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint clarifies how to avoid conflating coordination mechanisms with extraction mechanisms. The platform operator's Rope classification is their honest experience of what they built: a coordination tool that connects people. The snare classification from the user perspective is also honest: they are trapped by algorithmic optimization despite perceiving choice. The mandatrophy is resolved by recognizing that the same structural object carries both genuine coordination value AND structural extraction. Neither classification is 'the right one' — both are partial truths about the indexed positions. The tangled_rope classification from the regulatory perspective captures this: genuine coordination function (connection, information, commerce) PLUS asymmetric extraction (attention surplus, behavioral data, cognitive manipulation). The resolution prevents false naturalizing (mountain perspective claims extraction is inevitable) and false optimization rhetoric (platform claims there is no extraction, only coordination). The measurement data shows temporal progression: early platforms (extractiveness 0.22) were genuinely closer to pure coordination; contemporary platforms (extractiveness 0.58) have added extraction mechanisms on top of coordination. The theater_ratio increase indicates that resistance mechanisms are themselves being colonized and rendered performative. Mandatrophy is finally resolved because all six types appear in the perspectival presheaf: the object is intrinsically multivalent, and no single classification can be correct from all vantages simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coerced_engagement,
    'Is the attention extraction purely voluntary (users choosing to engage) or structurally coerced (social/economic necessity masquerading as choice)?',
    'Measurement of switching costs: cost of exit (social isolation, work friction, information deprivation) vs perceived utility gain from continued engagement. Comparison of engagement curves before/after algorithmic darkening (when platforms accidentally reduce extraction) vs intentional user choice to reduce.',
    'If predominantly voluntary: classification shifts toward Rope (coordination benefit retained). If predominantly coerced: validates Snare from user perspective (no genuine exit despite perceived choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_engagement, empirical, 'Whether engagement is truly voluntary or structurally coerced').

omega_variable(
    primary_life_function_maintenance_threshold,
    'What quantitative threshold of attentional bandwidth allocation preserves primary life-function maintenance? Below what percentage for work/health/relationship investment does systemic degradation occur?',
    'Longitudinal cohort studies: cognitive load allocation, performance metrics in primary functions, health outcomes, relationship stability, economic productivity, correlated with platform engagement time. Identify inflection point where increasing engagement causes decreasing life-function performance.',
    'If threshold exists at ~60% non-platform time (40% platform): current extraction exceeds system capacity. If threshold is lower: extraction is sustainable. Affects classification of ''cannibalization'' claim — determines if extraction is consuming capacity needed for non-extractive functions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_life_function_maintenance_threshold, empirical, 'Cognitive bandwidth threshold for maintaining primary life functions').

omega_variable(
    platform_competitive_necessity,
    'Do platforms compete on engagement metrics (race to the bottom in cognitive extraction) or could they compete on other dimensions (functionality, privacy, user autonomy) without losing market share?',
    'Economic analysis of platform revenue models, user switching costs, and feature competition. Natural experiments: platforms that reduce engagement optimization (e.g., Twitter rebranding, TikTok time limits in EU). Market share impact and revenue consequences. Cross-platform comparison of engagement-optimization intensity vs market success.',
    'If competition structurally requires engagement extraction: Snare classification validated (no exit for beneficiaries without losing market position). If competition could occur on other dimensions: extraction is strategic choice, not necessity — validates tangled_rope from platform perspective (they benefit from extraction but aren''t forced to extract).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_competitive_necessity, conceptual, 'Whether platform competition requires engagement extraction or enables alternatives').

omega_variable(
    collective_action_capacity_floor,
    'At what level of cognitive exhaustion does collective action capacity (ability of users to organize, advocate, regulate) drop below the threshold needed to resist further extraction?',
    'Time-series analysis: cognitive commons fragmentation metrics (epistemic polarization, news avoidance, political disengagement) against user cognitive load. Measure collective action outcomes (regulatory success, platform concessions) correlated with population-level attention allocation. Identify feedback loop: higher extraction → lower cognitive capacity for collective resistance → extraction accelerates.',
    'If collective action capacity reaches floor before extraction reaches equilibrium: trap is irreversible (regulatory or user-resistance solutions become impossible). If floor hasn''t been reached: intervention window still open. Determines if Snare is temporary (users can still organize) or permanent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_capacity_floor, empirical, 'Cognitive load threshold below which collective action capacity collapses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_market_cannibalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_market_cannibalization, theater_ratio, 0, 0.15).
narrative_ontology:measurement(attn_tr_t5, attention_market_cannibalization, theater_ratio, 5, 0.38).
narrative_ontology:measurement(attn_tr_t10, attention_market_cannibalization, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_market_cannibalization, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(attn_be_t5, attention_market_cannibalization, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(attn_be_t10, attention_market_cannibalization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_market_cannibalization, information_standard).
narrative_ontology:boltzmann_floor_override(attention_market_cannibalization, 0.35).
narrative_ontology:affects_constraint(attention_market_cannibalization, platform_algorithmic_capture).
narrative_ontology:affects_constraint(attention_market_cannibalization, digital_advertising_extraction).
narrative_ontology:affects_constraint(attention_market_cannibalization, epistemic_commons_fragmentation).

% DUAL FORMULATION NOTE:
% The cognitive exhaustion loop is downstream of individual platform design decisions (algorithmic ranking, notification scheduling, recommendation feed optimization) but represents a distinct constraint on the cognitive commons as a whole. Upstream constraints operate at the single-platform level; this constraint aggregates across multiple platforms and their competitive dynamics. The loop's extractiveness exceeds the sum of individual platform extractions because platform proliferation creates multiplicative attention demand (users juggle multiple feeds) and collective action problems (no individual platform can exit the race to engagement optimization without losing market share).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_market_cannibalization, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
