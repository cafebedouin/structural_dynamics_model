% ============================================================================
% CONSTRAINT STORY: dublin_newspaper_circulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dublin_newspaper_circulation, []).

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
 *   constraint_id: dublin_newspaper_circulation
 *   human_readable: Dublin Newspaper Circulation Distribution System
 *   domain: media/distribution/urban_economics
 *
 * SUMMARY:
 *   The Dublin newspaper circulation system is a distribution constraint that
 *   coordinates information flow to a dispersed urban population while
 *   simultaneously extracting rents from independent newsagents who operate
 *   as the final-mile distribution nodes. Major publishers (Irish
 *   Independent, Irish Times, etc.) benefit from a guaranteed retail presence
 *   without managing street-level logistics; wholesalers (Easons, other
 *   distributors) control the bottleneck between publishers and retailers;
 *   independent newsagents bear the structural cost of maintaining inventory,
 *   managing returns, and accepting unfavorable terms because they have no
 *   alternatives. The system exhibits tangled rope characteristics: genuine
 *   coordination function (newspapers reach readers through a reliable
 *   distribution network) alongside significant extraction (independent
 *   retailers are margin-squeezed, suburbs are under-served, entry barriers
 *   prevent competitive alternatives). Over the 2010-2024 interval,
 *   extractiveness has increased (digital transition has made physical
 *   distribution less valuable to publishers but they maintain it through
 *   institutional momentum, shifting costs to retailers) while theater has
 *   risen (much of the early-morning distribution activity now functions to
 *   maintain the appearance of print reach rather than to serve actual reader
 *   demand). The constraint is increasingly unstable: publishers have digital
 *   alternatives that reduce their dependence on the physical system, while
 *   independent newsagents have no exit and face inventory shrinkage as
 *   circulation declines.
 *
 * KEY AGENTS:
 *   - Major Newspaper Publishers (Irish Independent, Irish Times, Herald): Institutional beneficiaries (arbitrage exit) — capture reach metrics and advertising benefits; can arbitrage to digital
 *   - Distribution Wholesalers (Easons primary): Institutional beneficiary-beneficiary (arbitrage exit) — control margin between publishers and retailers; can shift to adjacent distribution categories
 *   - Independent Newsagents: Primary victims (powerless/trapped) — no alternatives to wholesale supply; bear inventory risk, capital costs, and margin pressure from circulation decline
 *   - Suburban/Peripheral Communities: Secondary victims (constrained) — under-served by distribution network because low margins make their zones unprofitable for wholesalers
 *   - Print Newspaper Industry Practice: Institutional inertia (constrained) — the same-day physical distribution requirement persists through habit and advertiser expectations despite declining reader reliance
 *   - Analytical Observer: Civilizational perspective — can see the system as both coordination (solves information distribution problem) and extraction (rents concentrated with wholesalers, costs distributed to retailers)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dublin_newspaper_circulation, 0.58).
domain_priors:suppression_score(dublin_newspaper_circulation, 0.48).
domain_priors:theater_ratio(dublin_newspaper_circulation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dublin_newspaper_circulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(dublin_newspaper_circulation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dublin_newspaper_circulation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dublin_newspaper_circulation, tangled_rope).
narrative_ontology:human_readable(dublin_newspaper_circulation, "Dublin Newspaper Circulation Distribution System").
narrative_ontology:topic_domain(dublin_newspaper_circulation, "media/distribution/urban_economics").

domain_priors:requires_active_enforcement(dublin_newspaper_circulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dublin_newspaper_circulation, major_newspaper_publishers).
narrative_ontology:constraint_beneficiary(dublin_newspaper_circulation, distribution_wholesalers).
narrative_ontology:constraint_victim(dublin_newspaper_circulation, independent_newsagents).
narrative_ontology:constraint_victim(dublin_newspaper_circulation, newspaper_accessibility_in_periphery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT NEWSAGENT (SNARE) — Trapped in distribution system with no alternatives. Forced to stock through major wholesalers at unfavorable terms or lose customer access. Cannot coordinate with other newsagents due to competition. Bears full cost of supply chain inefficiency.
constraint_indexing:constraint_classification(dublin_newspaper_circulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SECONDARY NEWSAGENTS (TANGLED ROPE) — Constrained by capital requirements and franchise agreements but also benefit from the distribution infrastructure. Can exit by closing shops but face significant sunk costs and employee layoffs. Mixed coordination and extraction.
constraint_indexing:constraint_classification(dublin_newspaper_circulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAJOR PUBLISHERS (ROPE) — Benefit from guaranteed distribution network without managing logistics directly. Can arbitrage between platforms (digital, print, subscriptions). See circulation as coordination: the distribution system solves the problem of reaching dispersed readers.
constraint_indexing:constraint_classification(dublin_newspaper_circulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WHOLESALERS (ROPE) — Institutional beneficiary with high arbitrage. Control the bottleneck between publishers and retailers. Can shift to digital distribution or expand to adjacent markets. Experience the system as coordination for publishers but extraction from retailers.
constraint_indexing:constraint_classification(dublin_newspaper_circulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRINT INDUSTRY STANDARD (PITON) — The same-day physical distribution requirement is increasingly theatrical as digital platforms dominate. The constraint persists through institutional inertia — publishers maintain expensive print distribution partly from habit, partly from advertisers who still demand print reach metrics. Theater_ratio high because much of the activity (early morning distribution, retail shelf placement) exists to maintain the appearance of reach rather than to satisfy actual reader demand.
constraint_indexing:constraint_classification(dublin_newspaper_circulation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the Dublin newspaper circulation system coordinates information distribution to a dispersed urban population while simultaneously extracting rents from independent retailers who have no alternatives. The system provides genuine coordination benefit (readers get newspapers, publishers reach audiences) alongside genuine extraction (retailers bear capital costs, suppliers bear inventory risk, suburbs are under-served). The constraint is not natural law but a contingent institutional arrangement that is both functional and extractive.
constraint_indexing:constraint_classification(dublin_newspaper_circulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dublin_newspaper_circulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dublin_newspaper_circulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dublin_newspaper_circulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dublin_newspaper_circulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dublin_newspaper_circulation, TR),
    TR >= 0.70.

:- end_tests(dublin_newspaper_circulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, trending upward. The system extracts from independent retailers through forced wholesale participation and margin compression, but extraction is not total — retailers can theoretically exit (by closing shops). The extractiveness has increased over the interval because digital transition has made physical distribution less valuable to publishers (reducing their willingness to fund competitive alternatives) while retailers remain locked in through sunk capital costs and lack of coordination mechanisms. Suppression (0.48): Moderate. Barriers to exit include capital requirements for retail operations, lack of alternative distribution channels, and geographic isolation of suburban retailers from coordination opportunities. But suppression is not absolute — some newsagents have successfully pivoted to convenience-store models or closed voluntarily. Theater ratio (0.65): Moderate-high. Early-morning newspaper delivery now functions partly as theater — the distribution activity maintains the appearance of print reach to advertisers and publishers even as reader demand declines. The theater has increased because the coordination function (reliable distribution) is now provided equally well by digital platforms, making the physical ritual increasingly performative. Claimed_type (Tangled Rope) reflects the analytical observer's perspective that the system provides both genuine coordination (information distribution to dispersed population) and genuine extraction (rent concentration, margin squeeze, alternative suppression).
 *
 * PERSPECTIVAL GAP:
 *   The most significant gap is between the publisher/wholesaler rope perspective (coordination) and the newsagent snare perspective (extraction). This gap reveals that the same constraint — the wholesale distribution system — is experienced as a coordination mechanism by beneficiaries and as an extraction mechanism by trapped retailers. The piton perspective (print industry standard) is aspirational — it rationalizes the constraint as a necessary standard while the functional analysis suggests it is increasingly theatrical. The analytical tangled rope perspective resolves the gap by showing that both readings are correct: the system does coordinate and does extract.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers and wholesalers are beneficiaries with institutional power and arbitrage exit options — they benefit from the constraint and can shift to adjacent markets or digital platforms if physical circulation becomes unprofitable. Their directionality d is low (0.1-0.2), producing negative or minimal χ because the constraint flows extraction toward them, not away. Independent newsagents are victims with powerless agency and trapped exit options — they depend on the wholesale system for their business model and cannot exit without closing shops. Their directionality d is high (0.85-0.95), producing high χ because the constraint extracts from them with minimal options. The analytical observer at the civilizational/global context uses the structural decomposition of coordination vs extraction to assign a moderate d (0.5-0.65), reflecting that the system is mixed — some benefits (newspapers distributed), some costs (retailers marginalized).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by decomposing the constraint into its coordination and extraction components. The coordination function is genuine but increasingly provided by digital alternatives (reducing its necessity). The extraction function is genuine and depends on the institutional lock-in of independent retailers who have no alternatives. As digital distribution grows, the coordination benefit declines while extraction persists — the system becomes progressively more purely extractive (moving toward snare). The tangled rope classification captures the current state (mixed), but the trajectory predicts motion toward pure snare as coordination necessity declines and extraction lock-in persists. The mandatrophy resolution is that the constraint is not mislabeled but is undergoing structural degradation — the rope component is eroding while the snare component is strengthening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_transition_timeline,
    'What is the effective timeline for digital distribution to fully replace physical newspaper circulation in Dublin?',
    'Tracking circulation decline rates by demographic, monitoring publisher digital subscription growth, observing retailer revenue collapse thresholds',
    'If < 5 years: the constraint degrades rapidly to piton. If > 15 years: extraction persists longer, enabling rent accumulation by wholesalers. Affects classification stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_transition_timeline, empirical, 'Timeline for digital replacement of physical circulation').

omega_variable(
    cooperative_newsagent_viability,
    'Could independent newsagents form a cooperative distribution network that bypasses major wholesalers?',
    'Cost analysis of cooperative logistics infrastructure; case studies from other cities (UK, Scandinavia); retailer willingness-to-participate surveys',
    'If viable: trapped exit option becomes constrained, reducing suppression and χ. Snare reclassifies as tangled_rope. If not viable: suppression confirmed as structural, snare classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperative_newsagent_viability, empirical, 'Viability of cooperative newsagent distribution networks').

omega_variable(
    suburban_underservice_causation,
    'Is suburban under-service a consequence of the extraction mechanism (wholesalers deprioritize low-margin zones) or of market efficiency (low demand justifies sparse distribution)?',
    'Price discrimination analysis; comparison of suburban circulation rates under cooperative vs wholesale distribution in pilot programs; demographic demand studies',
    'If extraction-driven: victims category expands to include suburban communities; suppression increases. If market-driven: underservice is not extractive, beneficiary/victim categories shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suburban_underservice_causation, empirical, 'Root cause of suburban newspaper under-service').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dublin_newspaper_circulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dublin_circ_tr_t0, dublin_newspaper_circulation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dublin_circ_tr_t5, dublin_newspaper_circulation, theater_ratio, 5, 0.55).
narrative_ontology:measurement(dublin_circ_tr_t10, dublin_newspaper_circulation, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(dublin_circ_be_t0, dublin_newspaper_circulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dublin_circ_be_t5, dublin_newspaper_circulation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dublin_circ_be_t10, dublin_newspaper_circulation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dublin_newspaper_circulation, resource_allocation).
narrative_ontology:affects_constraint(dublin_newspaper_circulation, irish_media_consolidation).
narrative_ontology:affects_constraint(dublin_newspaper_circulation, retail_digital_transition).

% DUAL FORMULATION NOTE:
% Dublin newspaper circulation is a downstream constraint from broader Irish media consolidation (which produced the publisher oligopoly) and is upstream of the retail digital transition (which threatens the physical distribution model). Decomposed from Irish media system constraints to focus on the specific extractive distribution mechanism rather than the publishing market structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dublin_newspaper_circulation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
