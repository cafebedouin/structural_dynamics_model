% ============================================================================
% CONSTRAINT STORY: middle_east_telecom_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_middle_east_telecom_competition, []).

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
 *   constraint_id: middle_east_telecom_competition
 *   human_readable: Middle East Telecom Market Competition and Regulatory Extraction
 *   domain: telecommunications/regulatory_capture/regional_economics
 *
 * SUMMARY:
 *   The Middle East telecom market exhibits a hybrid coordination-extraction
 *   structure where regulatory licensing, spectrum allocation, and
 *   interconnection frameworks serve both genuine coordination functions
 *   (preventing wasteful duplication, enabling service standards) and
 *   incumbent protection mechanisms (blocking new entrants, sustaining
 *   above-market pricing). The constraint embodies regulatory capture:
 *   national regulators depend on licensing fee revenue, incumbent operators
 *   lobby to maintain spectrum monopolies, and barriers to entry are
 *   justified through infrastructure cost and spectrum scarcity arguments
 *   that may or may not reflect technical reality. The extractiveness
 *   trajectory (0.38→0.58 over the interval) reflects deepening rent-seeking:
 *   as mobile competition has technically undermined traditional monopoly
 *   justifications, incumbents have shifted toward regulatory extraction
 *   mechanisms to maintain market power. The theater ratio (0.42→0.55) shows
 *   increasing performativity in 'competitive' regulation — regulators
 *   conduct spectrum auctions and licensing reviews that maintain the
 *   appearance of market discipline while substantive control remains with
 *   incumbents.
 *
 * KEY AGENTS:
 *   - Price-Burdened Consumer: Primary victim (powerless/trapped) — faces oligopolistic pricing with no exit from national market; telecommunications access is non-discretionary
 *   - Aspiring Market Entrant: Secondary victim (moderate/constrained) — faces high licensing costs, spectrum barriers, and interconnection fees; can potentially move to other national markets at significant cost
 *   - Incumbent Operator: Primary beneficiary (institutional/arbitrage) — captures monopoly/near-monopoly rents through spectrum allocation, licensing framework, and interconnection control; can arbitrage across technologies and markets
 *   - National Regulator: Conflicted institutional actor (institutional/constrained) — dependent on telecom licensing revenue for government budget; constrained by incumbent lobbying and international standards; benefits from coordination function but bears extraction pressure
 *   - Legacy Monopoly Structure: Institutional mechanism (institutional/arbitrage) — regulatory framework maintaining extracted value through inertia despite technological changes undermining monopoly justification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent regulatory arrangements as immutable laws of telecom physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(middle_east_telecom_competition, 0.58).
domain_priors:suppression_score(middle_east_telecom_competition, 0.68).
domain_priors:theater_ratio(middle_east_telecom_competition, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(middle_east_telecom_competition, extractiveness, 0.58).
narrative_ontology:constraint_metric(middle_east_telecom_competition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(middle_east_telecom_competition, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(middle_east_telecom_competition, tangled_rope).
narrative_ontology:human_readable(middle_east_telecom_competition, "Middle East Telecom Market Competition and Regulatory Extraction").
narrative_ontology:topic_domain(middle_east_telecom_competition, "telecommunications/regulatory_capture/regional_economics").

domain_priors:requires_active_enforcement(middle_east_telecom_competition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(middle_east_telecom_competition, incumbent_operators).
narrative_ontology:constraint_beneficiary(middle_east_telecom_competition, government_revenue_streams).
narrative_ontology:constraint_victim(middle_east_telecom_competition, consumer_pricing_pressure).
narrative_ontology:constraint_victim(middle_east_telecom_competition, market_entry_competitors).
narrative_ontology:constraint_victim(middle_east_telecom_competition, network_infrastructure_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRICE-BURDENED CONSUMER (SNARE) — Trapped within national boundaries with limited service alternatives. Faces oligopolistic pricing with no viable exit. Must continue telecommunications access; cannot switch to competing infrastructure. Bears full extraction through inflated tariffs while incumbents maintain market control via regulatory barriers.
constraint_indexing:constraint_classification(middle_east_telecom_competition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING MARKET ENTRANT (TANGLED ROPE) — Constrained by spectrum licensing costs, infrastructure duplication requirements, and interconnection fees. Benefits from coordinated regional spectrum standards and shared infrastructure frameworks, but faces asymmetric extraction through licensing barriers and interconnection terms controlled by incumbents. Can relocate operations across borders at high cost; cannot easily enter specific national market.
constraint_indexing:constraint_classification(middle_east_telecom_competition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT OPERATOR (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: regulatory licensing framework enables service delivery coordination, prevents destructive duplication of infrastructure, and establishes interconnection standards. Net beneficiary from market structure; has arbitrage options across markets and technologies. Extraction flows toward this agent through spectrum monopoly and interconnection premium.
constraint_indexing:constraint_classification(middle_east_telecom_competition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL REGULATOR (TANGLED ROPE) — Constrained by state revenue dependency on telecom licensing fees and government budget pressure. Benefits from coordinated regulatory framework that prevents wasteful infrastructure competition and ensures service coverage obligations. But faces asymmetric extraction through political pressure from incumbent operators, budget pressure requiring high licensing fees, and international standards bodies that limit autonomy. Cannot easily exit regulatory role; constrained by fiscal and political realities.
constraint_indexing:constraint_classification(middle_east_telecom_competition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY MONOPOLY STRUCTURE (PITON) — The formerly-monopoly incumbent telecom remains locked in a regulatory protection regime even as mobile competition has technically undermined the monopoly justification. Maintains extracted value through regulatory inertia: licensing framework, interconnection terms, and spectrum allocation continue to protect the legacy operator despite technological shifts making pure monopoly unnecessary. Theater ratio reflects performative 'competition' oversight while substantive control remains with entrenched incumbent. Regulatory ritual persists because exit costs for both regulator and incumbent are high.
constraint_indexing:constraint_classification(middle_east_telecom_competition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, some market concentration in telecom is presented as structurally inevitable: high infrastructure costs, need for coordinated spectrum allocation, and natural monopoly characteristics of network effects are claimed to make perfect competition impossible. This perspective naturalizes regulatory capture as an unavoidable property of telecom markets. However, structural data contradicts the mountain classification — the high extractiveness (0.58), active enforcement requirements, and suppression (0.68) all point to contingent institutional arrangements, not natural laws.
constraint_indexing:constraint_classification(middle_east_telecom_competition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(middle_east_telecom_competition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(middle_east_telecom_competition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(middle_east_telecom_competition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(middle_east_telecom_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(middle_east_telecom_competition, TR),
    TR >= 0.70.

:- end_tests(middle_east_telecom_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting that the market functions with nominal competition (multiple operators exist) but extractive outcomes persist (pricing above cost, barriers to entry, limited service innovation). The extractiveness has increased from 0.38 to 0.58 over the interval as regulatory barriers have substituted for natural monopoly ones. Suppression (0.68): High. Barriers to exit are significant: consumers face duopoly/oligopoly pricing with limited alternatives; entrants face licensing costs (often $500M-$1.5B in Gulf states), spectrum acquisition costs, and interconnection fee structures controlled by incumbents. National borders create hard constraints on roaming alternatives in many markets. Theater ratio (0.55): Moderate-rising. Regulatory 'competition' frameworks conduct spectrum auctions and licensing reviews that project market discipline while substantive market power remains concentrated. The theater has increased as visible competition (multiple operators) has created appearance of functional market without substantive constraint on pricing or innovation. The tangled_rope classification holds: genuine coordination functions (infrastructure standards, spectrum coordination, service coverage obligations) coexist with asymmetric extraction (licensing fees that exceed coordination costs, interconnection terms favoring incumbents, spectrum allocation maintaining barriers).
 *
 * PERSPECTIVAL GAP:
 *   The incumbent operator sees rope — a coordination mechanism enabling their business model and managing infrastructure complexity. The national regulator sees tangled rope — they coordinate the market but are themselves extracted from by incumbent lobbying pressure and constrained by budget dependency on licensing fees. Aspiring entrants see snare-like extraction through licensing barriers and interconnection terms. Price-burdened consumers see pure extraction (snare) — they have no exit and bear oligopolistic pricing. The analytical observer risks the false summit (mountain) by naturalizing regulatory barriers as inevitable properties of telecom infrastructure, when structural data reveals them as contingent institutional choices. The legacy monopoly structure appears as piton — the regulatory protection regime persists through inertia even as technological changes (mobile competition, satellite broadband, VoIP) have eroded the technical justification for the original monopoly.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: the incumbent operator benefits from spectrum allocation and licensing framework (low directionality, high positive extraction flow toward them); consumers and entrants face high costs and limited options (high directionality, high extraction flow away from them); the regulator is caught between coordination needs and revenue dependency (intermediate directionality with extraction pressure from incumbents and constraint from political economy). Beneficiary declarations (incumbent operators, government revenue) and victim declarations (consumer pricing pressure, market entry competitors, infrastructure equity) map to real extraction flows: incumbents capture spectrum rents through licensing monopoly, and governments depend on licensing fees for budget revenue. The asymmetric power is encoded: institutional power + arbitrage exit for incumbents → low d; powerless + trapped for consumers → high d; institutional power + constrained exit for regulators → intermediate d. These derivations produce the perspectival gap: beneficiaries see coordination, victims see extraction, regulators see contingent commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the coordination function (infrastructure standardization, spectrum allocation preventing interference, service coverage obligations) is real and justified, but the extraction magnitude (0.58 extractiveness, 0.68 suppression) exceeds the coordination cost. The constraint is tangled rope precisely because both elements are present: genuine coordination value that would justify some regulatory structure, combined with incumbents' use of that regulatory structure to extract rents beyond coordination necessity. The classification prevents two mislabelings: (1) calling it pure rope by ignoring the extraction asymmetry, and (2) calling it pure snare by ignoring that some barriers (spectrum allocation) serve real coordination functions. The tangled rope classification holds the tension: yes, this regulation coordinates the market; also yes, incumbents have captured that coordination mechanism for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_duplication_necessity,
    'Is infrastructure duplication genuinely wasteful and economically inefficient, or does this claim mask incumbent protection?',
    'Cross-national comparison: markets with multiple infrastructure operators (Europe, North America) vs monopoly/duopoly structures (Gulf region). Cost-benefit analysis of competition vs duplication.',
    'If genuinely wasteful: the coordination function is real and tangled_rope classification holds. If primarily incumbent protection: classification shifts toward snare, and suppression represents artificial scarcity, not coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_duplication_necessity, empirical, 'Whether infrastructure duplication is genuinely inefficient or primarily protects incumbents').

omega_variable(
    spectrum_scarcity_vs_allocation,
    'Does spectrum scarcity justify exclusive licensing, or does auction/dynamic allocation prove abundance at market-clearing prices?',
    'Technical analysis of spectrum utilization rates; comparison of exclusive licensing models vs open-access dynamic spectrum; long-term trend in spectrum availability as technology improves (5G, 6G, satellite capacity).',
    'If scarcity is real: regulatory licensing is coordination necessity (mountain/rope properties). If scarcity is allocated (not inherent): exclusive licensing is extraction mechanism (snare/tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spectrum_scarcity_vs_allocation, empirical, 'Whether spectrum scarcity justifies exclusive licensing or reveals allocation choice').

omega_variable(
    regulatory_capture_magnitude,
    'How much of the suppression (0.68) derives from technical necessity vs political capture by incumbent operators?',
    'Comparative regulatory analysis: correlation between licensing fee magnitude and government budget pressures; analysis of regulatory decisions favoring incumbents vs consumer welfare; survey of regulator independence metrics (GSMA Intelligence, World Bank Doing Business).',
    'If capture is primary: suppression is political extraction, not coordination cost. Classification emphasizes snare/tangled rope from regulator''s perspective. If technical necessity dominates: regulatory constraints are genuine coordination functions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_magnitude, empirical, 'Proportion of suppression attributable to capture vs technical necessity').

omega_variable(
    cross_border_arbitrage_viability,
    'Can consumers or businesses effectively arbitrage across national telecom markets (roaming, VPNs, satellite), or are national borders effectively hard barriers?',
    'Cost analysis of roaming vs domestic service; VPN adoption and regulatory blocking; satellite broadband (Starlink) penetration and legal status by nation; empirical exit flows when service quality degrades.',
    'If cross-border arbitrage is viable: exit_options for trapped consumers should be ''constrained'' not ''trapped'', classification shifts from snare. If borders are hard barriers: suppression (0.68) is correctly high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_border_arbitrage_viability, empirical, 'Whether national borders create hard barriers or soft constraints to telecom arbitrage').

omega_variable(
    competitor_entry_barriers_origin,
    'Are barriers to market entry technical (spectrum scarcity, infrastructure cost) or regulatory (licensing, interconnection terms, frequency allocation)?',
    'Decompose entry barriers: licensing cost, spectrum auction price, infrastructure build cost, interconnection fees, and time-to-service. Compare across regional markets with different regulatory regimes (Saudi Arabia vs UAE vs Egypt patterns).',
    'If primarily technical: barriers are coordination functions; tangled_rope classification holds. If primarily regulatory: barriers are extractive; classification shifts toward snare for entrants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competitor_entry_barriers_origin, empirical, 'Whether entry barriers are technical or regulatory in origin').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(middle_east_telecom_competition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metc_tr_t0, middle_east_telecom_competition, theater_ratio, 0, 0.42).
narrative_ontology:measurement(metc_tr_t5, middle_east_telecom_competition, theater_ratio, 5, 0.5).
narrative_ontology:measurement(metc_tr_t10, middle_east_telecom_competition, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(metc_be_t0, middle_east_telecom_competition, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(metc_be_t5, middle_east_telecom_competition, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(metc_be_t10, middle_east_telecom_competition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(middle_east_telecom_competition, resource_allocation).
narrative_ontology:affects_constraint(middle_east_telecom_competition, middle_east_internet_infrastructure).
narrative_ontology:affects_constraint(middle_east_telecom_competition, gulf_state_media_regulation).
narrative_ontology:affects_constraint(middle_east_telecom_competition, digital_services_taxation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(middle_east_telecom_competition, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
