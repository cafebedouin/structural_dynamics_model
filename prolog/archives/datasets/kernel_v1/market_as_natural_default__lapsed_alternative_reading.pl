% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_lapsed_alternative, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed-Closure Artifact (Historical-Contingency Reading)
 *   domain: political_economy/institutional_theory/market_design
 *
 * SUMMARY:
 *   Market dominance in contemporary capitalism appears as a natural,
 *   inevitable feature of economic coordination — the default mechanism for
 *   allocating resources and organizing complex exchange. This narrative
 *   naturalizes what the lapsed-closure reading identifies as a historically
 *   contingent institutional arrangement. The constraint operates through a
 *   specific mechanism: centuries of active enclosure (legal innovations in
 *   property law, contract enforcement, intellectual property regimes) have
 *   systematically foreclosed pre-market coordination mechanisms (commons
 *   management, mutual aid, gift economies, barter networks). As these
 *   enclosure mechanisms became embedded in institutions and legal
 *   structures, the active maintenance burden decreased — the suppression
 *   became invisible, and the market appeared natural rather than engineered.
 *   The lapsed-closure reading traces this historical process: high active
 *   suppression during the enclosure period (17th-19th centuries), declining
 *   suppression as alternatives were progressively eliminated, rising theater
 *   ratio as naturalization narrative replaces active enforcement as the
 *   maintenance mechanism. Contemporary market dominance is therefore
 *   defended not through explicit suppression of alternatives but through the
 *   much more effective strategy of making alternatives cognitively
 *   unavailable — treating the market as an immutable feature of economic
 *   reality rather than a contingent design choice. This reading coexists
 *   with two competing kernel readings: the beneficiary-maintained reading
 *   (which emphasizes ongoing coercive defense of market dominance despite
 *   viable alternatives) and the genuine-natural reading (which argues that
 *   market mechanisms are structurally necessary and pre-market coordination
 *   mechanisms were genuinely inferior). The three readings represent
 *   irreducible uncertainty in how to interpret the same institutional
 *   architecture.
 *
 * KEY AGENTS:
 *   - Incumbent Market Actors: Institutional beneficiaries (property owners, finance, corporate structures) — capture rents through property law and contract enforcement without requiring ongoing suppression; operate arbitrage options across jurisdictions
 *   - Foreclosed Commons Users: Powerless victims (subsistence populations, informal workers, gift-economy participants) — trapped by historical enclosure; alternatives are legally unavailable; experience diffuse extraction through pricing mechanisms
 *   - Alternative Coordination Movements: Organized victims (commons movements, cooperatives, mutual aid networks) — benefit from market coordination infrastructure while bearing extraction costs through legal liability and marginalization; constrained but organized
 *   - Property Law and Contract Frameworks: Institutional mechanisms — deliver suppression without active maintenance once embedded; operate through absence (what is not permitted) rather than presence (what is actively enforced)
 *   - Market Naturalization Doctrine: Intellectual architecture (economics pedagogy, policy discourse, business culture) — maintains naturalization narrative through performative reiteration; prevents alternative framings from becoming thinkable
 *   - Emerging Alternative Architectures: Organized agents building sunset mechanisms (digital commons platforms, cooperative platforms, legal pluralism movements) — offering escape routes that reduce dependence on market dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.58).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market Dominance as Lapsed-Closure Artifact (Historical-Contingency Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/institutional_theory/market_design").

domain_priors:requires_active_enforcement(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '685ae51c-c5ef-426f-8797-3c23a28d8e7b').
narrative_ontology:cs_kernel_codification('685ae51c-c5ef-426f-8797-3c23a28d8e7b', distributed).
narrative_ontology:cs_authority_grounding('685ae51c-c5ef-426f-8797-3c23a28d8e7b', extraction).
narrative_ontology:cs_interpretation_layer_present('685ae51c-c5ef-426f-8797-3c23a28d8e7b').
narrative_ontology:cs_reading_relation('685ae51c-c5ef-426f-8797-3c23a28d8e7b', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('685ae51c-c5ef-426f-8797-3c23a28d8e7b', market_as_natural_default__genuine_natural_reading, influences).
narrative_ontology:cs_axiom('685ae51c-c5ef-426f-8797-3c23a28d8e7b', foundational, enclosure_as_contingent_historical_choice).
narrative_ontology:cs_axiom_status(enclosure_as_contingent_historical_choice, holdable).
narrative_ontology:cs_axiom_grounding('685ae51c-c5ef-426f-8797-3c23a28d8e7b', enclosure_as_contingent_historical_choice, empirically_contingent).
narrative_ontology:cs_axiom('685ae51c-c5ef-426f-8797-3c23a28d8e7b', foundational, naturalization_narrative_as_primary_maintenance).
narrative_ontology:cs_axiom_status(naturalization_narrative_as_primary_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('685ae51c-c5ef-426f-8797-3c23a28d8e7b', naturalization_narrative_as_primary_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('685ae51c-c5ef-426f-8797-3c23a28d8e7b', active_enclosure_defense).
narrative_ontology:cs_drift_state('685ae51c-c5ef-426f-8797-3c23a28d8e7b', contemporary_naturalized_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('685ae51c-c5ef-426f-8797-3c23a28d8e7b', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, incumbent_market_actors).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, property_law_beneficiaries).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, foreclosed_alternative_coordination).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, diffuse_commons_users).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, non_monetizable_exchange_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORECLOSED COMMONS USERS (SNARE) — Trapped by historical enclosure mechanisms (property law, contract enforcement, IP regimes) that have become invisible through naturalization. Cannot exit to non-market coordination; the alternatives that existed pre-enclosure are legally unavailable. The suppression mechanism is not active enforcement but rather the absence of memory that alternatives ever existed. Maximum experienced extraction — the cost of market participation (rent, pricing, access control) is unavoidable.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE COORDINATION MOVEMENTS (TANGLED ROPE) — Organized agents (commons movements, mutual aid networks, cooperative platforms) benefit from the market's coordination infrastructure (logistics, standards, communications) while bearing extraction costs (legal liability, market pressure, narrative marginalization). The market mechanism simultaneously enables and constrains their alternatives. Coordinated but non-market exchange exists at the margins of market dominance; these actors perceive both coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT MARKET ACTORS (ROPE) — Institutional beneficiaries with arbitrage options experience the market as coordination mechanism. They capture rents through property law and contract enforcement without needing to maintain active suppression of alternatives — the naturalization narrative does the work. Operates as pure coordination from their perspective: 'markets efficiently allocate resources.'
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MARKET NATURALIZATION DOCTRINE (PITON) — The intellectual architecture ('markets are natural,' 'price signals are laws of nature,' 'efficiency requires markets') operates as theatrical performance maintaining the constraint after the active maintenance mechanisms have atrophied. Economic pedagogy, business school curriculum, and policy discourse perform the naturalization ritual. Theater ratio is high because the doctrine is largely performative — the actual enforcement of market dominance relies on property law, contract courts, and IP regimes, not on belief in market efficiency.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MARKET AS NATURAL LAW (MOUNTAIN) — The analytical observer within neoclassical economic theory views market dominance as a mathematical consequence of preference orderings, scarcity, and rational choice. From this perspective, markets are not contingent institutional arrangements but inevitable structures following from immutable axioms of human action. The analytical observer within this framework sees no alternatives — non-market coordination is analytically impossible or inherently inefficient. However, the lapsed-closure reading identifies this as a false summit: the 'naturality' depends on treating the historical enclosure (property law, contract frameworks) as given rather than contingent.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: EMERGING ALTERNATIVE ARCHITECTURES (SCAFFOLD) — Digital commons platforms, decentralized coordination mechanisms, and legal pluralism movements are building alternative enforcement layers that reduce dependence on market mechanisms. These actors perceive the market constraint as temporary and solvable through technological and legal innovation. The scaffold has a genuine sunset: as alternative coordination layers mature (blockchain governance, platform cooperativism, commons law), the market's monopoly on enforcement relaxes. The coordination function remains; the extraction mechanism loses force.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_as_natural_default__lapsed_alternative_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, TR),
    TR >= 0.70.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The lapsed-closure reading identifies extraction flowing from foreclosed alternatives rather than from active suppression. The beneficiaries (incumbent market actors) capture rents through property rights and contract enforcement. The victims bear costs through pricing mechanisms, access control, and foreclosure from non-monetizable exchange. The extractiveness is significant but not maximal — alternative coordination mechanisms do provide some value (logistics, standards, communication networks) even as they extract surplus. The trajectory shows rising extractiveness over time as market mechanisms penetrated deeper into previously non-commodified domains (labor, attention, data, relationships). Suppression (0.62): Moderate-high. Active suppression was high during the enclosure period (property-law innovation, criminal penalties for poaching, ejection from commons, contract-enforcement machinery). Contemporary suppression is lower in active enforcement burden but delivered through institutional inertia and legal structures — the framework prevents alternatives without requiring ongoing coercion. The suppression mechanism has shifted from active to structural. Theater Ratio (0.68): High and rising. Historical theater was low — enclosure was defended through explicit legal innovation and political assertion ('might makes right,' royal decree, parliamentary statute). Contemporary theater is high — the market is defended through naturalization narrative ('markets are efficient,' 'competition drives innovation,' 'prices reflect scarcity'). The rising trajectory reflects the transition from enforced to naturalized dominance. At time 100 (contemporary), the theater dominates: enforcement happens quietly (IP courts, contract litigation) while the public narrative emphasizes inevitability and efficiency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is distinguished by the maximum gap between the beneficiary's and victim's classifications. From the beneficiary's analytical position within market logic, the constraint is pure coordination (Rope): markets solve the fundamental problem of allocating resources among competing uses. From the powerless victim's position, the constraint is pure extraction (Snare): they are trapped in a system where all exchange must flow through markets, alternatives are legally unavailable, and prices are set by those with power. The gap reflects not disagreement about facts but different structural access to the constraint's mechanisms. The beneficiary experiences the efficiency gains and coordination benefits; the victim experiences only the extraction and foreclosure. The lapsed-closure reading explains the gap: the beneficiaries inherited a system built by centuries of active enclosure; they maintain it through theater (naturalization narrative) rather than through active suppression. From their position, the system appears natural and beneficial. From the victim's position, it appears engineered and extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural relationship to the constraint's extraction mechanism. Beneficiaries (incumbent market actors) with arbitrage options derive d ≈ 0.10 (low directionality toward extraction target — they are net beneficiaries). Powerless foreclosed agents derive d ≈ 0.92 (high directionality — fully targeted by extraction). Organized alternative movements derive d ≈ 0.55 (symmetric — they both benefit from and bear costs of market dominance). The piton perspective (naturalization doctrine) and mountain perspective (neoclassical theory) are observers rather than direct agents — their d values reflect their analytical position (0.72 canonical), but they are not extractors or victims. The scaffold perspective (alternative architectures) derives d ≈ 0.40 (moderate target — constrained by current dominance but with escape route). The sigmoid f(d) transforms these structural positions into experienced extractiveness: powerless agents with trapped exit experience highest chi; beneficiaries with arbitrage experience lowest chi; organized agents experience moderate chi reflecting their dual position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the kernel reading frame. The three competing kernel readings (lapsed-closure, beneficiary-maintained, genuine-natural) produce different classifications from the same base structure. The lapsed-closure reading (this file) produces Tangled Rope: a hybrid of coordination function (markets do coordinate complex exchange) and asymmetric extraction (foreclosed alternatives, diffuse harm, rent capture). The beneficiary-maintained reading would produce Snare: emphasizing active suppression of viable alternatives despite their superiority. The genuine-natural reading would produce Mountain or Rope: emphasizing that market mechanisms are structurally necessary or naturally emerging from preference orderings. No single reading is definitively correct — they represent irreducible interpretation gaps in how to understand the kernel (market-as-natural-default). The lapsed-closure reading resolves mandatrophy by identifying where each alternative reading stands: the beneficiary-maintained reading forecloses the genuine-natural reading (if alternatives are being actively suppressed, they cannot be naturally inferior). The lapsed-closure reading coexists with both (depends on whether the historical enclosure remains actively defended or has atrophied into theater). The mandatrophy reflects deep indeterminacy about institutional causation: did enclosure succeed because market mechanisms are inherently superior, or did market superiority claims arise because enclosure was successful?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_versus_active_suppression,
    'Is market dominance maintained primarily through active suppression of alternatives (ongoing enforcement burden) or through successful naturalization that makes alternatives cognitively unavailable (zero maintenance cost)?',
    'Historical comparison of enforcement expenditure (law enforcement, contract litigation, IP enforcement) vs. time investment in naturalization doctrine. Counterfactual analysis: what would happen if enforcement apparatus were withdrawn but naturalization narrative remained intact?',
    'If maintained through active suppression: market constraint is Snare (requires ongoing coercive work). If naturalization is sufficient: market constraint is Piton (maintenance through theater). If both: Tangled Rope with mixed mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_versus_active_suppression, conceptual, 'Whether market dominance depends on active enforcement or naturalization narrative').

omega_variable(
    enclosure_as_contingent_d3_artifact,
    'Are the property law and contract frameworks that enable market dominance contingent historical constructions (enclosure movement, legal innovations, political choice) or structural necessities inherent to coordinating complex exchange?',
    'Comparative institutional analysis: documentation of pre-enclosure coordination mechanisms and their functionality (commons management, mutual aid, gift economies, barter networks). Evidence of legal innovation moments where enclosure mechanisms were actively installed rather than discovered.',
    'If contingent: market dominance is an artifact of specific historical D3 choices that could be reversed. If structural: market dominance emerges necessarily from complexity. This determines whether the lapsed-closure reading (contingency) or the genuine-natural reading (necessity) is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enclosure_as_contingent_d3_artifact, empirical, 'Whether enclosure mechanisms are historical contingencies or structural necessities').

omega_variable(
    beneficiary_awareness_of_naturalization_strategy,
    'Do incumbent market beneficiaries actively maintain the naturalization narrative, or has the narrative become so embedded in institutions that benefits accumulate passively?',
    'Documentary evidence of coordinated narrative maintenance (business lobbying, think tank funding, textbook authorship influence). Comparison with historical periods where the naturalization was more actively constructed and defended. Interviews with policy architects about intentionality of market-dominance framing.',
    'If actively maintained: beneficiaries are strategically invested in the false-natural framing (Snare mechanics). If passively accumulated: the benefit structure persists through institutional inertia (Piton mechanics). This distinction affects whether changing the narrative would shift material incentives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_awareness_of_naturalization_strategy, empirical, 'Whether naturalization is actively maintained or institutionally embedded').

omega_variable(
    scale_dependent_alternatives_viability,
    'Are non-market coordination mechanisms viable at different scales than markets, or are they genuinely inferior at all scales of organization?',
    'Comparative performance analysis: commons governance at village/regional scale, mutual aid networks during crisis periods, platform cooperatives vs. extractive platforms, open-source software production. Measurement of transaction costs, innovation rates, equity of distribution across different coordination mechanisms at matched scales.',
    'If scale-dependent: alternatives can coexist with markets in different niches (Rope from their perspective, not Snare). If globally inferior: market dominance reflects genuine efficiency (genuine-natural reading). If superiority is context-dependent: lapsed-closure reading is correct (contingency of current dominance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_dependent_alternatives_viability, empirical, 'Whether non-market coordination is viable at different scales').

omega_variable(
    reading_determination_kernel,
    'Which kernel reading applies: lapsed-closure artifact (this reading), beneficiary-maintained suppression (beneficiary_maintained_reading), or genuine structural necessity (genuine_natural_reading)?',
    'Falsification tests specific to each reading. Lapsed-closure: demonstrate enclosure history + naturalization narrative + reduced enforcement burden over time. Beneficiary-maintained: show active suppression of documented alternatives despite continued viability. Genuine-natural: show that all pre-market coordination mechanisms had fatal flaws that market mechanisms solve.',
    'The reading determines classification type (this reading: Tangled Rope via lapsed-closure). Different reading: different ε and different victim set. Core structural question about whether market dominance is contingent or necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_determination_kernel, conceptual, 'Which kernel reading of market-as-natural-default applies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mktlap_theater_t0_active_enforcement, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mktlap_theater_t50_transition, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(mktlap_theater_t100_naturalization_dominance, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(mktlap_extractiveness_t0_enclosure_period, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mktlap_extractiveness_t50_industrial_period, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(mktlap_extractiveness_t100_contemporary, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mktlap_suppression_t0_active_enclosure, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(mktlap_suppression_t50_normalized, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(mktlap_suppression_t100_narrative_maintenance, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__genuine_natural_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, enclosure_movement_legal_contingency).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, commons_foreclosure_mechanism).

% DUAL FORMULATION NOTE:
% The constraint family around market-as-natural-default contains three structurally distinct kernel readings (lapsed-closure, beneficiary-maintained, genuine-natural) that cannot be fully resolved into a single constraint. Each reading is a separate constraint story with different ε values and different victim/beneficiary structures. They share the same base institutional phenomenon but interpret its causation and persistence differently. The lapsed-closure reading (this file) emphasizes historical contingency and successful naturalization; network links to the other readings preserve the family structure while maintaining analytic separation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
