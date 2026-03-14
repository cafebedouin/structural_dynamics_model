% ============================================================================
% CONSTRAINT STORY: cabmen_guild_economic_decline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cabmen_guild_economic_decline, []).

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
 *   constraint_id: cabmen_guild_economic_decline
 *   human_readable: Cabmen Guild Economic Decline and Regulatory Lock-In
 *   domain: labor_economics/transportation
 *
 * SUMMARY:
 *   The cabmen's guild medallion licensing system represents a classic case
 *   of regulatory capture evolved into institutional inertia. Historically,
 *   the medallion system coordinated taxi service provision, preventing
 *   destructive price wars and ensuring service coverage. Over decades, it
 *   transformed into a rent-extraction mechanism: medallion owners benefited
 *   from artificial scarcity while incumbent drivers experienced both
 *   collective bargaining protection and asymmetric extraction through
 *   medallion costs and regulatory rents. The system's economic decline
 *   reflects the rise of rideshare platforms that bypass medallion regulation
 *   entirely, creating an alternative coordination mechanism with lower entry
 *   barriers but different risk distributions. The constraint exhibits all
 *   structural features of a tangled rope: genuine coordination value
 *   (service dispatch), asymmetric extraction (medallion rents), active
 *   enforcement (municipal licensing), and increasing theater as the system's
 *   functionality decays and incumbents invest more effort in maintaining the
 *   appearance of legitimacy. The rising theater ratio (0.40 → 0.65 over the
 *   interval) reflects the transition from a coordination mechanism defending
 *   incumbents' interests to a performative institutional shell maintained
 *   through regulatory capture.
 *
 * KEY AGENTS:
 *   - Prospective Cab Drivers: Primary victims (powerless/trapped) — face $100K-$300K medallion barriers; no alternative high-income pathway; structurally excluded from entry
 *   - Incumbent Cab Drivers: Primary beneficiaries (moderate/constrained) — benefit from collective bargaining and service coordination; also bear extraction through medallion value ownership, shift costs, and declining market share as rideshare grows
 *   - Municipal Regulators: Beneficiaries (institutional/arbitrage) — capture licensing fees and tax revenue; maintain coordination role; can exit through deregulation
 *   - Rideshare Platforms: Disruptors (organized/constrained) — building alternative dispatch coordination with lower entry barriers; sunset mechanism for medallion system
 *   - Guild Institution: Institutional actor (institutional/arbitrage) — maintains licensing, training, union structure; increasingly performative as economic base declines
 *   - Riders and Labor Market: Victims and consumers (moderate/mobile) — pay medallion scarcity premiums; experience constrained labor market entry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cabmen_guild_economic_decline, 0.58).
domain_priors:suppression_score(cabmen_guild_economic_decline, 0.68).
domain_priors:theater_ratio(cabmen_guild_economic_decline, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cabmen_guild_economic_decline, extractiveness, 0.58).
narrative_ontology:constraint_metric(cabmen_guild_economic_decline, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cabmen_guild_economic_decline, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cabmen_guild_economic_decline, tangled_rope).
narrative_ontology:human_readable(cabmen_guild_economic_decline, "Cabmen Guild Economic Decline and Regulatory Lock-In").
narrative_ontology:topic_domain(cabmen_guild_economic_decline, "labor_economics/transportation").

domain_priors:requires_active_enforcement(cabmen_guild_economic_decline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cabmen_guild_economic_decline, licensed_cab_operators).
narrative_ontology:constraint_beneficiary(cabmen_guild_economic_decline, municipal_regulators).
narrative_ontology:constraint_victim(cabmen_guild_economic_decline, non_licensed_drivers).
narrative_ontology:constraint_victim(cabmen_guild_economic_decline, rider_affordability).
narrative_ontology:constraint_victim(cabmen_guild_economic_decline, market_entry_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSPECTIVE CAB DRIVER (SNARE) — Faces insurmountable barriers to entry: municipal medallion systems create artificial scarcity with costs ($100K-$300K in major cities) that lock out workers without existing capital. No alternative transportation employment pathway exists with similar income potential for workers with limited formal credentials. Trapped by cost and regulatory prohibition — extraction is near-total.
constraint_indexing:constraint_classification(cabmen_guild_economic_decline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INCUMBENT CAB DRIVER (TANGLED ROPE) — Experiences both coordination benefit (guild protects against predatory dispatch and wages through collective bargaining) and asymmetric extraction (guild leadership captures rents, medallion value extraction constrains actual wage growth, shift costs push to driver). Exit is possible (switch to rideshare, trucking, delivery) but carries high social cost (decades of taxi-specific capital become worthless). Moderate power through organization, but constrained by identity lock to taxi work.
constraint_indexing:constraint_classification(cabmen_guild_economic_decline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MUNICIPAL REGULATOR (ROPE) — Benefits from medallion system coordination: stable tax base, predictable service standards, administrative simplicity. Experiences the constraint as solving the coordination problem of preventing taxi wars and ensuring service coverage. Can exit entirely (deregulate) or maintain arbitrage (regulatory licensing fee extraction). Low experienced extraction — sees the system as functional coordination.
constraint_indexing:constraint_classification(cabmen_guild_economic_decline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: RIDESHARE DISRUPTORS (SCAFFOLD) — See the medallion system as a temporary, doomed coordination mechanism being replaced by platform-based dispatch and dynamic pricing. The constraint has a sunset: regulatory capture by incumbents delays but cannot prevent the eventual shift to app-based models with driver-platform arbitrage replacing medallion-guild models. Organized agents (rideshare platforms, labor activists) are explicitly building alternative pathways with lower entry cost and decentralized dispatch. Suppression is declining as the alternative system matures.
constraint_indexing:constraint_classification(cabmen_guild_economic_decline, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE GUILD INSTITUTION (PITON) — The formal guild structure persists through institutional inertia despite declining functional purpose. Historic guild functions (wage negotiation, training standards, apprenticeship pathways) have atrophied. What remains is theater: licensing exams that don't correlate with driver quality, union meetings that don't influence dispatch economics, apprenticeship requirements that are nominally honored but functionally irrelevant. Theater ratio is high (0.65) — much guild activity is performative maintenance of legitimacy rather than coordination function. The institution persists because no one has formally disbanded it, not because it delivers coordination benefits.
constraint_indexing:constraint_classification(cabmen_guild_economic_decline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSUMER AND LABOR MARKET (TANGLED ROPE) — The medallion system coordinates service availability while extracting supernormal rents through artificial scarcity. Riders pay higher fares than competitive markets would produce; workers are excluded from entry. The system provides genuine coordination value (predictable service availability, dispatch efficiency) alongside significant extraction (medallion scarcity premium, suppressed wage competition). From this perspective, the system is a genuine hybrid — neither pure extraction nor pure coordination, but a durable entanglement of both.
constraint_indexing:constraint_classification(cabmen_guild_economic_decline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COMPETITIVE MARKET VIEW (MOUNTAIN) — From an analytical economics lens, perfect competition requires free entry and exit. The medallion system is a structural prohibition on entry — mathematically, this produces supernormal rents that cannot be competed away. This perspective sees the constraint as immutable under market logic: absent regulatory intervention, scarcity-based licensing will persist indefinitely because it is profitable for holders. However, empirical data contradicts the mountain classification — rideshare disruption is actively dismantling the medallion system, revealing it as a contingent institutional arrangement protected by regulatory capture, not an immutable market law.
constraint_indexing:constraint_classification(cabmen_guild_economic_decline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cabmen_guild_economic_decline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cabmen_guild_economic_decline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cabmen_guild_economic_decline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cabmen_guild_economic_decline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cabmen_guild_economic_decline, TR),
    TR >= 0.70.

:- end_tests(cabmen_guild_economic_decline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The medallion system creates artificial scarcity that generates supernormal rents extracted from entrants and riders. Entry barriers ($100K+) are primarily regulatory rather than coordination cost. Incumbent drivers benefit from service coordination but bear costs through medallion value ownership and declining wages as rideshare competition grows. The system's extractiveness has increased over the interval (0.45 → 0.58) as medallion values have climbed despite declining ridership — a classic sign of pure rent extraction masquerading as coordination. Suppression (0.68): High. Regulatory prohibition on entry without a medallion, combined with high cost, creates near-total barriers for prospective drivers. Alternative occupations (rideshare) are technically available but carry identity costs for taxi-identified workers. Incumbent drivers face suppression through locked-in medallion investment. Theater ratio (0.65): Moderately high and rising. Guild licensing exams, union meetings, and apprenticeship requirements persist despite declining functional role. The system's coordination function (dispatch, service standards) is increasingly performative — rideshare platforms provide equivalent or superior coordination with fewer formal structures. The rising theater ratio reflects institutional inertia: as the medallion system loses market share, incumbents invest more effort in maintaining regulatory legitimacy rather than improving actual service.
 *
 * PERSPECTIVAL GAP:
 *   The most significant gap exists between the prospective driver (snare — sees maximal, unchangeable extraction) and the municipal regulator (rope — sees functional coordination). This gap reveals the constraint's hybrid nature: what functions as coordination for the regulator (stable service provision, tax base) simultaneously functions as pure extraction for the prospective driver (prohibited entry, supernormal rents). The incumbent driver's perception (tangled rope) occupies the middle ground, seeing both coordination benefits and asymmetric extraction. The guild institution's piton classification reveals institutional inertia: the organization maintains formal legitimacy through theater (licensing, union structure) while its actual coordination function is captured by rideshare platforms. The analytical observer's mountain classification is a false summit: treating competitive entry barriers as immutable laws of markets naturalizes what is actually regulatory capture. The rideshare scaffold perspective reveals the constraint's structural vulnerability: if regulatory barriers fall, the entire extraction mechanism collapses because the coordination function is portable to alternative platforms.
 *
 * DIRECTIONALITY LOGIC:
 *   The critical directionality insight: the same regulatory mechanism (medallion licensing) produces opposite directionality values for different agents. Prospective drivers have d ≈ 0.95 (pure targets of extraction). Incumbent drivers have d ≈ 0.72 (identity-locked beneficiaries experiencing identity lock + constrained exit, derived from declaring them as beneficiaries of coordination but victims of extraction). Municipal regulators have d ≈ 0.15 (net beneficiaries — extraction runs toward them). The regulatory mechanism's directionality is observer-relative: it extracts from entrants and riders while benefiting incumbents and regulators. This is precisely the structure of a tangled rope: genuine coordination value (dispatch, service standards) coexists with asymmetric extraction (rents, entry barriers, suppressed competition). The incumbent driver's identity lock is key: decades of medallion-specific capital (hack license knowledge, dispatch relationships, taxi industry identity) makes exit psychologically difficult despite improving economic alternatives (rideshare platforms often offer superior earnings and flexibility). The identity lock derives from structural mobility (drivers could join rideshare) but identity frame makes exit unthinkable (taxi work is 'who they are,' not just 'what they do').
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE MANDATROPHY RESOLVED: The constraint exhibits genuine tangled rope structure. Coordination function: the medallion system (historically and currently) solves the multi-agent coordination problem of dispatch, service standards, and preventing destructive price wars. This coordination value is real and appreciated by both regulators and incumbent drivers. Asymmetric extraction: the system simultaneously creates supernormal rents through artificial scarcity, extracting from prospective entrants ($100K+ barrier) and riders (higher fares than competitive markets). Enforcement: the system requires active municipal enforcement of licensing restrictions, regulatory capture by incumbent interests, and regulatory gatekeeping. The key mandatrophy resolution: the constraint cannot be classified as either pure rope (coordination-only) or pure snare (extraction-only) because both functions are genuinely present and both are structural to the system. The rising theater ratio and measured extractiveness suggest the system is drifting toward pure snare as rideshare competition erodes the coordination function — what was previously a necessary scarcity mechanism becomes pure rent-seeking. However, the constraint remains a true tangled rope in its current form because the coordination mechanism is still functioning (dispatch works, service standards are enforced) even as extraction increases. The scaffold perspective (rideshare sunset) and piton perspective (increasing theater) are not contradictions but complementary observations: the constraint is a tangled rope becoming a piton (functionality degraded to theater) while simultaneously being actively replaced by a scaffold (rideshare as a competing coordination mechanism with a sunset for the medallion system).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_durability,
    'How durable is the regulatory capture protecting the medallion system against rideshare competition?',
    'Historical tracking of regulatory exemptions granted to rideshare platforms; analysis of lobbying expenditure trajectories; comparison of medallion values and driver earnings over time as rideshare penetration increases',
    'If capture is durable: the piton classification is correct — the system persists through inertia indefinitely, theater increases. If capture is fragile: the scaffold perspective is correct — rideshare represents a real sunset mechanism, suppression declines, system transitions within 15-20 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_durability, empirical, 'Durability of regulatory capture protecting medallion system').

omega_variable(
    medallion_holder_identity_lock,
    'To what extent is the incumbent cab driver''s commitment to the medallion system driven by identity fusion (decades invested, occupational identity) versus rational economic calculation?',
    'Post-exit trajectory analysis: do drivers who leave medallion systems to join rideshare platforms experience satisfaction/earnings improvements? Do those who stay in medallion systems view remaining as choice or necessity? Identity-lock prediction: drivers with high years-of-service show higher resistance to platform switching despite earnings advantage.',
    'If high identity lock: suppression is partially internalized — drivers carry the constraint with them after medallion system collapse, limiting labor market adaptability. If low identity lock: barriers are primarily structural-economic; policy changes rapidly unlock labor mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medallion_holder_identity_lock, empirical, 'Identity fusion in incumbent cab driver commitment to medallion').

omega_variable(
    alternative_dispatch_coordination_sufficiency,
    'Do rideshare platforms provide equivalent coordination and service reliability compared to medallion-based taxi dispatch?',
    'Empirical comparison of service metrics: wait times, pickup reliability, coverage of underserved areas (financial district vs residential suburbs), surge pricing dynamics, driver scheduling stability. Historical case studies from deregulated cities.',
    'If rideshare is equivalent: the medallion system is pure extraction with theater — rope classification does not hold for regulator. If rideshare is inferior for some use cases: medallion system coordination value is real, tangled rope classifications are stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_dispatch_coordination_sufficiency, empirical, 'Service quality equivalence between rideshare and medallion-based dispatch').

omega_variable(
    entry_cost_barrier_mechanism,
    'Is the medallion entry cost primarily a regulatory artificial scarcity or a genuine coordination cost (service standardization, insurance, infrastructure)?',
    'Decomposition analysis: what portion of medallion cost is regulatory scarcity premium vs. genuine coordination cost (insurance, dispatch infrastructure, training). Compare medallion costs to rideshare onboarding costs across jurisdictions.',
    'If primarily artificial scarcity: snare classification is correct — extraction is maximal for prospective drivers. If partially genuine coordination cost: prospective driver experiences moderate extraction, and lower entry cost under rideshare does not imply equivalent service quality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entry_cost_barrier_mechanism, empirical, 'Medallion cost as artificial scarcity versus coordination cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cabmen_guild_economic_decline, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cabmen_tr_t0, cabmen_guild_economic_decline, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cabmen_tr_t5, cabmen_guild_economic_decline, theater_ratio, 5, 0.52).
narrative_ontology:measurement(cabmen_tr_t10, cabmen_guild_economic_decline, theater_ratio, 10, 0.65).
narrative_ontology:measurement(cabmen_tr_t15, cabmen_guild_economic_decline, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(cabmen_be_t0, cabmen_guild_economic_decline, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cabmen_be_t5, cabmen_guild_economic_decline, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cabmen_be_t10, cabmen_guild_economic_decline, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cabmen_be_t15, cabmen_guild_economic_decline, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cabmen_guild_economic_decline, resource_allocation).
narrative_ontology:boltzmann_floor_override(cabmen_guild_economic_decline, 0.18).
narrative_ontology:affects_constraint(cabmen_guild_economic_decline, rideshare_labor_precarization).
narrative_ontology:affects_constraint(cabmen_guild_economic_decline, urban_transportation_regulation).

% DUAL FORMULATION NOTE:
% The medallion system decomposes into two structurally distinct constraints: (1) medallion_coordination_mechanism (ε≈0.15, Rope) — the dispatch and service standard coordination function; and (2) medallion_entry_barrier (ε≈0.78, Snare) — the artificial scarcity and rent extraction mechanism. These are presented as a unified tangled_rope story because they operate through the same institutional mechanism (municipal licensing), but they have different ε values and different failure modes. Rideshare platforms are attacking the entry_barrier component while partially preserving coordination through alternative dispatch mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cabmen_guild_economic_decline, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
