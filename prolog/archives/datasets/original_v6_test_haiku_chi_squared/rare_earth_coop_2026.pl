% ============================================================================
% CONSTRAINT STORY: rare_earth_coop_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_coop_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rare_earth_coop_2026
 *   human_readable: Manufacturer-Owned Rare Earth Cooperative (MOREC)
 *   domain: economic/industrial
 *
 * SUMMARY:
 *   MOREC represents a manufacturer-coordinated response to the
 *   high-extraction rare earth supply regime imposed by Project Vault. Rather
 *   than accepting Vault's profit-seeking markup and allocation restrictions,
 *   participating manufacturers pool capital to establish independent supply
 *   chains, stockpiling capacity, and processing infrastructure. This
 *   constraint exhibits core characteristics of pure coordination (Rope): the
 *   mechanism solves a genuine collective action problem (manufacturers
 *   cannot individually secure diverse rare earth supply; coordinated pooling
 *   reduces carry costs and enables price negotiation), exhibits low
 *   suppression (members can exit, though at cost to their individual supply
 *   security), and requires no external enforcement (the cooperative's
 *   governance structure is self-policing through membership interests).
 *   However, the constraint also exhibits exclusionary characteristics that
 *   create victims: manufacturers without sufficient capital or political
 *   standing are locked out of the cooperative, forcing them to source
 *   through Project Vault at higher cost. This creates a perspectival gap:
 *   members see coordination; excluded manufacturers see extraction.
 *
 * KEY AGENTS:
 *   - Participating Manufacturers: Primary beneficiaries (powerful/mobile) — pool capital to reduce supply-chain risk and pricing pressure; enjoy collective bargaining power
 *   - Downstream Electronics Industry: Secondary beneficiary (organized/constrained) — benefits from stable, competitively-priced rare earth supply through cooperative membership; supply chain integration constrains exit but MOREC provides group security
 *   - Non-Member Smaller Manufacturers: Primary victims (moderate/trapped) — excluded from cooperative by capital requirements; forced to source at higher Vault-imposed costs; cannot individually negotiate better terms
 *   - State Supply Authority: Institutional actor (institutional/constrained) — benefits from supply chain resilience (coordination function); loses direct control over compliance mandates and stockpile requirements (extraction asymmetry); experiences regulatory arbitrage as members optimize to cooperative structure rather than state directive
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees MOREC as horizontal coordination mechanism that reduces dependency on profit-seeking intermediary; low extractiveness reflects genuine coordination benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_coop_2026, 0.28).
domain_priors:suppression_score(rare_earth_coop_2026, 0.32).
domain_priors:theater_ratio(rare_earth_coop_2026, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_coop_2026, extractiveness, 0.28).
narrative_ontology:constraint_metric(rare_earth_coop_2026, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(rare_earth_coop_2026, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_coop_2026, rope).
narrative_ontology:human_readable(rare_earth_coop_2026, "Manufacturer-Owned Rare Earth Cooperative (MOREC)").
narrative_ontology:topic_domain(rare_earth_coop_2026, "economic/industrial").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_coop_2026, participating_manufacturers).
narrative_ontology:constraint_beneficiary(rare_earth_coop_2026, downstream_electronics_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARTICIPATING MANUFACTURER (ROPE) — Sees MOREC as pure coordination mechanism. Solves collective action problem of supply security without intermediary extraction. Joint capital pooling reduces individual carry costs and creates pricing transparency. d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.20. Low effective extraction; mechanism directly serves member interests.
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM ELECTRONICS INDUSTRY (ROPE) — Benefits from cooperative supply stability and competitive pricing that bypasses Project Vault's profit-seeking markup. Exit constrained by supply chain integration, but MOREC membership provides collective security. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.19. Coordination benefit outweighs extraction cost.
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-MEMBER SMALLER MANUFACTURER (SNARE) — Excluded from cooperative due to capital requirements or political barriers. Faces higher rare earth costs and supply uncertainty without membership. Cannot independently access alternative supply chains. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.34. Trapped in disadvantaged position; extraction is exclusionary mechanism.
constraint_indexing:constraint_classification(rare_earth_coop_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SUPPLY AUTHORITY (TANGLED ROPE) — Sees MOREC as both coordinating critical supply chains (state benefit) AND potentially avoiding state-mandated stockpile requirements (state loss). Coordination function genuine (manufacturers pool capital for resilience); extraction asymmetry exists (cooperative members avoid certain regulations that isolated manufacturers face). d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.34. Requires active enforcement of membership terms to prevent regulatory arbitrage.
constraint_indexing:constraint_classification(rare_earth_coop_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From civilizational/universal perspective, MOREC is a peer-coordinated supply management mechanism solving a genuine collective action problem without external coercion. The cooperative structure exhibits low suppression (members can exit, though at cost), low theater (capital pooling and supply management are functional, not performative), and moderate extractiveness (extraction is limited to exclusionary membership barriers). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.22. Engine classification: Rope.
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_coop_2026_tests).
:- end_tests(rare_earth_coop_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The base extraction reflects two countervailing factors: (1) Coordination benefit: manufacturers genuinely solve a collective action problem through capital pooling and supply diversification; extraction is limited to the exclusionary membership mechanism (non-members pay higher costs); (2) Trajectory analysis: extractiveness rose from 0.15 to 0.28 over 6 years, indicating early-stage consolidation — as membership stabilizes and alternative supply paths close, exclusionary extraction increases. Current 0.28 reflects the coordinate mechanism phase before lock-in dynamics dominate. Suppression (0.32): Moderate. Suppression mechanisms include capital threshold barriers (prevents entry), information asymmetry (cooperative members have supply data non-members lack), and supply-chain integration lock-in (members become dependent on cooperative infrastructure for cost parity). But suppression is not coercive — non-members can attempt to form competing cooperatives or negotiate directly with suppliers; they simply face higher friction. Theater ratio (0.41): Low-moderate. Capital pooling and supply management are functional activities; the cooperative's governance focuses on real supply-chain operations rather than performative ritual. Theater has slightly increased (0.35→0.41) as cooperative governance incorporates more stakeholder reporting and regulatory compliance theater, but the primary function remains supply coordination rather than symbolic legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between member and non-member is the dominant structural feature of this constraint. Members classify MOREC as Rope (coordination solving a real problem). Non-members classify it as Snare (exclusionary mechanism extracting through access denial). The state supply authority sees Tangled Rope: the cooperative coordinates supply resilience (state benefit) but enables regulatory arbitrage (state loss). The analytical observer sees Rope because the constraint's primary function is genuinely coordinating supply chains — the exclusionary extraction is secondary to the coordination purpose. The gap is not about measurement or framing; it is about structural position: your extraction experience depends on whether you can pay the membership capital threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   Participating manufacturers: Beneficiary + mobile → d≈0.48, f(d)≈0.60. Net beneficiary experiencing low effective extraction. Downstream electronics: Beneficiary + constrained → d≈0.52, f(d)≈0.68. Also net beneficiary; constrained exit reflects supply chain integration but MOREC provides group security reducing individual exit friction. Non-member smaller manufacturers: Victim + trapped → d≈0.85, f(d)≈1.15. Experience high effective extraction through exclusionary mechanism. State supply authority: Partially victim (regulatory arbitrage) + constrained → d≈0.65, f(d)≈1.00. Institutional relationship is mixed: state benefits from supply resilience coordination but loses regulatory leverage; extractive mechanism (circumventing mandates) operates at institutional level. Analytical observer: analytical → d≈0.50, f(d)≈0.65. Sees coordination as primary function; extraction as secondary effect of membership structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MOREC does not trigger mandatrophy because it is primarily a coordination mechanism, not a mislabeled extraction system. The constraint's low extractiveness (0.28) and low suppression (0.32) confirm that the primary function is coordinating supply security, not extracting rents. The exclusionary membership barrier creates real extraction for non-members (Snare perspective), but this is secondary to the coordination function and is resolved through different perspectives rather than through false natural law claims. The state supply authority's Tangled Rope perspective correctly identifies the hybrid nature: genuine coordination (supply resilience) + asymmetric extraction (regulatory arbitrage). No Mandatrophy pattern is triggered because the classification schema (Rope-with-Snare-secondary) accurately reflects the actual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    membership_capital_threshold_inclusivity,
    'Is the cooperative capital requirement designed for supply resilience or as a disguised cartel gate to exclude competitors?',
    'Comparative analysis of capital requirements vs actual supply chain management costs; historical correlation between membership thresholds and excluded firm size distribution; survey of excluded firms on stated reasons for non-participation',
    'If threshold is resilience-driven: MOREC is Rope from all perspectives. If threshold is cartel-driven: classification shifts to Snare with exclusionary extraction; χ increases substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_capital_threshold_inclusivity, empirical, 'Whether capital requirement serves supply resilience or cartel gatekeeping').

omega_variable(
    regulatory_arbitrage_intent,
    'Does MOREC''s structure intentionally avoid state stockpile mandates, environmental compliance costs, or labor standards that apply to independent sourcing?',
    'Comparison of compliance obligations for MOREC members vs non-members; analysis of regulatory filing patterns; interviews with state supply authorities on enforcement challenges',
    'If intentional arbitrage: requires_active_enforcement flag should be true; Tangled Rope from state perspective is primary; extraction increases. If unintentional: Rope classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_intent, empirical, 'Whether cooperative structure enables regulatory arbitrage').

omega_variable(
    alternative_supply_pathways,
    'Do open-market rare earth suppliers continue to offer viable alternatives to MOREC members, or does cooperative membership consolidate supply access in ways that raise switching costs?',
    'Market analysis of independent rare earth suppliers; tracking of price differentials and lead times for MOREC members vs non-members; historical data on member exit rates',
    'If alternatives exist: exit option is mobile, suppression stays low, Rope is stable. If MOREC creates lock-in: exit becomes constrained; suppression rises; classification drifts toward Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_pathways, empirical, 'Whether independent rare earth suppliers remain viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_coop_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(morec_tr_t0, rare_earth_coop_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(morec_tr_t3, rare_earth_coop_2026, theater_ratio, 3, 0.38).
narrative_ontology:measurement(morec_tr_t6, rare_earth_coop_2026, theater_ratio, 6, 0.41).

% Extraction over time
narrative_ontology:measurement(morec_be_t0, rare_earth_coop_2026, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(morec_be_t3, rare_earth_coop_2026, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(morec_be_t6, rare_earth_coop_2026, base_extractiveness, 6, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_coop_2026, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_coop_2026, project_vault_supply_monopoly).
narrative_ontology:affects_constraint(rare_earth_coop_2026, semiconductor_supply_security).
narrative_ontology:affects_constraint(rare_earth_coop_2026, rare_earth_geopolitical_dependency).

% DUAL FORMULATION NOTE:
% MOREC is downstream of Project Vault's high-extraction monopoly regime and affects semiconductor supply security and geopolitical rare earth dynamics. The cooperative emerges as a coordination response to Vault's extraction; understanding MOREC requires analyzing it alongside the upstream constraint it is designed to bypass.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
