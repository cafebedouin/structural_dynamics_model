% ============================================================================
% CONSTRAINT STORY: project_vault_extraction_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_project_vault_extraction_2026, []).

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
 *   constraint_id: project_vault_extraction_2026
 *   human_readable: Project Vault: Debt-Financed Strategic Extraction
 *   domain: economic/political
 *
 * SUMMARY:
 *   Project Vault is a $12 billion strategic minerals reserve funded by a $10
 *   billion EXIM Bank loan with an explicit profit mandate and debt
 *   acceleration clauses. The constraint combines legitimate supply
 *   coordination for the energy transition with structural extraction
 *   mechanisms that lock in asymmetric value capture. Surrounding
 *   communities, domestic competing producers, and future public budgets are
 *   trapped in extraction pathways from which they cannot exit. EXIM Bank and
 *   the Vault management consortium operate with arbitrage exits and mobile
 *   capital. The host-country government maintains nominal sovereignty but
 *   operates under constraints from loan covenants that functionally override
 *   public authority. The analytical observer faces a false summit risk:
 *   framing Vault as a necessary institutional response to supply scarcity
 *   naturalizes what is actually a contingent debt-financed extraction
 *   structure. The constraint exhibits multiple DR types from different
 *   perspectives: pure snare for powerless and moderately-powered victims;
 *   mixed tangled rope for sourcing nations with partial coordination
 *   benefits; temporary scaffold for climate advocates betting on energy
 *   transition sunset; performative piton for host-country government; and
 *   high-extraction snare for the creditor syndicate with arbitrage exits.
 *
 * KEY AGENTS:
 *   - EXIM Bank and Creditor Syndicate: Primary extractors (institutional/arbitrage) — loan originators with profit mandate and arbitrage exit via securitization
 *   - Vault Management Consortium: Organized beneficiary (organized/mobile) — operations management with contractual extraction protection and capital mobility
 *   - Surrounding Communities: Primary victims (powerless/trapped) — land-users facing irreversible environmental externalities and geographic immobility
 *   - Domestic Competing Producers: Secondary victims (moderate/constrained) — undercut by subsidized debt and market power; capital-locked and exit-barred
 *   - Host-Country Government: Nominal authority (institutional/constrained) — maintains symbolic control; actual power overridden by loan covenants and profit mandate
 *   - Public Budget / Future Governments: Long-term victims (moderate/trapped) — debt servicing obligations and extraction lock-in inherited by successors
 *   - Sourcing Nations and Downstream Industries: Mixed (powerful/mobile) — benefit from supply security; extraction via price asymmetry and lock-in
 *   - Climate/Energy Transition Advocates: Temporary beneficiaries (organized/mobile) — benefit from supply security; expect sunset as battery recycling scales
 *   - Analytical Observer: Risk of false summit (analytical/analytical) — naturalization of contingent extraction as immutable necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(project_vault_extraction_2026, 0.68).
domain_priors:suppression_score(project_vault_extraction_2026, 0.72).
domain_priors:theater_ratio(project_vault_extraction_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(project_vault_extraction_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(project_vault_extraction_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(project_vault_extraction_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(project_vault_extraction_2026, snare).
narrative_ontology:human_readable(project_vault_extraction_2026, "Project Vault: Debt-Financed Strategic Extraction").
narrative_ontology:topic_domain(project_vault_extraction_2026, "economic/political").

domain_priors:requires_active_enforcement(project_vault_extraction_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, exim_bank_creditors).
narrative_ontology:constraint_beneficiary(project_vault_extraction_2026, vault_management_consortium).
narrative_ontology:constraint_victim(project_vault_extraction_2026, surrounding_communities).
narrative_ontology:constraint_victim(project_vault_extraction_2026, future_public_resources).
narrative_ontology:constraint_victim(project_vault_extraction_2026, competing_domestic_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURROUNDING COMMUNITIES (SNARE) — Trapped by land rights limitations and geographic immobility. Environmental externalities (water depletion, tailings contamination, habitat loss) are irreversible. Communities cannot exit extraction zone without severing livelihood ties. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.89.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DOMESTIC COMPETING PRODUCERS (SNARE) — Constrained by capital requirements and market access barriers. Vault's subsidized debt ($10B EXIM loan at favorable rates) and profit mandate undercut market prices. Competitors cannot exit without writing down assets or relocating operations at prohibitive cost. d≈0.80, f(d)≈1.20, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC BUDGET / FUTURE GOVERNMENTS (SNARE) — Trapped by debt servicing obligations and lock-in to extraction model. The $10B loan mandates profit extraction at fixed rates regardless of commodity prices or environmental costs. Renegotiation or early exit incurs severe penalties. Future governments inherit extraction path and cannot redirect capital without default. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: EXIM BANK / CREDITOR SYNDICATE (SNARE) — Primary extractors. Arbitrage exit: loan can be securitized, sold to pension funds, or refinanced. Profit mandate ensures extraction stream prioritized over environmental or social objectives. Debt servicing claims supersede all other obligations. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.55. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VAULT MANAGEMENT CONSORTIUM (SNARE) — Organized beneficiary with mobile capital. Consortium can reallocate operations, shift to alternative commodities, or exit to new jurisdictions. Extraction is contractually protected; operational flexibility is high. d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.04. Low effective extraction; primary beneficiary position.
constraint_indexing:constraint_classification(project_vault_extraction_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: SOURCING NATIONS / DOWNSTREAM INDUSTRIES (TANGLED ROPE) — Mixed: benefits from secure supply and price stability (coordination function) but extracted from via market power asymmetry. Vault's guaranteed output locks in supply without pricing flexibility. Nations have mobile capital but face lock-in to single buyer. d≈0.55, f(d)≈0.72, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(project_vault_extraction_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE / ENERGY TRANSITION ADVOCATES (SCAFFOLD) — Temporary support with sunset logic. Vault provides critical minerals for renewable energy transition (batteries, EV drivetrains, solar panels). Benefits from short-term supply security. Sunrise clause: renewable energy and battery recycling pathways will reduce critical mineral demand after 15-25 years. Sunset: constraint becomes redundant as circular economy scales. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.53. Theater is moderate (0.55) — true supply coordination overlaid with extraction mechanics.
constraint_indexing:constraint_classification(project_vault_extraction_2026, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: HOST-COUNTRY GOVERNMENT (PITON) — Nominally sovereign but structurally degraded. Government authority over resources is theatrical: Vault operates under concession agreement with exit penalties exceeding government budget; profit mandate overrides public interest considerations; renegotiation triggers loan acceleration clauses. Government maintains symbolic control (licensing, permitting) without functional governance. theater_ratio=0.55 shows mixed signals; institutional inertia evident in government's continued framing as 'public asset' despite effective privatization. d≈0.70, f(d)≈1.10, σ=1.0 → χ≈0.75.
constraint_indexing:constraint_classification(project_vault_extraction_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURALIZED VIEW (PITON-AS-MOUNTAIN RISK) — Risk of false summit: 'Strategic minerals require debt financing; extraction is inevitable and necessary for national security/climate transition.' This frames contingent institutional arrangement (debt-financed extraction with profit mandate) as natural law. However, structural data contradicts this: suppression (0.72), extractiveness (0.68), and the explicit victim declarations show this is enforced extraction, not immutable necessity. Theater (0.55) indicates genuine coordination function overlaid with extraction — if institutional inertia fails, mountain claim collapses. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.00.
constraint_indexing:constraint_classification(project_vault_extraction_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(project_vault_extraction_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(project_vault_extraction_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(project_vault_extraction_2026, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(project_vault_extraction_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(project_vault_extraction_2026, TR),
    TR >= 0.70.

:- end_tests(project_vault_extraction_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint creates asymmetric value capture via debt financing (government bears currency and interest risk; creditors capture fixed returns), profit mandate (all surplus above servicing costs flows to EXIM and consortium), and acceleration clauses (falling commodity prices trigger forced extraction intensification rather than reduced pressure). The initial value (0.45) reflects the supply coordination function; it rises to 0.68 as profit extraction and debt pressures intensify. Suppression (0.72): Very high. Communities lack exit options (geographic immobility, livelihood dependence); competing producers face capital lock-in and market barriers; future governments inherit non-negotiable debt obligations; sourcing nations face long-term supply contracts at asymmetric terms. Exit costs are prohibitive across all victim groups. Theater ratio (0.55): Moderate. The constraint combines genuine supply coordination (minerals are needed for energy transition) with extraction theater. Host-country government maintains performative 'public ownership' despite functional privatization; environmental and social responsibility frameworks are ceremonial compliance. Theater is lower (0.55 vs typical snare ~0.65) because the supply coordination function is real, not purely theatrical — battery demand is genuine. Claimed type: Snare. Meets all thresholds: χ ≥ 0.66 from primary victim perspectives; suppression ≥ 0.60; extractiveness ≥ 0.46.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and structural. EXIM Bank and the consortium see a profitable coordination mechanism (rope/arbitrage perspective): they solve supply security for downstream nations and guarantee mining operations profitability. Surrounding communities see irreversible extraction (snare): environmental harms, water depletion, and livelihood degradation with no exit. Competing domestic producers see market elimination (snare): subsidized Vault debt undercuts their margins and forces write-downs or exit. The host-country government sees performative ownership (piton): it issues licenses and holds concession agreements but lacks functional control — loan covenants and profit mandates override its public interest authority. Future public budgets see inherited extraction (snare/generational): the $10B debt obligation persists for 20+ years regardless of commodity prices or political change. The energy transition advocate sees temporary supply (scaffold): believes battery recycling will make Vault redundant in 15-25 years. The analytical observer risks naturalizing the entire structure as immutable necessity for strategic minerals — a false summit that obscures the contingent institutional choices (debt financing, profit mandate, market concentration) that enable extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Surrounding communities: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction. Geographic immobility + irreversible environmental externalities = trapped status. EXIM Bank: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Can securitize, refinance, or exit; profit mandate ensures extraction prioritization. Domestic producers: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction. Can theoretically relocate but face prohibitive capital loss; market barriers trap them. Host-country government: Mixed. Nominal beneficiary (license fees, concession payments); actual victim (debt lock-in, authority erosion). Override to d≈0.70 to reflect institutional capture: nominally in control, structurally subordinated to loan covenants. Future public budgets: Victim + trapped → d≈0.88, f(d)≈1.35. Very high extraction. Debt servicing obligations are non-negotiable; future governments cannot exit without sovereign default. Sourcing nations: Victim + constrained/mobile (ambiguous) → d≈0.55 (canonical for powerful/mobile mixed). Can diversify supply but face lock-in to Vault through long-term contracts and sunk relationship investments. Scaffold perspective (climate advocates): Beneficiary + mobile → d≈0.40 (organized/mobile with sunset logic). Extract low value from their perspective; they expect constraint to dissolve.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by explicitly declaring extraction (snare) rather than coordination (rope). The temptation is to classify Vault as a rope or tangled_rope because it provides genuine supply security for energy transition — a legitimate coordination service. The mandatrophy check prevents this error: (1) The structural data shows ε=0.68 (high extraction) and suppression=0.72 (very high barriers to exit), not the low-extraction signature of rope. (2) The beneficiary/victim declarations show systematic asymmetry: beneficiaries (EXIM, consortium) have arbitrage exits; victims (communities, competitors, future budgets) have trapped/constrained exits. (3) The profit mandate creates extractive intent: value capture is the primary objective, supply coordination is the mechanism. (4) The debt acceleration clauses show that if supply coordination were the goal, commodity price declines would trigger less extraction; instead, they trigger intensification. The snare classification is robust. The scaffold perspective (energy transition advocates) expects the constraint to sunset as battery recycling scales — this is legitimate (true sunset logic, not aspirational). The piton perspective (host-country government) shows institutional degradation: nominal control without functional power. The analytical observer's risk is to naturalize contingent institutional arrangements (debt financing, profit mandate, market concentration) as immutable necessity — the false summit detector catches this as a risk but does not override the snare classification. Mandatrophy is resolved: this is extraction enabled by institutional design, not necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_acceleration_threshold,
    'At what commodity price floor do debt acceleration clauses trigger forced extraction intensification?',
    'Analysis of loan covenants; historical modeling of price volatility vs acceleration triggers; comparison to commodity market cycles',
    'If threshold is low (<30% of current price): constraint is purely extractive (Snare confirmed). If threshold is high (>60% price decline): short-term profitability protection suggests mixed motives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_acceleration_threshold, empirical, 'Debt acceleration trigger pricing threshold').

omega_variable(
    environmental_penalty_vs_profit,
    'Do environmental remediation and community compensation costs actually constrain profit extraction, or are they fully externalized to public budgets?',
    'Audit of Vault''s environmental accounting and comparison to industry standards; analysis of actual vs contractual remediation obligations',
    'If externalized: snare classification confirmed at all victim perspectives. If internalized: effective extractiveness drops to ~0.45, potentially reclassifying as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_penalty_vs_profit, empirical, 'Whether environmental costs are internalized or externalized').

omega_variable(
    loan_securitization_and_predatory_cascades,
    'Does EXIM Bank''s ability to securitize the Vault loan enable predatory financial cascades in downstream markets (pension funds, insurance companies forced to hold toxic environmental exposure)?',
    'Tracing securitization pathways; analysis of pension fund exposure to Vault debt; correlation between debt sales and downstream institutional distress',
    'If true: extraction scales beyond Vault''s direct victims to financial system-wide victims. Snare classification strengthens; mandatrophy risk increases if extraction becomes ''financial system contagion.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loan_securitization_and_predatory_cascades, conceptual, 'Whether securitization creates predatory financial cascades').

omega_variable(
    battery_recycling_timeline,
    'Will battery recycling technologies mature quickly enough (10-15 years) to make Vault''s sunset clause in the scaffold perspective plausible, or is the transition timeline indefinite?',
    'Technical roadmap for battery recycling scale-up; comparison to current industry capacity; modeling of EV fleet build-out and end-of-life timelines',
    'If timeline is 10-15 years: scaffold classification is realistic, constraint has real sunset logic. If timeline is 30+ years: scaffold is aspirational (false sunset), constraint persists indefinitely as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(battery_recycling_timeline, empirical, 'Timeline for battery recycling technology maturation').

omega_variable(
    alternative_supply_pathways,
    'Do alternative sourcing models (allied nation stockpiles, ocean nodules, urban mining) provide genuine exit options for downstream industries, or is Vault''s supply lock-in functionally irreversible?',
    'Inventory analysis of global strategic mineral reserves; technical feasibility and cost modeling for alternative extraction; analysis of trade agreements locking in Vault sourcing',
    'If alternatives exist: effective suppression drops, some victims (sourcing nations, downstream) gain constrained→mobile transition, reclassifying from snare toward tangled_rope. If Vault is only viable source: suppression confirmed, snare classification solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_pathways, empirical, 'Availability of alternative supply pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(project_vault_extraction_2026, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vault_tr_t0, project_vault_extraction_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(vault_tr_t2, project_vault_extraction_2026, theater_ratio, 2, 0.48).
narrative_ontology:measurement(vault_tr_t4, project_vault_extraction_2026, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(vault_be_t0, project_vault_extraction_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vault_be_t2, project_vault_extraction_2026, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(vault_be_t4, project_vault_extraction_2026, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(project_vault_extraction_2026, resource_allocation).
narrative_ontology:boltzmann_floor_override(project_vault_extraction_2026, 0.35).
narrative_ontology:affects_constraint(project_vault_extraction_2026, critical_minerals_supply_dependency).
narrative_ontology:affects_constraint(project_vault_extraction_2026, battery_supply_chain_lock_in).
narrative_ontology:affects_constraint(project_vault_extraction_2026, environmental_externality_asymmetry).
narrative_ontology:affects_constraint(project_vault_extraction_2026, sovereign_debt_constraint).

% DUAL FORMULATION NOTE:
% Project Vault decomposes into four structural constraints: (1) Critical minerals supply dependency (ε~0.30, rope-like coordination function), (2) Debt-financed extraction mechanics (ε=0.68, snare — this file), (3) Environmental externality asymmetry (ε~0.55, tangled_rope — coordination for transition + extraction of environmental costs), (4) Sovereign debt lock-in (ε~0.75, snare — host-country constraint). The extraction ε=0.68 is distinct from the supply coordination ε~0.30 — the same project exhibits different ε values depending on which aspect is evaluated. This file focuses on the debt-extraction constraint; upstream constraint is supply_dependency; downstream constraint is environmental_asymmetry and sovereign_debt_lock_in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(project_vault_extraction_2026, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
