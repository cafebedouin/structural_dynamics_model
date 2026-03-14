% ============================================================================
% CONSTRAINT STORY: pandemic_vaccine_nationalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pandemic_vaccine_nationalism, []).

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
 *   constraint_id: pandemic_vaccine_nationalism
 *   human_readable: Pandemic Vaccine Nationalism and Supply Chain Extraction
 *   domain: public_health/political_economy
 *
 * SUMMARY:
 *   During the COVID-19 pandemic (2020-2022), vaccine distribution revealed a
 *   core structural constraint: the simultaneous need for global pandemic
 *   containment and the institutional incentive for high-income nations to
 *   vaccinate their own populations first. This constraint exhibits
 *   properties of both coordination (pandemic control requires global
 *   immunity) and extraction (wealthy nations systematized access to supply
 *   while disease continued spreading in low-income regions). The constraint
 *   operated through multiple enforcement mechanisms: intellectual property
 *   protection preventing manufacturing in lower-income countries,
 *   contractual exclusivity with manufacturers, currency and procurement
 *   advantage for wealthy nations, and geopolitical leverage. The
 *   beneficiaries were vaccine manufacturers (guaranteed markets and premium
 *   pricing) and high-income nations (disease control while maintaining
 *   geopolitical power). The victims were low-income nations (delayed access,
 *   constrained by contractual terms and currency), their populations
 *   (continued disease burden), and the global pandemic containment goal
 *   (variant emergence in under-vaccinated regions undermined overall
 *   containment). Theater increased substantially over the measurement
 *   interval as rhetoric about equity (COVAX) diverged from actual
 *   distribution mechanisms dominated by bilateral deals.
 *
 * KEY AGENTS:
 *   - High-Income Nations (institutional/arbitrage): Primary beneficiaries — early supply access, negotiating power over manufacturers, ability to maintain disease control while others bore pandemic burden
 *   - Vaccine Manufacturers (institutional/arbitrage): Primary beneficiaries — guaranteed markets, premium pricing, IP protection enforcement, exclusive production agreements
 *   - Low-Income Nations (powerless/trapped): Primary victims — constrained by currency, contractual exclusivity, manufacturing capacity limitations, no credible exit from the constraint structure
 *   - Middle-Income Nations (moderate/constrained): Secondary victims with some negotiating capacity — constrained by limited capital and manufacturing capability but able to form coalitions and negotiate technology transfer
 *   - Global Pandemic Containment (powerless/trapped): Abstract victim — continued disease circulation in low-income regions enabled variant emergence, undermining global disease control goal
 *   - World Health Organization (institutional/mobile): Tertiary actor — nominally coordinating via COVAX but lacking enforcement power against bilateral government-manufacturer deals; shifted to theater as actual coordination capacity proved weak
 *   - Analytical Observer (analytical/analytical): Sees simultaneous coordination need and systematic extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pandemic_vaccine_nationalism, 0.58).
domain_priors:suppression_score(pandemic_vaccine_nationalism, 0.65).
domain_priors:theater_ratio(pandemic_vaccine_nationalism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pandemic_vaccine_nationalism, extractiveness, 0.58).
narrative_ontology:constraint_metric(pandemic_vaccine_nationalism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pandemic_vaccine_nationalism, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pandemic_vaccine_nationalism, tangled_rope).
narrative_ontology:human_readable(pandemic_vaccine_nationalism, "Pandemic Vaccine Nationalism and Supply Chain Extraction").
narrative_ontology:topic_domain(pandemic_vaccine_nationalism, "public_health/political_economy").

domain_priors:requires_active_enforcement(pandemic_vaccine_nationalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pandemic_vaccine_nationalism, high_income_nations).
narrative_ontology:constraint_beneficiary(pandemic_vaccine_nationalism, vaccine_manufacturers).
narrative_ontology:constraint_victim(pandemic_vaccine_nationalism, low_income_nations).
narrative_ontology:constraint_victim(pandemic_vaccine_nationalism, global_pandemic_containment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME NATION HEALTH AUTHORITY (SNARE) — Trapped by vaccine scarcity, currency constraints, and contractual terms that enforce exclusive manufacturing commitments to wealthy nations. Bears full cost of delayed vaccination, variant emergence, and continued disease burden. No exit option from the constraint structure itself — limited to participation on extractive terms or vaccine absence.
constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-INCOME NATION COALITION (TANGLED ROPE) — Constrained by competing demands and limited manufacturing capacity, but also benefits from some supply access and technology transfer negotiations. Experiences both coordination (shared interest in disease control) and asymmetric extraction (unequal access timing and pricing). Can negotiate but at high cost; some agency exists through coalitional pressure and manufacturing partnerships.
constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VACCINE MANUFACTURER (ROPE) — Experiences the constraint as coordination of supply chain, intellectual property, and production capacity allocation. Benefits from high-income nation contracts and priority access to raw materials. Extraction runs toward this agent. Active enforcement through IP protection and contractual exclusivity is coordination mechanism for their benefit, not coercion they bear.
constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-INCOME NATION GOVERNMENT (TANGLED ROPE) — Organized agent with significant power to coordinate supply access through procurement, but also constrained by domestic political pressure to vaccinate citizens first. Benefits from early supply access and negotiating power; also bears cost of international reputation damage and political pressure. Active enforcement (purchase orders, contractual priority) serves both coordination and asymmetric extraction functions.
constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WORLD HEALTH ORGANIZATION (PITON) — COVAX initiative represents degraded institutional commitment to equitable distribution. Theater ratio high: public rhetoric emphasizes equity while structural enforcement remains weak relative to bilateral government-manufacturer deals. The organization has nominally shifted from coordination (pandemic control requires global immunity) to theater (equity messaging without enforcement power). Maintains institutional presence through performative multilateralism rather than functional control.
constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination need (pandemic containment requires global immunity coverage) coupled with systematic extraction (wealthy nations securing early/preferential supply while disease spreads in low-income regions, reducing pressure on their own healthcare systems and enabling variant emergence that threatens future pandemic phases). The constraint produces both coordination function and asymmetric extraction simultaneously.
constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pandemic_vaccine_nationalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pandemic_vaccine_nationalism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pandemic_vaccine_nationalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pandemic_vaccine_nationalism, TR),
    TR >= 0.70.

:- end_tests(pandemic_vaccine_nationalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. High-income nations secured vaccines at scale while low-income nations faced multi-year delays. The extraction mechanism is real: wealthy nations' purchasing power and contractual dominance systematically captured supply. However, extractiveness is not at snare levels (≥0.66) because: (1) legitimate coordination problem exists (limited manufacturing capacity in early 2021), (2) some vaccine doses did eventually reach low-income regions through COVAX and manufacturer donations, (3) IP and manufacturing partnerships developed over time showing some flexibility. The constraint is extraction-laden coordination rather than pure rent-seeking. Suppression (0.65): High. Multiple barriers prevented low-income nations from exiting or circumventing the constraint: (a) intellectual property enforcement prevented independent manufacturing, (b) raw material supply chains dominated by high-income nation suppliers, (c) currency constraints prevented market-rate purchasing, (d) contractual exclusivity prevented manufacturers from serving multiple markets simultaneously. Suppression is structural (enforceable through legal and economic mechanisms) rather than internalized. Theater ratio (0.48): Moderate. Early period (months 0-3) focused on action — actual supply allocation and logistical operations. Mid-period (months 6-9) saw emergence of performative commitment to equity (COVAX announcements, rhetorical emphasis on global access) while bilateral deals continued dominating actual supply flows. Late period (months 12+) theater increased as rhetoric about 'ensuring global equity' persisted while structural barriers (now partly normalized) remained.
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals the constraint's dual nature. High-income nations and manufacturers see primarily coordination (rope/tangled rope) — solving the legitimate problem of supply allocation under scarcity. Low-income nations see extraction (snare) — systematic capture of supply with no meaningful exit. The middle-income coalition sees mixed experience (tangled rope) — some coordination benefit from supply relationships but substantial extraction from unequal timing and pricing. The WHO sees its own degraded function (piton) — having lost enforcement power against bilateral deals, it maintains theater through COVAX while bilateral agreements determine actual allocation. The analytical observer sees tangled rope — genuine coordination need coupled with systematic asymmetric extraction. The perspectival gaps demonstrate that indexical position (power, exit options, beneficiary/victim status) determines experienced constraint type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position: (1) High-income nations: beneficiaries with arbitrage exit (can manufacture internally, switch suppliers, negotiate exclusive deals) → low d → negative χ; (2) Vaccine manufacturers: beneficiaries with institutional power and multiple high-income nation customers (arbitrage exit) → very low d → strongly negative χ from their perspective, appears as rope; (3) Low-income nations: victims with trapped exit (constrained by currency, IP law, manufacturing capacity, contractual obligations to manufacturers) → high d → high χ; (4) Middle-income nations: victims with constrained exit (can negotiate some technology transfer but at high cost, limited manufacturing capability) → moderate-high d → moderate χ; (5) Global containment goal: powerless victim with no exit option → highest d → maximum experienced extraction. The derived directionality values confirm the tangled rope classification: beneficiaries experience the constraint as coordination while victims experience extraction, but both functions are present in the same structural mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that 'vaccine nationalism' is not a false dichotomy between coordination (epidemic control) and extraction (supply hoarding). Both are true simultaneously from different positions. High-income nations genuinely faced a coordination problem (limited supply, urgent domestic need). Their solution — prioritizing domestic vaccination — genuinely solved their coordination problem. Simultaneously, that solution systematically extracted from low-income nations by denying them access during the critical early vaccination window. The constraint is tangled rope, not snare, because the coordination function is real and beneficial (domestic vaccination did reduce disease burden in high-income nations). It is not rope because the coordination benefit was asymmetrically distributed. The mandatrophy dissolves when we recognize that indexical position determines whether the same constraint appears as solved (rope from manufacturer perspective) or extractive (snare from low-income nation perspective). No single classification is 'correct' — the presheaf over positions is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    variant_emergence_attribution,
    'To what degree did delayed vaccination in low-income regions directly cause emergence of high-transmissibility variants (Omicron, BA.2, etc.)?',
    'Phylogenetic analysis of variant emergence sites; correlation between vaccination coverage gaps and variant emergence timing by region; modeling of infection dynamics under different vaccination deployment schedules',
    'If high causal link: vaccine nationalism is a snare (direct harm to global containment goal). If low causal link: nationalism may be coordinated strategy with acceptable collateral effects (tangled rope from beneficiary perspective). Attribution determines whether extraction is unambiguous or defensible as risk-sharing asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(variant_emergence_attribution, empirical, 'Causal role of vaccination delays in variant emergence').

omega_variable(
    manufacturing_capacity_constraint,
    'Was global vaccine manufacturing capacity genuinely constrained in early 2021, or were capacity limitations manufactured through IP enforcement and exclusive manufacturing agreements?',
    'Historical analysis of available production lines, raw material availability, licensing agreements offered to low-income manufacturers; counterfactual modeling of production under technology transfer vs current agreements',
    'If genuinely constrained: scarcity justifies allocation mechanism (snare is less extractive—unavoidable rationing). If artificially constrained: extraction mechanism is clear (snare is high-extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_capacity_constraint, empirical, 'Whether manufacturing constraints were structural or contractual').

omega_variable(
    externality_price_discovery,
    'Did high-income nations'' vaccination-first strategy adequately price the negative externality of delayed global immunity (variant risk, economic damage from continued lockdowns in low-income regions)?',
    'Economic modeling of externality costs; comparison of early vaccination spending vs later variant containment costs; analysis of whether beneficiaries compensated for externality damage',
    'If externality underpriced: extraction is uncompensated (clear snare structure for victims). If externality fairly priced through aid/debt relief: extraction is hedged (tangled rope becomes less asymmetric).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_price_discovery, conceptual, 'Whether negative externalities were adequately priced').

omega_variable(
    ip_waiver_counterfactual,
    'Would a temporary TRIPS waiver in 2021 have materially increased vaccine production in low-income regions, or would transfer of IP alone prove insufficient without capital investment and raw material supply agreements?',
    'Analysis of actual manufacturing partnerships when agreements changed (2022 onward); comparison of capacity ramp-up under technology transfer vs pure IP access; historical precedent from other pharmaceutical technology transfers',
    'If waiver would have been sufficient: IP enforcement is primary extraction mechanism (clear snare for victims). If insufficient: extraction is secondary to capital/supply constraints (snare remains but mechanism is more structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ip_waiver_counterfactual, empirical, 'Whether IP waiver alone would enable equitable production').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pandemic_vaccine_nationalism, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pvn_tr_t0, pandemic_vaccine_nationalism, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pvn_tr_t6, pandemic_vaccine_nationalism, theater_ratio, 6, 0.48).
narrative_ontology:measurement(pvn_tr_t12, pandemic_vaccine_nationalism, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(pvn_be_t0, pandemic_vaccine_nationalism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pvn_be_t6, pandemic_vaccine_nationalism, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(pvn_be_t12, pandemic_vaccine_nationalism, base_extractiveness, 12, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pandemic_vaccine_nationalism, resource_allocation).
narrative_ontology:affects_constraint(pandemic_vaccine_nationalism, intellectual_property_enforcement_pharmaceutical).
narrative_ontology:affects_constraint(pandemic_vaccine_nationalism, global_disease_surveillance_capacity).

% DUAL FORMULATION NOTE:
% Vaccine nationalism decomposes into two structurally related constraints: IP enforcement (which prevents low-income manufacturing capacity building) and supply chain coordination (which determines allocation order and pricing). The extractiveness of the combined constraint (0.58) reflects both mechanisms operating together. IP enforcement alone (in isolation) would register as pure snare for low-income manufacturers (~0.70 extractiveness). Supply chain scarcity alone would register as coordination problem (rope, ~0.35 extractiveness). The tangled rope emerges from their simultaneity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pandemic_vaccine_nationalism, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
