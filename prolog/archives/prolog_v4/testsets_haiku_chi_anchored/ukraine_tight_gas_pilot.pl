% ============================================================================
% CONSTRAINT STORY: ukraine_tight_gas_pilot
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukraine_tight_gas_pilot, []).

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
 *   constraint_id: ukraine_tight_gas_pilot
 *   human_readable: Ukraine Tight Gas Pilot Project Framework
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   Ukraine's tight gas pilot project framework represents a partnership
 *   between Naftogaz (state-backed national gas company) and Expert Petroleum
 *   (foreign technical expert) to develop tight gas reserves previously
 *   considered inaccessible without advanced extraction technology. The
 *   framework exhibits a hybrid coordination-extraction structure: it solves
 *   a genuine technical problem (tight gas requires specialized knowledge and
 *   equipment), but it does so through an exclusionary mechanism that
 *   prevents competitive bidding, locks out domestic competitors, and shifts
 *   economic costs to domestic consumers and environmental stakeholders. The
 *   constraint is sustained by state preference for Naftogaz monopoly
 *   control, geopolitical urgency (energy independence narrative post-Russia
 *   relations), and regulatory capture of environmental oversight. This
 *   creates a Tangled Rope classification at the analytical level: the
 *   coordination function (transferring tight gas extraction expertise) is
 *   real and valuable; the asymmetric extraction (exclusive rents, consumer
 *   cost-shifting, environmental externality) is also real. From the
 *   perspective of domestic consumers and competing companies, the framework
 *   appears as a Snare — pure extraction with no offsetting coordination
 *   benefit.
 *
 * KEY AGENTS:
 *   - Naftogaz Leadership & Ukrainian Energy Ministry: Primary beneficiary (institutional/arbitrage) — gains exclusive control, energy independence positioning, state revenue from exports
 *   - Expert Petroleum Corporation: Secondary beneficiary & coordinator (institutional/arbitrage) — gains exclusive technical access, knowledge transfer, regulatory predictability despite geopolitical risk
 *   - Ukrainian Domestic Consumers: Primary victim (powerless/trapped) — bear tariff costs, energy dependency, cannot exit or choose alternative suppliers
 *   - Competing Ukrainian Gas Companies: Secondary victim (organized/constrained) — locked out by exclusivity, resource-disadvantaged, can organize but face state preference
 *   - Regional Environmental Stakeholders: Tertiary victim (powerless/trapped) — bear extraction costs (water contamination, seismicity, land use conflict), no regulatory recourse, cannot exit
 *   - Geopolitical Analyst: Observer (analytical/analytical) — sees both genuine coordination function and asymmetric extraction; recognizes hybrid structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukraine_tight_gas_pilot, 0.58).
domain_priors:suppression_score(ukraine_tight_gas_pilot, 0.62).
domain_priors:theater_ratio(ukraine_tight_gas_pilot, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, extractiveness, 0.58).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ukraine_tight_gas_pilot, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukraine_tight_gas_pilot, tangled_rope).
narrative_ontology:human_readable(ukraine_tight_gas_pilot, "Ukraine Tight Gas Pilot Project Framework").
narrative_ontology:topic_domain(ukraine_tight_gas_pilot, "economic/geopolitical").

domain_priors:requires_active_enforcement(ukraine_tight_gas_pilot).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, naftogaz_leadership).
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, expert_petroleum_corp).
narrative_ontology:constraint_beneficiary(ukraine_tight_gas_pilot, ukrainian_government_energy_ministry).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, competing_gas_companies).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, ukrainian_domestic_consumers).
narrative_ontology:constraint_victim(ukraine_tight_gas_pilot, regional_environmental_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UKRAINIAN DOMESTIC CONSUMERS (SNARE) — Trapped by energy dependence and limited consumer choice. Exclusivity framework prevents competitive pricing; costs of pilot passed to consumers via tariff structures. No viable exit; dependent on state gas supply. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL ENVIRONMENTAL STAKEHOLDERS (SNARE) — Bear costs of tight gas extraction (water contamination risk, induced seismicity, land access) with no participation in decision-making. No exit option; regulatory capture limits accountability. d≈0.94, f(d)≈1.40, σ=0.9 → χ≈0.82.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMPETING UKRAINIAN GAS COMPANIES (SNARE) — Locked out of tight gas pilot by exclusivity clauses. Organized but constrained by state preference for Naftogaz partnership. Can operate in non-pilot fields but face resource disadvantage. d≈0.85, f(d)≈1.22, σ=1.0 → χ≈0.71.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EXPERT PETROLEUM CORP (TANGLED ROPE) — Gains exclusive technical access and knowledge transfer; coordinates supply expertise with state actor. But also experiences constraints: Ukrainian regulatory uncertainty, geopolitical risk, limited arbitrage options if political situation shifts. Benefits from coordination role (knows unique reserve geology, technologies); pays costs of political exposure. d≈0.52, f(d)≈0.70, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NAFTOGAZ LEADERSHIP & ENERGY MINISTRY (ROPE) — Primary beneficiary. Gains exclusive control over tight gas development, avoids competitive bidding, secures energy independence narrative, and positions as energy exporter. Experiences framework as coordination: organizing state resources, partnering with technical expert, solving technical problem. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GEOPOLITICAL ANALYST (TANGLED ROPE) — Sees both coordination function (Ukraine genuinely needs technical expertise for complex reserves, partnership solves real technical problem) AND asymmetric extraction (exclusive framework creates rents, locks out competitors, shifts costs to consumers and environment). The coordination benefit (technical knowledge transfer, reserve development) is real; the extraction asymmetry (captured regulatory process, consumer cost-shifting) is also real. This is a genuine hybrid. d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukraine_tight_gas_pilot_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukraine_tight_gas_pilot, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukraine_tight_gas_pilot, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ukraine_tight_gas_pilot_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated from initial assessment (0.38) as pilot operationalization reveals actual cost structures. The framework creates exclusive rents through regulatory capture and competitive exclusion. Tariffs are rising to fund pilot infrastructure, and competing companies face genuine resource disadvantage. However, the extractiveness is moderated from a pure Snare level (0.75+) because the coordination function is real — tight gas genuinely requires specialized technology and long-term commitment, and the partnership does transfer that expertise. The measure reflects both benefits (technical knowledge) and costs (exclusivity rents) weighted toward beneficiaries. Suppression (0.62): Significant. Multiple mechanisms suppress alternatives: (1) regulatory preference for Naftogaz exclusivity prevents competitive bidding, (2) environmental oversight is captured by energy ministry (weak independent regulation), (3) consumer choice is eliminated by monopoly structure, (4) competing companies face state barriers. But suppression is not maximal (0.80+) because some regulatory appeals exist (EU integration pressure, international environmental standards) and the pilot has sunset logic implicitly (if successful, may transition to normal operations; if failed, may be abandoned). Theater ratio (0.65): Moderate-high. Significant performative content includes: energy independence narrative framing (masks state rent-capture); environmental 'due diligence' processes (weak oversight); technical necessity justification (overstates exclusivity requirement). But actual technical coordination is occurring (not pure theater); thus 0.65 rather than 0.80+.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival gap. Naftogaz leadership sees Rope (coordination benefit of technical expertise). Expert Petroleum sees Tangled Rope (genuine technical partnership mixed with geopolitical risk). Competing companies see Snare (locked out, disadvantaged, no compensation). Domestic consumers see Snare (tariff costs, no alternative supplier, no exit). Environmental stakeholders see Snare-with-No-Recourse (externalized costs, regulatory capture, no voice in decision-making). The analytical observer sees Tangled Rope (real coordination function, real asymmetric extraction, both structural features). The gap exists because the exclusivity mechanism creates a radially asymmetric distribution of benefits and costs — beneficiaries experience genuine coordination value; victims experience pure extraction cost-shifting.
 *
 * DIRECTIONALITY LOGIC:
 *   Naftogaz & Ministry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Positive arbitrage (can exit by renegotiating or terminating partnership, maintains state control option). Net beneficiary. Expert Petroleum: Mixed beneficiary + constrained arbitrage → d≈0.52, f(d)≈0.70. Gains exclusive access but faces geopolitical risk that narrows arbitrage options. Partial beneficiary with extraction cost. Competing companies: Victim + constrained → d≈0.85, f(d)≈1.22. Locked out of pilot, face state barriers, organized but cannot exit nationally. Significant extraction relative to organized status. Domestic consumers: Victim + trapped → d≈0.92, f(d)≈1.38. Bear tariff costs with no alternative supplier choice. Maximum extraction for powerless status. Environmental stakeholders: Victim + trapped → d≈0.94, f(d)≈1.40. Bear externalized costs with no participation in decision-making. Maximum extraction; less organized than consumers.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled_rope classification at the analytical level resolves the mandatrophy by acknowledging that the framework has a real coordination function (tight gas expertise transfer) AND real asymmetric extraction (exclusive rents, cost-shifting to consumers and environment). This is not a false summit or a mislabeled Snare — the coordination is genuine, as evidenced by the technical complexity requiring expert input and the knowledge transfer occurring through the partnership. The extraction is also genuine, as evidenced by competitive exclusion, tariff increases, and environmental externalization. The mandatrophy is resolved by recognizing that Tangled Rope is the correct classification when both elements are structurally present. The beneficiary and victim perspectives differ because they measure the constraint from structurally different positions: beneficiaries see the coordination benefit clearly (they designed it to capture that benefit); victims see the extraction asymmetry clearly (they bear the costs). Both perspectives are correct; the tangled_rope classification unifies them by acknowledging that the constraint is genuinely hybrid, not mislabeled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_necessity_vs_political_capture,
    'Is the exclusive partnership framework technically necessary (tight gas truly requires focused expertise and long-term commitment) or primarily a political capture mechanism (could open bidding access similar expertise)?',
    'Comparative analysis: tight gas development outcomes in other countries (Poland, Romania) with competitive vs exclusive frameworks; technical assessments of whether knowledge transfer model requires single-partner exclusivity vs open competitive bidding.',
    'If technically necessary: classification remains Tangled Rope (genuine coordination + extraction hybrid). If primarily political: all perspectives shift toward Snare (extraction mechanism masquerading as technical necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_political_capture, empirical, 'Whether exclusivity is technically necessary or politically motivated').

omega_variable(
    environmental_cost_externalization,
    'Are environmental costs (water table contamination, induced seismicity, land use conflict) being accurately priced into pilot economics, or systematically externalized to uncompensated regional actors?',
    'Independent environmental impact assessment; comparison of stated vs actual remediation costs in tight gas pilots elsewhere; analysis of compensation mechanisms (if any) available to affected communities.',
    'If costs externalized: suppression metric should increase to 0.75+, elevating regional actors from Snare toward Snare-with-No-Recourse. If costs internalized: suppression decreases to 0.45-0.50, classification approaches Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(environmental_cost_externalization, empirical, 'Whether environmental costs are externalized or internalized').

omega_variable(
    geopolitical_constraint_collapse,
    'If Ukraine''s geopolitical risk profile shifts (peace treaty, EU/NATO integration completion, or conversely, territorial loss), how would the exclusive partnership framework respond? Is the constraint robust to geopolitical shock?',
    'Scenario analysis of partnership continuity under: (a) formal peace with Russia, (b) full NATO integration, (c) continued contested territory. Review of contract terms regarding force majeure, sovereignty triggers, and renegotiation clauses.',
    'If framework collapses under geopolitical shift: structure is contingent (Scaffold with political sunset). If robust: structure is more entrenched (moves toward Snare permanence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_constraint_collapse, conceptual, 'Robustness of partnership framework to geopolitical shifts').

omega_variable(
    consumer_tariff_pass_through,
    'What proportion of tight gas pilot costs are being passed to domestic consumers via tariff adjustments vs. absorbed by state budget or foreign investment?',
    'Analysis of tariff change timeline vs pilot cost milestones; breakdown of pilot financing (state budget, international loans, corporate reinvestment); comparison of Ukrainian consumer tariffs vs regional peers.',
    'If >60% passed to consumers: suppression increases to 0.70+, strengthens Snare classification for domestic consumers. If <20% passed: consumers see lower effective extraction, classification might move toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_tariff_pass_through, empirical, 'Proportion of pilot costs passed to consumers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukraine_tight_gas_pilot, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(utg_tr_t0, ukraine_tight_gas_pilot, theater_ratio, 0, 0.5).
narrative_ontology:measurement(utg_tr_t2, ukraine_tight_gas_pilot, theater_ratio, 2, 0.58).
narrative_ontology:measurement(utg_tr_t5, ukraine_tight_gas_pilot, theater_ratio, 5, 0.65).

% Extraction over time
narrative_ontology:measurement(utg_be_t0, ukraine_tight_gas_pilot, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(utg_be_t2, ukraine_tight_gas_pilot, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(utg_be_t5, ukraine_tight_gas_pilot, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukraine_tight_gas_pilot, resource_allocation).
narrative_ontology:affects_constraint(ukraine_tight_gas_pilot, russian_gas_leverage).
narrative_ontology:affects_constraint(ukraine_tight_gas_pilot, european_energy_market_integration).
narrative_ontology:affects_constraint(ukraine_tight_gas_pilot, ukrainian_energy_security).

% DUAL FORMULATION NOTE:
% The tight gas pilot framework is downstream of broader energy independence strategy but represents a distinct structural constraint. The coordination function (technical expertise transfer) is genuine; the extraction asymmetry (exclusive rents) is also genuine. Alternative formulations that emphasize only coordination (pure Rope) or only extraction (pure Snare) fail to capture the hybrid structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ukraine_tight_gas_pilot, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
