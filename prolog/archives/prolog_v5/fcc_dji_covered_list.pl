% ============================================================================
% CONSTRAINT STORY: fcc_dji_covered_list
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fcc_dji_covered_list, []).

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
 *   constraint_id: fcc_dji_covered_list
 *   human_readable: FCC 'Covered List' Ban on DJI Drones
 *   domain: technological/political/regulatory
 *
 * SUMMARY:
 *   The FCC's 'Covered List' ban on DJI drones (announced 2024, enforced
 *   2025-present) represents a deliberate regulatory intervention to decouple
 *   U.S. drone supply chains from Chinese manufacturing and remove suspected
 *   national security vulnerabilities. The constraint exhibits the core
 *   mandatrophy tension: Is the ban primarily a coordination mechanism
 *   (establishing supply-chain security and domestic innovation incentives)
 *   or an extraction mechanism (transferring market value from DJI users to
 *   domestic manufacturers)? The constraint demonstrates why indexical
 *   classification is necessary. From a powerless commercial operator's
 *   perspective (trapped exit), the ban is pure extraction — they lose their
 *   preferred supplier with no transition period. From a domestic
 *   manufacturer's perspective (arbitrage exit), the ban is pure coordination
 *   — it solves their collective cost-competitiveness problem. From the
 *   defense establishment's perspective (constrained by geopolitical risk),
 *   the ban is mixed: genuine security coordination but also constrained by
 *   retaliation risk. The theater ratio (0.65) reflects that the regulatory
 *   framing emphasizes supply-chain security, but the actual distributional
 *   logic is industrial policy — market protection for domestic firms under
 *   the cover of national security.
 *
 * KEY AGENTS:
 *   - Small Commercial Drone Operators: Primary victims (powerless/trapped) — cannot exit without massive capital reallocation or business closure; no transition period provided
 *   - Agricultural & Precision Imaging Industry: Secondary victims (organized/constrained) — benefit from coordination but constrained by supplier substitution barriers
 *   - DJI Commercial Operations: Primary victim (institutional/constrained) — loses U.S. market access; cannot negotiate grandfathering or transition
 *   - Domestic Drone Manufacturers (Skydio, Altus, Auteryx): Primary beneficiaries (institutional/arbitrage) — gain market share redistribution and government preference
 *   - U.S. Defense & Intelligence Establishment: Mixed institutional beneficiary (institutional/constrained) — gains supply-chain security but faces geopolitical retaliation risk
 *   - FCC Regulatory Apparatus: Institutional implementer (institutional/constrained) — enforces a geopolitical decision; formal role is performative
 *   - The Commercial Drone Transition Coalition: Organized temporary coordinator (organized/mobile) — industry groups see sunset horizon as domestic capabilities mature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fcc_dji_covered_list, 0.58).
domain_priors:suppression_score(fcc_dji_covered_list, 0.72).
domain_priors:theater_ratio(fcc_dji_covered_list, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fcc_dji_covered_list, extractiveness, 0.58).
narrative_ontology:constraint_metric(fcc_dji_covered_list, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fcc_dji_covered_list, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fcc_dji_covered_list, tangled_rope).
narrative_ontology:human_readable(fcc_dji_covered_list, "FCC 'Covered List' Ban on DJI Drones").
narrative_ontology:topic_domain(fcc_dji_covered_list, "technological/political/regulatory").

domain_priors:requires_active_enforcement(fcc_dji_covered_list).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, domestic_drone_manufacturers).
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, u_s_defense_establishment).
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, regulatory_enforcement_agencies).
narrative_ontology:constraint_victim(fcc_dji_covered_list, dji_commercial_operations).
narrative_ontology:constraint_victim(fcc_dji_covered_list, agricultural_users).
narrative_ontology:constraint_victim(fcc_dji_covered_list, precision_filmmaking_industry).
narrative_ontology:constraint_victim(fcc_dji_covered_list, small_commercial_drone_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL COMMERCIAL DRONE OPERATOR (SNARE) — Trapped within the U.S. market with no viable alternative suppliers at comparable price-performance. DJI dominates affordable professional-grade drones; competitors exist but are substantially more expensive or functionally limited. Operators cannot exit without massive capital reallocation or business closure. Suppression is severe: ban enforcement includes seizure penalties, and no grandfathering or transition period was provided for existing DJI fleet operators. High extraction of accumulated business value.
constraint_indexing:constraint_classification(fcc_dji_covered_list, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AGRICULTURAL & PRECISION IMAGING INDUSTRY (TANGLED ROPE) — Organized actors (agricultural associations, surveying firms, film production guilds) benefit from coordination on supply-chain stability and regulatory clarity. But the ban constrains their choice set — they cannot easily substitute to non-DJI platforms at equivalent cost. They also face genuine coordination benefits from regulation that prevents malicious drone use and establishes safety standards. Requires active enforcement (FAA integration). Mixed extraction: lose preferred supplier, but gain regulatory clarity and coordination certainty.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC DRONE MANUFACTURERS & DEFENSE CONTRACTORS (ROPE) — Primary institutional beneficiary. Market protection through ban creates arbitrage opportunity: DJI market share redistributes to Auteryx, Skydio, Altus, and other U.S.-based firms. Benefits from supply-side coordination: government purchases, R&D subsidies, preferential procurement policies. Experiences constraint as pure coordination — the ban solves their collective action problem (competing with DJI's manufacturing cost advantage).
constraint_indexing:constraint_classification(fcc_dji_covered_list, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: U.S. DEFENSE & INTELLIGENCE ESTABLISHMENT (TANGLED ROPE) — Benefits from supply-chain security (reduces technical backdoor risk in civilian drone ecosystem). But constrained by geopolitical retaliation risk: China may retaliate with restrictions on U.S. tech exports or rare-earth mineral restrictions. Also constrained by enforcement burden — the ban requires ongoing verification and interagency coordination. The coordination function is genuine (supply-chain security, counter-espionage), but extraction is present (forcing costs onto civilian operators to achieve security margin that primarily benefits defense).
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FCC REGULATORY APPARATUS (PITON) — The FCC exercises regulatory authority over spectrum and equipment certification, but the DJI ban is primarily driven by national security concerns (DoD/CFIUS), not spectrum management or RF safety. The FCC's formal institutional role in the ban is largely performative — the agency implements and enforces a geopolitical decision made elsewhere. Theater ratio is high because the regulatory framing (equipment safety, spectrum integrity) masks the underlying security rationale. Regulatory ritual persists through institutional necessity rather than functional necessity.
constraint_indexing:constraint_classification(fcc_dji_covered_list, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMMERCIAL DRONE INDUSTRY TRANSITION COALITION (SCAFFOLD) — Organized industry groups (drone manufacturers, user associations) see the ban as a temporary coordination mechanism with a sunset: alternative U.S.-based suppliers and open-source platforms are maturing. The ban provides a temporary (sunset ~5-10 years) protection window for domestic industry to scale manufacturing, reduce costs, and achieve feature parity with DJI. Beneficiaries: domestic manufacturers. Coordination function: supply-chain diversification. Theater is moderate because the transition goal is explicit. Suppression is high during the window but has a decay trajectory.
constraint_indexing:constraint_classification(fcc_dji_covered_list, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, supply-chain decoupling between the U.S. and China is treated as inevitable and structurally irreversible. The ban appears as an immutable law of late-stage great-power competition. However, this perspective risks naturalizing what is actually a contingent policy choice. The constraint is not a mountain — it is a deliberate regulatory intervention with observable distributional costs. The false summit reveals normalization of geopolitical extraction.
constraint_indexing:constraint_classification(fcc_dji_covered_list, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fcc_dji_covered_list_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fcc_dji_covered_list, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fcc_dji_covered_list, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fcc_dji_covered_list, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fcc_dji_covered_list, TR),
    TR >= 0.70.

:- end_tests(fcc_dji_covered_list_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The ban redistributes significant market value ($1.5-2.5B annually estimated DJI U.S. revenue) to domestic manufacturers without full compensation to disrupted users. However, extractiveness is not maximal because genuine coordination benefits exist — supply-chain security reduces backdoor risk, and regulatory stability benefits organized operators. The value is extracted but with plausible justification. Suppression (0.72): High. Enforcement includes seized equipment, operation penalties, and no grandfathering for existing users. Barriers to exit are severe: no domestic alternatives achieve DJI's price-performance, open-source platforms are immature, and international procurement is restricted. Theater ratio (0.65): Moderate-high. The regulatory framing emphasizes spectrum safety and equipment certification (FCC's traditional mandate), but the actual driver is national security (DoD/CFIUS). The constraint is justified by security concerns that are difficult to independently verify. Regulatory theater has increased as the ban matured — enforcement shifted from equipment certification focus to geopolitical security rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between beneficiaries and victims. Beneficiaries (domestic manufacturers, defense establishment) experience the ban as legitimate coordination — it solves their collective action problem (DJI cost advantage, supply-chain security). Victims (commercial operators, DJI) experience it as pure extraction — loss of market access with no transition or compensation. The organized intermediate actors (agricultural industry, transition coalition) perceive mixed extraction — loss of supplier choice but gain of regulatory clarity. The gap is irreducible because it reflects genuine asymmetry in exit options and distributional impact, not measurement uncertainty. The constraint is truly a tangled rope: it exhibits both real coordination function (supply-chain security, regulatory clarity) and real extraction (market redistribution without compensation).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions relative to the extraction flow. Commercial operators are trapped victims (d ≈ 0.90): high directionality toward target, high experienced extraction. Domestic manufacturers are beneficiaries with arbitrage options (d ≈ 0.10): low directionality, negative experienced extraction. The defense establishment occupies a liminal position: structural beneficiary (supply-chain security) but constrained by geopolitical risk (d ≈ 0.55). Institutional agents with constrained exit options (organized industry groups, FCC apparatus) occupy d ≈ 0.50-0.65: mixed extraction. The derivation chain priority is: (1) explicit override (none declared), (2) structural derivation from beneficiary/victim + exit options, (3) canonical fallback. Most perspectives use structural derivation — the differentiation comes from heterogeneous exit options across agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED: This constraint exhibits the core mandatrophy — the classification is contingent on which framing dominates. If the supply-chain security benefit is treated as primary, the ban is justified as a tangled rope: real coordination (security) with justified extraction (transitional cost). If the market redistribution is treated as primary, the ban is a snare with security theater — the security justification masks industrial policy. The constraint does not resolve mandatrophy because the empirical facts underdetermining the classification are genuinely unresolved: (1) the quantified magnitude of DJI backdoor risk is classified and not independently verifiable, (2) the cost-benefit of the ban relative to alternative security policies (firmware inspection, U.S.-based software layer, hardware escrow) is not transparent, (3) the transition timeline for domestic suppliers to achieve parity is uncertain. The mandate ambiguity persists because decision-makers have not revealed their true cost-benefit calculation. Transparency on the threat assessment would resolve the mandatrophy toward either justified tangled rope (threat confirmed) or security theater snare (threat speculative). Without such transparency, the constraint remains in mandatrophy superposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_supplier_emergence_timeline,
    'Will domestic drone manufacturers achieve genuine feature/cost parity with DJI platforms within 5-7 years, or will the ban persist indefinitely due to cost asymmetries?',
    'Track Skydio, Altus, Auteryx technical specifications and pricing trajectory; measure market share recovery for domestic brands post-ban; assess manufacturing cost drivers',
    'If parity achieved: scaffold sunset is real, ban transitions to maintenance-only enforcement. If parity not achieved: ban becomes permanent snare for commercial users, and the ''temporary protection'' framing was always false.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supplier_emergence_timeline, empirical, 'Timeline for domestic drone manufacturers to achieve feature/cost parity with DJI').

omega_variable(
    supply_chain_security_threat_magnitude,
    'What is the quantified risk of DJI hardware/software backdoors for U.S. national security, and how large is the actual threat compared to the justified policy response?',
    'CFIUS threat assessments; NSA/NRO technical analysis of DJI firmware; comparative risk analysis of alternative drone platforms; evidence of actual compromised data collection',
    'If threat is substantial and well-documented: defense extraction is justified, tangled_rope classification confirmed. If threat is speculative or asymmetric relative to response: ban represents security theater masking economic protectionism, snare classification applies to both users and defense establishment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_security_threat_magnitude, empirical, 'Quantified national security risk from DJI supply chain vulnerabilities').

omega_variable(
    distributional_cost_allocation,
    'How much economic value is extracted from commercial users vs distributed to domestic manufacturers through the ban, and is the allocation justified by coordination benefits?',
    'Industry loss surveys; competitive bid analysis; market cap redistribution in domestic drone sector; comparison to alternative security policies (e.g., firmware inspection, U.S.-based software layer)',
    'If cost redistribution is proportional to security benefit: tangled_rope. If extraction vastly exceeds coordination benefit: snare for users, institutional rent-seeking for beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_cost_allocation, empirical, 'Economic value extraction from commercial users vs benefit to domestic manufacturers').

omega_variable(
    geopolitical_retaliation_risk,
    'Will China retaliate with restrictions on U.S. technology exports (semicondulators, software, rare earths), and will the cost of retaliation exceed the security benefit of the DJI ban?',
    'Trade negotiations analysis; retaliatory action observed post-ban; economic impact assessment; semiconductor supply disruption modeling',
    'If retaliation is severe: defense establishment bears extraction costs (constraint becomes tangled_rope for them too), and the ban''s net security benefit becomes negative. If retaliation is minimal: extraction flows to domestic manufacturers as intended, ban remains justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_retaliation_risk, empirical, 'Likelihood and magnitude of Chinese retaliation against U.S. technology exports').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fcc_dji_covered_list, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcc_dji_tr_t0, fcc_dji_covered_list, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fcc_dji_tr_t2, fcc_dji_covered_list, theater_ratio, 2, 0.58).
narrative_ontology:measurement(fcc_dji_tr_t5, fcc_dji_covered_list, theater_ratio, 5, 0.65).

% Extraction over time
narrative_ontology:measurement(fcc_dji_be_t0, fcc_dji_covered_list, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fcc_dji_be_t2, fcc_dji_covered_list, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(fcc_dji_be_t5, fcc_dji_covered_list, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fcc_dji_covered_list, enforcement_mechanism).
narrative_ontology:affects_constraint(fcc_dji_covered_list, semiconductor_supply_chain_decoupling).
narrative_ontology:affects_constraint(fcc_dji_covered_list, rare_earth_supply_security).
narrative_ontology:affects_constraint(fcc_dji_covered_list, defense_contractor_procurement_preference).

% DUAL FORMULATION NOTE:
% The FCC DJI ban represents a downstream enforcement mechanism in a broader constraint family: supply-chain decoupling between the U.S. and China. The semiconductor_supply_chain_decoupling constraint has ε ≈ 0.35 (Rope: pure coordination on redesign and domestic fab development). The DJI ban has ε ≈ 0.58 (Tangled Rope: both coordination and extraction). The family hierarchy reflects that the upstream constraint (semiconductor) is a coordination solution, while the downstream constraint (DJI) is a mixed mechanism that leverages coordination rhetoric to justify extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fcc_dji_covered_list, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
