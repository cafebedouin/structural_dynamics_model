% ============================================================================
% CONSTRAINT STORY: fcc_dji_covered_list
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: technological/political
 *
 * SUMMARY:
 *   The FCC's 'Covered List' ban on DJI drones (issued under authority
 *   delegated by the Secure Equipment Act of 2021) prohibits the use,
 *   importation, and sale of DJI equipment in the United States, ostensibly
 *   to protect critical infrastructure and federal systems from espionage and
 *   data exfiltration. The constraint operates as a hybrid: it solves a
 *   genuine coordination problem (unified supply-chain security standard
 *   across defense contractors and critical infrastructure operators) while
 *   simultaneously extracting rents from small-scale operators,
 *   alternative-platform manufacturers, and agricultural/construction sectors
 *   dependent on DJI's low-cost solutions. The ban exhibits high suppression
 *   (0.68) because technical alternatives exist but at substantially higher
 *   cost and switching burden, and political alternatives (waiver petitions,
 *   state-level exemptions) are closed to non-institutional actors. The
 *   theater ratio (0.52) reflects that the ban's security framing is
 *   partially performative: the actual espionage risk from DJI drones over
 *   U.S. critical infrastructure has never been publicly detailed, and the
 *   ban's scope (commercial and civilian drones) exceeds the stated threat
 *   (critical infrastructure/federal systems). This creates a structural gap
 *   between the official justification (national security) and the actual
 *   mechanism (supply-chain protection via market foreclosure). The
 *   constraint's extractiveness (0.58) reflects that legitimate coordination
 *   interests coexist with protectionist rent extraction benefiting U.S.
 *   manufacturers.
 *
 * KEY AGENTS:
 *   - DJI Commercial Operations: Primary victim (powerless/trapped) — corporation banned from U.S. market with no recourse; domestic operations cease unless restructured
 *   - Small-Scale Commercial Operators: Primary victim (powerless/trapped) — own DJI equipment with no legal resale; lose contracts and revenue; lack political capital to resist
 *   - Agricultural and Construction Users: Secondary victim (moderate/constrained) — face high switching costs to U.S. alternatives; benefit from coordination ecosystem but extraction dominates
 *   - U.S. Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — gains competitive market position; supply-chain security standardization reduces counter-intelligence burden
 *   - Domestic Drone Manufacturers (Skydio, Auteryon, Freefly): Secondary beneficiary (institutional/arbitrage) — gain protected market position; can charge premium vs DJI-displaced demand
 *   - FCC Regulatory Authority: Institutional enforcer (institutional/arbitrage) — gains administrative authority and bureaucratic scope; can grant/deny waivers as political tool
 *   - Commercial Drone Industry Association: Organized actor (organized/constrained) — benefits from market consolidation but harms from reduced competition and innovation diversity
 *   - Analytical Observer: Geopolitical analyst (analytical/analytical) — observes both legitimate security concerns and protectionist extraction; cannot resolve ambiguity without classified threat data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fcc_dji_covered_list, 0.58).
domain_priors:suppression_score(fcc_dji_covered_list, 0.68).
domain_priors:theater_ratio(fcc_dji_covered_list, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fcc_dji_covered_list, extractiveness, 0.58).
narrative_ontology:constraint_metric(fcc_dji_covered_list, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fcc_dji_covered_list, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fcc_dji_covered_list, tangled_rope).
narrative_ontology:human_readable(fcc_dji_covered_list, "FCC 'Covered List' Ban on DJI Drones").
narrative_ontology:topic_domain(fcc_dji_covered_list, "technological/political").

domain_priors:requires_active_enforcement(fcc_dji_covered_list).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, us_defense_industrial_base).
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, domestic_drone_manufacturers).
narrative_ontology:constraint_beneficiary(fcc_dji_covered_list, fcc_regulatory_authority).
narrative_ontology:constraint_victim(fcc_dji_covered_list, dji_commercial_operations).
narrative_ontology:constraint_victim(fcc_dji_covered_list, small_business_drone_operators).
narrative_ontology:constraint_victim(fcc_dji_covered_list, agricultural_and_construction_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-SCALE COMMERCIAL OPERATOR (SNARE) — Cannot easily exit the U.S. market or switch platforms; owns DJI equipment with no legal resale path; loses contractual revenue streams. Lacks organization, capital, or lobbying power. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81. Pure extraction with high suppression (no technical alternatives available at DJI's price point; switching costs are prohibitive).
constraint_indexing:constraint_classification(fcc_dji_covered_list, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AGRICULTURAL AND CONSTRUCTION USERS (TANGLED ROPE) — Coordination function: standardized DJI platforms enabled efficient crop monitoring and site surveying across supply chains. Extraction: ban forces costly platform transitions and operational disruption. Exit constrained: alternative platforms (Auteryon, Freefly) exist but at 2-4x cost and require retraining. Benefits from regulatory clarity are offset by switching burdens. d≈0.72, f(d)≈1.08, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. DEFENSE INDUSTRIAL BASE (ROPE) — Primary beneficiary. Benefits from market clearance for U.S.-based alternatives (Auteryon, Skydio, Freefly, etc.). Constraint solves coordination problem: unified supply-chain exclusion of foreign platforms reduces espionage/counter-intelligence risk across defense contractors. Exit via arbitrage: can lobby for exceptions, obtain export waivers, or influence policy. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(fcc_dji_covered_list, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMERCIAL DRONE INDUSTRY ASSOCIATION (TANGLED ROPE) — Coordination function: standardized regulatory framework reduces fragmentation across state and local jurisdictions. Extraction: ban favors U.S. manufacturers while excluding foreign competitors (including DJI). Constrained: must lobby within existing regulatory structure; cannot exit system. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Balanced: benefits from market consolidation, harms from reduced competition and innovation.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FCC REGULATORY AUTHORITY (SCAFFOLD) — Coordination function: ban establishes clear technology standards and supply-chain security for critical infrastructure and federal procurements. Enforcement theater: 'covered list' mechanism is procedurally clean but politically theatrical (framing security as technical necessity). Sunset clause implicit: as U.S. alternatives mature and gain market share, the ban's restrictive character could relax into a technology-neutral standard. Arbitrage: FCC can modify definitions, grant waivers, or pivot to alternative mechanisms. d≈0.12, f(d)≈0.05, σ=1.0 → χ≈0.03. Low effective extraction; FCC sees itself as enabling infrastructure.
constraint_indexing:constraint_classification(fcc_dji_covered_list, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: COLD WAR EXPORT CONTROL LEGACY (PITON) — theater_ratio=0.52. The 'covered list' ban invokes Cold War-era export control narratives (ITAR, EAR, FCC jurisdiction over foreign technology) that persist through inertia. Original function: control military-grade technology diffusion. Degraded function: applied to commercial drones where actual espionage risk is contested. Maintained through institutional inertia and lobbying from beneficiary industries rather than demonstrated security necessity. d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.01.
constraint_indexing:constraint_classification(fcc_dji_covered_list, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL VIEW (TANGLED ROPE) — Coordination function: genuine security interest in limiting data collection by Chinese state-influenced entity over U.S. infrastructure. Extraction: protectionist mechanism that excludes competition, raises consumer prices, limits innovation diversity. Base extractiveness (0.58) reflects that both functions are structurally present. Suppression (0.68) reflects active enforcement and limited technical alternatives. d≈0.60, f(d)≈0.78, σ=1.2 → χ≈0.54.
constraint_indexing:constraint_classification(fcc_dji_covered_list, tangled_rope,
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
    constraint_indexing:constraint_classification(fcc_dji_covered_list, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The ban is not pure extraction (ε would approach 0.70+) because the coordination function is genuine: standardized supply-chain security does reduce administrative friction for federal procurements and critical infrastructure operators. However, extractiveness is elevated above pure coordination (≈0.30) because the ban uses security rhetoric to achieve what is substantially protectionist outcome. The benefit to U.S. manufacturers (higher market share, price premium) is not incidental — it is explicitly celebrated in industry advocacy. The midpoint 0.58 reflects that coordination and rent extraction are structurally intertwined. Suppression (0.68): High. Suppression reflects both technical barriers (no equivalent low-cost alternatives from non-Chinese manufacturers; switching costs are 2-4x for comparable platforms) and political barriers (small operators have zero recourse; waivers are discretionary and limited to federal/critical infrastructure; no legislative override mechanism readily available). Theater ratio (0.52): Moderate. The 'covered list' mechanism is procedurally clear and administratively neat, but the threat justification is partially theatrical. Public threat assessments are classified; the ban's scope (commercial drones in civilian hands over non-critical infrastructure) exceeds what would be justified by a narrowly tailored espionage defense. The theater reflects the gap between the stated security rationale and the observed protectionist outcome.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates perspectival disagreement across four classification types: (1) Snare from the powerless small operator's view (trapped, no exit, pure extraction). (2) Tangled Rope from moderate-power agricultural/construction sectors (they experience both coordination benefits and extraction costs). (3) Rope from the defense industrial base (pure coordination from their structural position; they perceive no extraction, only security benefit). (4) Scaffold from the organized industry association view (temporary supply-chain consolidation with implicit sunset as U.S. alternatives mature). (5) Piton from the civilizational view (Cold War export control legacy narratives maintained through inertia). The perspectival gap emerges because different agents experience different trade-offs: beneficiaries (defense industry) genuinely perceive coordination; victims (small operators) perceive pure extraction. The FCC as enforcer occupies an ambiguous middle — it sees itself as enabling security (Scaffold) while analytically positioned to observe its own regulation as performative (Piton). The constraint is not ambiguous because observers disagree on facts; it is genuinely a Tangled Rope because it structurally combines coordination (supply-chain security) and extraction (market foreclosure) in a single mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   DJI commercial operations: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. No exit options: cannot restructure without U.S. ownership (which would dissolve the platform), cannot appeal ban (FCC authority is delegated by Congress), cannot sell inventory (equipment becomes worthless). Small operators: Victim + trapped → d≈0.93, f(d)≈1.40. Same logic as DJI. Agricultural/construction users: Victim + constrained → d≈0.72, f(d)≈1.08. Can switch to U.S. alternatives, but costs are high enough that switching is constrained (not mobile). Defense industrial base: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Can influence policy, lobby for waivers, shape security narratives; experiences positive extraction (receives benefit without proportional cost). FCC regulatory authority: Beneficiary + arbitrage → d≈0.12, f(d)≈0.05. Gains bureaucratic scope and administrative authority; can grant discretionary waivers; arbitrage access to policy-making process. Commercial industry association: Both beneficiary (consolidated market) and victim (reduced competition/innovation) → d≈0.50, f(d)≈0.65. Balanced; constraining but also beneficial.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate vs atrophy tension is resolved by recognizing that the constraint contains BOTH a genuine coordination mandate (supply-chain security standardization that reduces administrative friction for federal systems) AND an extractive mechanism (market foreclosure that raises prices for non-federal users and eliminates choice). The Tangled Rope classification captures this hybrid. Mandatrophy would arise if the constraint CLAIMED to be pure coordination (Rope) but STRUCTURALLY operated as extraction (Snare). The FCC's framing navigates this: it claims the ban is a security necessity, but does NOT claim that the ban involves no economic harm or that harm is incidental. The constraint avoids false naturalization (Mountain claim) because the ban is clearly legislative/administrative, not law of nature. The risk of mandatrophy is on the enforcement side: if FCC or Congress begins claiming the ban is 'necessary for national security' without acknowledging the protectionist benefit (which benefits domestic manufacturers), the constraint would risk misclassification. Currently, the constraint's classification is honest to its structure: Tangled Rope (both coordination and extraction present). The scaffold perspective (FCC regulatory view) risks slipping toward false sunset — claiming the ban is temporary when the beneficiary industries have no incentive to let alternatives mature. This would be mandatrophy via the piton direction: maintaining extraction through institutional inertia while claiming temporary coordination. Ongoing monitoring of U.S. drone manufacturer cost curves is essential to validate the scaffold perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_espionage_risk_magnitude,
    'What is the true magnitude of espionage/data-collection risk from DJI platforms over critical U.S. infrastructure, and how does it compare to risks from other Chinese technology vectors (telecom, cloud, semiconductors)?',
    'Declassified threat assessments from NSA/CISA; comparative analysis of data flows and access patterns across Chinese technology supply chains; forensic evidence of DJI-specific exfiltration vs generic telecom surveillance',
    'If risk > critical and asymmetric to DJI: ban is genuine security measure (Rope from defense perspective). If risk = moderate or symmetric to other vectors: ban is selective protectionism (Snare from operator perspective confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actual_espionage_risk_magnitude, empirical, 'Magnitude of DJI-specific espionage risk versus other Chinese technology vectors').

omega_variable(
    alternative_platform_viability,
    'Can U.S.-based drone manufacturers achieve price-performance parity with DJI within 3-5 years, or will the market sustain a permanent cost premium?',
    'Comparative pricing of equivalent U.S. platforms (Skydio, Auteryon, Freefly) vs DJI legacy; market share tracking; R&D investment and production scaling data',
    'If parity achieved: scaffold sunset becomes real; ban transitions from extraction to temporary coordination. If permanent premium persists: ban maintains asymmetric extraction indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether U.S. drone manufacturers can achieve DJI price-performance parity').

omega_variable(
    regulatory_scope_creep_trajectory,
    'Will the ''covered list'' mechanism expand to other DJI products (ground systems, robotics, cloud services) or establish precedent for banning other Chinese platforms (Huawei, ByteDance, etc.) beyond current scope?',
    'Legislative history and FCC rulemaking trends; congressional testimony from regulatory expansionists; comparative regulatory action in allied nations (EU, Japan, Australia)',
    'If scope expands: ban becomes systemic economic barrier (Snare intensification). If confined to drones: extraction remains bounded (Tangled Rope stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_scope_creep_trajectory, empirical, 'Trajectory of regulatory scope expansion from drone-specific to ecosystem-wide').

omega_variable(
    dji_domestic_operations_continuation,
    'Will DJI exit U.S. market entirely, establish domestic subsidiary with U.S. ownership/control, or litigate the ban on commerce clause grounds?',
    'DJI public statements and SEC filings; litigation docket tracking; corporate restructuring announcements; market share data for successor entities',
    'If exit: extraction ends (constraint resolves). If domestic subsidiary: constraint becomes quasi-zombie (Piton). If litigation succeeds: ban collapses (Snare reversal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dji_domestic_operations_continuation, empirical, 'DJI''s strategic response: exit, restructure, or litigate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fcc_dji_covered_list, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcc_dji_tr_t0, fcc_dji_covered_list, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fcc_dji_tr_t2, fcc_dji_covered_list, theater_ratio, 2, 0.45).
narrative_ontology:measurement(fcc_dji_tr_t5, fcc_dji_covered_list, theater_ratio, 5, 0.52).

% Extraction over time
narrative_ontology:measurement(fcc_dji_be_t0, fcc_dji_covered_list, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fcc_dji_be_t2, fcc_dji_covered_list, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(fcc_dji_be_t5, fcc_dji_covered_list, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fcc_dji_covered_list, enforcement_mechanism).
narrative_ontology:affects_constraint(fcc_dji_covered_list, supply_chain_security_standardization).
narrative_ontology:affects_constraint(fcc_dji_covered_list, technology_nationalism_geopolitical_extraction).
narrative_ontology:affects_constraint(fcc_dji_covered_list, domestic_drone_manufacturer_price_floor).

% DUAL FORMULATION NOTE:
% The FCC DJI ban decomposes into two structurally distinct constraints: (1) supply_chain_security_standardization (ε≈0.25, Rope/Mountain) — the genuine coordination problem of reducing counter-intelligence burden for federal systems through unified supply-chain standards; (2) market_protection_rent_extraction (ε≈0.68, Snare/Tangled Rope) — the protectionist mechanism that benefits domestic manufacturers through geographic market foreclosure. The present constraint (ε=0.58) represents their structural entanglement: the ban achieves coordination function BY MEANS OF extraction mechanism, not despite it. Both downstream constraints are affected: supply-chain standardization enables price floors for domestic manufacturers; technology nationalism rhetoric justifies the extraction. Upstream: the Cold War export control legacy (piton_export_control_narrative, ε≈0.15) provides the institutional scaffolding and legitimacy for the ban's invocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fcc_dji_covered_list, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
