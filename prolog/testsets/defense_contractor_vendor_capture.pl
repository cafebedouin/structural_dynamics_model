% ============================================================================
% CONSTRAINT STORY: defense_contractor_vendor_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_defense_contractor_vendor_capture, []).

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
 *   constraint_id: defense_contractor_vendor_capture
 *   human_readable: Defense Contractor Vendor Capture and Supply Chain Lock-in
 *   domain: defense/procurement/industrial_organization
 *
 * SUMMARY:
 *   Defense contractor vendor capture represents a structural constraint
 *   embedded in U.S. military procurement where legitimate supply chain
 *   coordination needs (standardization, security clearances, technical
 *   interoperability, proven reliability, supply continuity) are coupled with
 *   extractive lock-in mechanisms that exclude competitors, inflate prices,
 *   and reduce adaptive capacity. The constraint exhibits both genuine
 *   coordination function (primes and qualified suppliers do solve the real
 *   problem of managing complex military supply chains) and significant
 *   asymmetric extraction (excluded suppliers are locked out, taxpayers bear
 *   inflated costs, military platforms lose flexibility). The system is
 *   maintained through formal institutions (procurement regulations,
 *   qualification standards, security requirements, contract vehicles) that
 *   appear neutral but function as competitive moats. The theater ratio
 *   (0.64) reflects that source selection and competition processes are
 *   partially performative — competition is held but incumbent contractors
 *   have structural advantages; qualifications are maintained but standards
 *   are written to incumbent designs; sources are selected through formal
 *   boards but institutional inertia dominates outcomes. Rising
 *   extractiveness and suppression over the 20-year measurement window (ε:
 *   0.42→0.58, suppression: 0.58→0.68) indicate that the lock-in mechanism
 *   has intensified through post-9/11 security hardening, consolidation of
 *   the contractor base (fewer suppliers after merger waves), and political
 *   capture that blocks reform.
 *
 * KEY AGENTS:
 *   - Prime Defense Contractors (e.g., Lockheed Martin, Boeing, Northrop Grumman): Institutional/arbitrage — primary beneficiaries capturing restricted-competition rents through standardized supply agreements
 *   - Established Tier-1 Suppliers: Institutional/arbitrage — qualified suppliers enjoying steady demand and predictable margins in locked-in customer base
 *   - Excluded Suppliers (SMEs, new entrants, commercial firms): Powerless/trapped — locked out by qualification barriers, security requirements, and prime-controlled subcontracting
 *   - Taxpayers and Citizens: Powerless/trapped — pay inflated prices through defense budgets with no market alternative or exit mechanism
 *   - Military End-Users (Services, Commands): Powerful/constrained — benefit from supply reliability but constrained by switching costs, locked into vendor ecosystems
 *   - Defense Acquisition Bureaucracy (DOD procurement offices, security agencies): Institutional/arbitrage — manage the qualification and competition process; maintain theater of competition; benefits from institutional stability and incumbent supplier relationships
 *   - Defense Reform Coalition (Congressional advocates, GAO, efficiency analysts, good-government groups): Organized/constrained — perceive both coordination needs and extractive lock-in; advocate for competition mandates and open standards but face political barriers
 *   - Analytical Observer: Analytical/analytical — risks naturalizing contingent institutional choices as inevitable security requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(defense_contractor_vendor_capture, 0.58).
domain_priors:suppression_score(defense_contractor_vendor_capture, 0.68).
domain_priors:theater_ratio(defense_contractor_vendor_capture, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(defense_contractor_vendor_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(defense_contractor_vendor_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(defense_contractor_vendor_capture, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(defense_contractor_vendor_capture, tangled_rope).
narrative_ontology:human_readable(defense_contractor_vendor_capture, "Defense Contractor Vendor Capture and Supply Chain Lock-in").
narrative_ontology:topic_domain(defense_contractor_vendor_capture, "defense/procurement/industrial_organization").

domain_priors:requires_active_enforcement(defense_contractor_vendor_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(defense_contractor_vendor_capture, incumbent_prime_contractors).
narrative_ontology:constraint_beneficiary(defense_contractor_vendor_capture, established_tier1_suppliers).
narrative_ontology:constraint_victim(defense_contractor_vendor_capture, competing_suppliers).
narrative_ontology:constraint_victim(defense_contractor_vendor_capture, taxpayers).
narrative_ontology:constraint_victim(defense_contractor_vendor_capture, military_operational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED SUPPLIER (SNARE) — Small and medium suppliers face structural barriers to entry: qualification standards, security clearances, technical specifications written to incumbent designs, minimum production runs that exceed their capacity, and contract vehicles where primes control subcontractor selection. No exit from the procurement system exists if the supplier wants to serve the defense market. Extraction is maximal — the supplier is locked out by design and bears the cost of non-participation or forced acceptance of unfavorable terms.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TAXPAYERS (SNARE) — Locked into funding an inefficiently supplied defense industrial base. No exit mechanism: taxes are mandatory, procurement decisions are opaque, competitive pressure is suppressed. The entire citizenry bears the cost of inflated component pricing, inefficient supplier selection, and technical lock-in. Extraction is maximal from a civilizational time horizon perspective — the cost persists across generations.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARY END-USER (TANGLED ROPE) — Benefits from genuine coordination: standardized supplies, proven reliability, security certification, supply continuity. But constrained by lock-in: once committed to a vendor ecosystem, switching costs are prohibitive (qualification cycles, system redesign, supply chain restart). Mixed extraction and coordination — the military gets the supplies it needs but at higher cost and with less flexibility than genuine competition would provide. Powerful agents but constrained by the system they help maintain.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIME CONTRACTORS (ROPE) — See the constraint as pure coordination: managing qualified suppliers, maintaining security, ensuring supply reliability, standardizing components. The constraint solves genuine collective action problems. Net beneficiary — primes benefit from restricted competition, predictable margins, and customer dependency. Arbitrage exit available: can pivot to commercial markets, can diversify suppliers (but don't). Effective extraction runs away from this agent.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TIER-1 SUPPLIERS (ROPE) — Qualified suppliers experience the system as stable, profitable coordination. Entry is restricted but they are inside the door. Steady demand, predictable margins, long-term contracts. Arbitrage available: suppliers can diversify to commercial markets, can improve efficiency. But no incentive to do so — the defense ecosystem is lucrative specifically because it restricts competition.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM COALITION (TANGLED ROPE) — Congressional advocates, GAO auditors, and efficiency analysts see both genuine supply chain coordination needs AND extractive lock-in. They work to reduce theater (competition mandates), require open standards, establish lowest-price-technically-acceptable procurement, and mandate supplier diversity. But constrained by incumbent political power and classification requirements. Mixed — they perceive the constraint as changeable (reform is possible) but face high structural barriers to exit the current system.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PROCUREMENT BUREAUCRACY (PITON) — The formal acquisition process (source selection, qualification boards, competition oversight) is substantially performative: competition is held but incumbent contractors have structural advantage; sources are selected but decisions reflect institutional inertia; qualifications are maintained but standards are written to incumbent designs. The theater persists through institutional momentum — replacing it would require alternatives (commercial-grade manufacturing, open-source standards, rapid supplier rotation) that are politically and culturally resisted. Theater ratio high (0.64) reflects this degradation.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL / NATURAL LAW (MOUNTAIN) — A narrative claims that defense procurement lock-in is an inherent feature of military supply chain security: you cannot have both reliability AND competition, security clearances AND open entry, rapid adaptation AND standardized interfaces. This perspective naturalizes the constraint as immutable. However, structural data contradicts this — beneficiaries are identifiable, extraction is asymmetric, and alternative procurement models (foreign military sales, commercial dual-use components, modular open standards) demonstrate that the lock-in is contingent, not necessary. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(defense_contractor_vendor_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(defense_contractor_vendor_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(defense_contractor_vendor_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(defense_contractor_vendor_capture, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(defense_contractor_vendor_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(defense_contractor_vendor_capture, TR),
    TR >= 0.70.

:- end_tests(defense_contractor_vendor_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts meaningful rents from excluded competitors and taxpayers through price inflation, reduced competition, and barriers to entry. However, extraction is not maximal because genuine supply chain coordination does occur — primes do manage qualified suppliers effectively, supply reliability is real, and some of the cost premium reflects real security requirements. The 16-point increase over 20 years (0.42→0.58) reflects intensification through post-9/11 security hardening and contractor consolidation. Suppression (0.68): High. Multiple barriers suppress exit and competition: security clearances (months-to-years to obtain), qualification certification (expensive, time-consuming, incumbent advantage), minimum production runs, contract vehicles controlled by primes, technical standards written to incumbent designs, and political capture that blocks reform. Competing suppliers face high barriers; taxpayers face complete barriers (mandatory funding); even the military is constrained by switching costs. Theater ratio (0.64): Moderate-high. Source selection processes, competition boards, and qualification standards create appearance of open competition but function as ritual theater. Competition is held but outcomes are predetermined by incumbent advantages; qualifications are maintained but standards are written narrowly; sources are selected by formal boards but decisions reflect institutional inertia and political relationships. The theater has increased as formal compliance with competition regulations has become more elaborate while de facto competition has decreased through consolidation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark divergence across observer positions. Prime contractors and tier-1 suppliers perceive coordination (Rope) — the system solves genuine supply chain problems. Military end-users perceive mixed coordination and constraint (Tangled Rope) — they benefit from supply reliability but are locked in and constrained. Excluded suppliers and taxpayers perceive extraction (Snare) — trapped with no exit mechanism. The procurement bureaucracy perceives its own degradation (Piton) — the formal process persists through institutional inertia even as stakeholders recognize its dysfunction. The reform coalition perceives a changeable hybrid (Tangled Rope) — both coordination and extraction are real, but reform is possible though constrained. The analytical observer risks seeing this as natural law (Mountain) — claiming that security and competition are inherently incompatible — but the structural data reveals this as a false summit. Peer nations demonstrate that alternative procurement models exist; historical analysis shows lock-in intensified through policy choices, not technical inevitability; and commercial-grade components increasingly meet military requirements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural relationship to the constraint. Prime contractors and tier-1 suppliers: d ≈ 0.15 (clear beneficiaries with arbitrage exit, low effective extraction f(d) ≈ -0.01). Military end-users: d ≈ 0.55 (mixed — both benefit and constrained, moderate effective extraction f(d) ≈ 0.75). Excluded suppliers: d ≈ 0.95 (maximal targets with no exit, high effective extraction f(d) ≈ 1.42). Taxpayers: d ≈ 1.00 (pure targets with mandatory participation, maximal effective extraction). Defense bureaucracy: d ≈ 0.20 (institutional actor managing the constraint, low effective extraction despite institutional power). Reform coalition: d ≈ 0.70 (organized victims/advocates, significant experienced extraction). The perspectival gap between beneficiaries and targets is large — beneficiaries experience the system as low-extraction coordination, while targets experience high-extraction lock-in. This gap is the signature of extractive constraint failure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope is the correct classification when both genuine coordination and asymmetric extraction coexist. The false summit perspective (natural law framing of security vs. competition incompatibility) is detected by the beneficiary declarations and omega variables. Security requirements are real (supporting the coordination function) but are not inherently incompatible with competition (supporting the extractive lock-in detection). The reform coalition's Tangled Rope classification confirms this — they perceive both the coordination function and the extraction, and they work to maintain coordination while reducing extraction through procurement reforms (competition mandates, open standards, supplier diversity programs). The constraint is not pure extraction (Snare) because genuine supply chain coordination exists; nor is it pure coordination (Rope) because asymmetric extraction is substantial. Tangled Rope is the stable classification across analytical perspectives, with the piton perspective (performative competition) providing diagnostic evidence of where the coordination function has degraded into theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_versus_competition_tradeoff,
    'Is the observed supply chain lock-in a necessary security requirement, or a contingent institutional choice that uses security rhetoric to justify extraction?',
    'Comparative analysis of defense procurement in peer nations (NATO allies, Australia, Canada, Israel) with varying degrees of supplier concentration; examination of commercial-grade security standards (ISO, NIST) applied to dual-use military components; historical analysis of procurement lock-in before and after 9/11 policy shifts',
    'If security necessity: classification shifts toward Rope (coordination outweighs extraction). If contingent choice: classification remains Tangled Rope/Snare (extraction is intentional, not inevitable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_versus_competition_tradeoff, empirical, 'Whether lock-in is security necessity or policy choice').

omega_variable(
    qualification_standard_capture,
    'Are military qualification standards (MIL-SPEC, NADCAP, etc.) set to genuinely ensure reliability, or written narrowly to exclude competitors?',
    'Analysis of standard-setting process: who proposes specifications, who votes, incumbent supplier representation in standards bodies; comparison of military specs to commercial equivalents for functionally identical components; longitudinal tracking of specifications before/after incumbent supplier technical improvements',
    'If genuinely driven by reliability: beneficiary status of incumbents is overstated (coordination benefit justified). If driven by competitive exclusion: beneficiary capture is confirmed (extraction is primary driver).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualification_standard_capture, empirical, 'Whether qualification standards serve reliability or competitive exclusion').

omega_variable(
    switching_cost_artificialness,
    'How much of the observed switching cost is inherent to military supply chains, and how much is artificially inflated through proprietary designs, incompatible interfaces, and qualification delays?',
    'Case studies of major platform redesigns and supplier switches (F-35 production changes, ship class transitions); measurement of switch costs in modular vs integrated designs; analysis of requalification timelines for functionally equivalent alternative suppliers',
    'If switching costs are inherent: lock-in is partially natural (reduces extraction magnitude). If artificially inflated: switching costs represent pure extraction overhead (increases extraction magnitude and confirms Snare for trapped agents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_artificialness, empirical, 'Whether switching costs are inherent or artificially inflated').

omega_variable(
    reform_political_feasibility,
    'Is the observed supply chain lock-in politically locked in through campaign contributions, lobbying capture, and revolving-door employment, or primarily maintained by technical inertia?',
    'Campaign finance analysis (contractor contributions to defense-relevant politicians and PACs); lobbying spend tracking; revolving-door employment patterns (DOD procurement officials → contractor roles); analysis of reform bill success rates and amendment blocking',
    'If politically captured: the constraint is actively enforced and will persist unless political structures change (Snare or Tangled Rope with high suppression). If technically entrenched: reform is possible through technical alternatives (Piton or Scaffold with sunset potential).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_political_feasibility, empirical, 'Whether lock-in is politically captured or technically entrenched').

omega_variable(
    commercial_sector_viability,
    'Could commercial-grade suppliers (automotive, aerospace, electronics) meet military supply chain requirements with appropriate dual-use standards, or are military-exclusive suppliers genuinely necessary?',
    'Analysis of defense components that already source from commercial suppliers (Boeing commercial avionics in military platforms, commercial semiconductors with hardening, etc.); cost comparison of military-only vs dual-use supply chains; technical requirements analysis for components vs performance standards vs proprietary specifications',
    'If commercial viability: lock-in is policy choice (Tangled Rope/Snare with active reform pathways). If military-exclusive necessity: lock-in reflects genuine technical constraints (shifts toward Rope or justified Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_sector_viability, empirical, 'Whether commercial suppliers could meet military requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(defense_contractor_vendor_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dvcp_tr_t0, defense_contractor_vendor_capture, theater_ratio, 0, 0.54).
narrative_ontology:measurement(dvcp_tr_t10, defense_contractor_vendor_capture, theater_ratio, 10, 0.6).
narrative_ontology:measurement(dvcp_tr_t20, defense_contractor_vendor_capture, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(dvcp_be_t0, defense_contractor_vendor_capture, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dvcp_be_t10, defense_contractor_vendor_capture, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dvcp_be_t20, defense_contractor_vendor_capture, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dvcp_su_t0, defense_contractor_vendor_capture, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(dvcp_su_t10, defense_contractor_vendor_capture, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(dvcp_su_t20, defense_contractor_vendor_capture, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(defense_contractor_vendor_capture, resource_allocation).
narrative_ontology:affects_constraint(defense_contractor_vendor_capture, military_innovation_lock_in).
narrative_ontology:affects_constraint(defense_contractor_vendor_capture, defense_industry_political_capture).
narrative_ontology:affects_constraint(defense_contractor_vendor_capture, taxpayer_fiscal_extraction).

% DUAL FORMULATION NOTE:
% Defense contractor vendor capture is upstream of specific weapons system constraints and defense policy constraints. The vendor capture mechanism affects all downstream defense innovation (lock-in to incumbent design paradigms) and political capture (contractor lobbying power). Network decomposition: the vendor capture constraint is the coordination mechanism; military_innovation_lock_in is the downstream consequence in technical design space; defense_industry_political_capture is the downstream consequence in political economy space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(defense_contractor_vendor_capture, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
