% ============================================================================
% CONSTRAINT STORY: third_country_partnership_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_third_country_partnership_extraction, []).

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
 *   constraint_id: third_country_partnership_extraction
 *   human_readable: Third Country Partnership Extraction in Trade and Investment Networks
 *   domain: economic/political/trade
 *
 * SUMMARY:
 *   Third country partnership extraction occurs when developed-economy
 *   investors and multinational corporations establish commercial
 *   relationships in developing countries through bilateral or trilateral
 *   agreements that ostensibly coordinate capital investment, technology
 *   transfer, and economic development. In practice, these partnerships
 *   frequently generate asymmetric extraction: external investors capture
 *   returns on capital while third country populations bear labor
 *   suppression, environmental externalization, and regulatory constraint.
 *   The constraint is enforced through investor protections (ISDS mechanisms,
 *   capital mobility rights) that prioritize external capital returns over
 *   host-country labor standards, environmental governance, and democratic
 *   autonomy. The partnership framework appears as legitimate coordination
 *   from the investor perspective and as pure extraction from the third
 *   country worker and environment perspectives. Over the 15-year interval,
 *   extractiveness has increased from 0.35 to 0.62 as partnerships have
 *   accumulated extractive provisions (stricter investor protections, broader
 *   regulatory carve-outs) while theater has risen from 0.52 to 0.71,
 *   indicating that the development and sustainability performance narrative
 *   has grown more elaborate even as actual coordination function has
 *   degraded. The constraint exhibits all six DR types from different
 *   structural positions.
 *
 * KEY AGENTS:
 *   - Third Country Workers: Primary victims (powerless/trapped) — labor extraction with no exit options; wages suppressed to investor return requirements
 *   - Third Country Environment: Primary victim (powerless/trapped) — environmental costs externalized; no governance mechanism to prevent or price damage
 *   - Developing Country Government: Secondary actor (moderate/constrained) — experiences mixed coordination (infrastructure investment) and extraction (regulatory autonomy subordinated to investor rights)
 *   - Multinational Corporation: Beneficiary-and-coordinator (powerful/mobile) — invests capital and integrates supply chains (genuine coordination) while extracting through wage suppression and environmental externalization (extraction)
 *   - Developed Economy Investor: Primary beneficiary (institutional/arbitrage) — structures partnerships to capture returns; can redirect capital to alternative jurisdictions if returns insufficient
 *   - International Development Institution: Ceremonial validator (institutional/arbitrage) — ostensibly monitors and facilitates development outcomes but lacks enforcement power; performance of development support masks inability to prevent extraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy choices (capital mobility protections, regulatory carve-outs) as immutable market laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(third_country_partnership_extraction, 0.58).
domain_priors:suppression_score(third_country_partnership_extraction, 0.65).
domain_priors:theater_ratio(third_country_partnership_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(third_country_partnership_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(third_country_partnership_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(third_country_partnership_extraction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(third_country_partnership_extraction, snare).
narrative_ontology:human_readable(third_country_partnership_extraction, "Third Country Partnership Extraction in Trade and Investment Networks").
narrative_ontology:topic_domain(third_country_partnership_extraction, "economic/political/trade").

domain_priors:requires_active_enforcement(third_country_partnership_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(third_country_partnership_extraction, developed_economy_firms).
narrative_ontology:constraint_beneficiary(third_country_partnership_extraction, advanced_capital_exporters).
narrative_ontology:constraint_victim(third_country_partnership_extraction, third_country_labor).
narrative_ontology:constraint_victim(third_country_partnership_extraction, third_country_environment).
narrative_ontology:constraint_victim(third_country_partnership_extraction, developing_economy_governments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THIRD COUNTRY WORKER (SNARE) — Trapped in partnership agreements that channel external capital to local infrastructure but extract labor at suppressed wages. No meaningful exit: local employment alternatives are absent, migration barriers are high, and contract terms are non-negotiable. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(third_country_partnership_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THIRD COUNTRY ENVIRONMENT (SNARE) — Environmental costs (pollution, resource depletion, habitat loss) are externalized into the hosting jurisdiction while capital profits flow to external investors. The constraint is enforced through property rights that prioritize investor returns over local environmental governance. No exit path for ecological commons.
constraint_indexing:constraint_classification(third_country_partnership_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVELOPING COUNTRY GOVERNMENT (TANGLED ROPE) — Experiences mixed effects. Partnership agreements provide infrastructure investment and tax revenue (coordination benefit) but also impose policy constraints (investor protections, regulatory caps) that subordinate local public interest to external capital returns. Exit is costly — rejecting partnerships means foregone infrastructure investment, but accepting them means surrendering regulatory autonomy. Moderate power with genuine constraints on agency.
constraint_indexing:constraint_classification(third_country_partnership_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEVELOPED ECONOMY INVESTOR (ROPE) — Experiences the partnership as coordination: legal frameworks and investor protections enable capital allocation. Arbitrage exit is available — investors can redirect capital to alternative markets if returns are insufficient. Net beneficiary. The constraint appears as a functional coordination mechanism from this vantage.
constraint_indexing:constraint_classification(third_country_partnership_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTINATIONAL CORPORATION (TANGLED ROPE) — Highly mobile (can relocate operations to more favorable jurisdictions) but also genuinely coordinates local supply chain integration and technology transfer. Experiences the constraint as mixed: legitimate coordination benefit (supply chain efficiency) layered with extractive power (wage suppression, environmental externalization). Extraction runs through this agent toward third country workers and environments, but the corporation also invests capital and accepts some operational risk.
constraint_indexing:constraint_classification(third_country_partnership_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL DEVELOPMENT INSTITUTION (PITON) — Ostensibly supports third country development through partnership facilitation and loan guarantees. The functional activity (vetting partnerships, monitoring compliance) is substantially performative — institutions lack enforcement power over multinational behavior and rely on third country governments to regulate, which creates a coordination failure. Theater ratio is high because the performance of development support masks a structural inability to prevent extraction. Piton classification reflects this degradation: the institution was designed to prevent extractive partnerships but has become a ceremonial validator of them.
constraint_indexing:constraint_classification(third_country_partnership_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital mobility and wage arbitrage across borders are immutable features of integrated global markets. The extraction of third country labor and environmental resources is treated as an inherent consequence of market integration, not a contingent institutional choice. However, this perspective naturalizes policy arrangements (capital mobility protections, weak environmental regulation, suppressed labor standards) as laws of economics, which the structural data reveals as institutional choices vulnerable to reformation.
constraint_indexing:constraint_classification(third_country_partnership_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(third_country_partnership_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(third_country_partnership_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(third_country_partnership_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(third_country_partnership_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(third_country_partnership_extraction, TR),
    TR >= 0.70.

:- end_tests(third_country_partnership_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and increasing. Third country partnerships extract labor (wage suppression below alternative-market equilibrium), environmental resources (externalized cleanup costs), and fiscal capacity (foregone tax revenue through special economic zones). The 0.35→0.62 trajectory reflects that extractive provisions have accumulated over time as investors have learned to structure agreements to maximize return certainty. At time 0, partnerships were newer and less optimized for extraction; by time 15, contractual sophistication has increased extraction. Suppression (0.65): High. Third country workers face trapped conditions: employment alternatives in home countries are sparse (rural underemployment, subsistence agriculture); migration barriers to developed economies are high (visa restrictions, language/credential barriers); contract terms are non-negotiable (take-it-or-leave-it, no collective bargaining). Environmental commons has zero bargaining power and no exit. Developing governments face high cost to exit (capital flight, credit downgrades, loss of infrastructure investment). Theater ratio (0.68, increasing): Development institutions, corporate sustainability reporting, and partnership frameworks produce elaborate performance of commitment to labor standards, environmental protection, and knowledge transfer. The gap between stated commitments and enforced outcomes is substantial. ISDS mechanisms are framed as investor protection but function as regulatory extraction enforcement. Sustainability reporting is voluminous but auditing and enforcement are weak.
 *
 * PERSPECTIVAL GAP:
 *   Third country workers experience snare (trapped, no exit, net extraction with no coordination benefit). Developed investors experience rope (beneficiary, exit available, coordination function visible). Developing governments experience tangled rope (infrastructure benefit but regulatory subordination). The gap between worker-snare and investor-rope is maximal (5 classification types apart), revealing that partnership arrangements distribute benefits and costs asymmetrically. Workers see pure extraction; investors see pure coordination. Both are correct from their positions — the asymmetry IS the constraint. Development institutions experience piton (performing development while lacking enforcement capacity) — a degradation from their mandate to prevent extractive arrangements. The analytical observer risks mountain (naturalizing policy choices as economic necessity), which is false — partnerships are restructurable policy arrangements, not laws of physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from beneficiary/victim status and exit options. Third country workers are victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42, maximum experienced extractiveness. Third country environment is victim with zero exit → d = 1.0 → f(d) ≈ 1.50, maximum extraction but no agent power to negotiate. Developing government is victim with constrained exit (high cost but possible) → d ≈ 0.60 → f(d) ≈ 0.80, moderate-high experienced extraction. Multinational is mixed (beneficiary from labor extraction, bear some coordination costs) → d ≈ 0.45 → f(d) ≈ 0.55, moderate experienced extraction. Investor is beneficiary with arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12, negative experienced extraction (constraint subsidizes this agent). Development institution is ceremonial beneficiary (derives legitimacy from association) with arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.03, near-zero experienced extraction. Scope modifier: global partnerships at global scope (σ = 1.2) amplify extraction across all indices.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that classification depends on structural position, not on whether the partnership 'really' is coordination or extraction. From the investor (beneficiary, arbitrage exit), it is coordination (rope). From the worker (victim, trapped exit), it is extraction (snare). From the government (mixed, constrained exit), it is mixed (tangled rope). The mandatrophy is resolved by acknowledging that all classifications are structurally correct from their respective positions. The question is not 'Is third country partnership extraction coordination or pure extraction?' but 'Whose structural position are you analyzing?' The falsifiable claim is that investors genuinely benefit (rope perspective is correct from that position) and workers genuinely suffer net extraction (snare perspective is correct from that position). The piton classification for development institutions reveals that the ceremonial narrative (development support, partnership facilitation, monitoring) masks institutional inability to prevent or sanction extraction. The analytical mountain is a false summit — it naturalizes policy choices as inevitable economics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    investor_protection_vs_host_autonomy,
    'Are investor-state dispute settlement (ISDS) mechanisms genuine protection of legitimate investments or mechanisms for extractive constraint on third country regulatory autonomy?',
    'Analysis of ISDS case outcomes: frequency of claims against labor/environmental regulations vs claims against expropriation; ratio of awards to developing-country governments vs multinational claimants; correlation between ISDS-initiated cases and subsequent regulatory rollback',
    'If genuine protection: partnership structure is legitimate coordination (shifts snare classifications toward tangled rope). If primarily extractive constraint: ISDS is enforcement mechanism for rent extraction (confirms snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(investor_protection_vs_host_autonomy, empirical, 'Whether ISDS protections legitimate partnerships or enforce extraction').

omega_variable(
    wage_suppression_structural_or_competitive,
    'Are suppressed wages in third country partnerships a structural result of unequal bargaining power or a competitive equilibrium in global labor markets?',
    'Comparative wage analysis: third country wages in multinational operations vs domestic non-partnership firms in same sector; wage convergence or divergence over 10-year periods; correlation between partnership entry and local wage growth in comparable regions',
    'If structural inequality: suppression is enforced asymmetry (snare classification confirmed). If competitive equilibrium: suppression reflects scarcity, not constraint (reclassifies toward tangled rope for workers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_structural_or_competitive, empirical, 'Whether wage suppression is structural or competitive').

omega_variable(
    exit_cost_for_developing_governments,
    'How real is the constraint on developing country governments? Can governments reject partnerships or renegotiate terms without catastrophic capital flight or credit downgrades?',
    'Historical case analysis: governments that rejected or renegotiated partnership agreements; outcomes (capital flight magnitude, credit rating impact, alternative funding sources); time to recovery of investment and governance autonomy',
    'If exit costs are catastrophic (>5% of GDP, multi-year sovereign downgrades): developing government classification remains trapped (tangled rope at best). If exit costs are manageable (<2% of GDP, recovery in 2-3 years): exit options upgrade toward constrained or mobile, reducing snare/tangled rope classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_for_developing_governments, empirical, 'Whether developing governments can realistically exit partnerships').

omega_variable(
    technology_transfer_actualization,
    'Do partnership agreements actually deliver promised technology transfer and knowledge spillovers to third country firms, or is technology transfer rhetoric a performance mask for labor and resource extraction?',
    'Longitudinal analysis of third country firm capabilities pre- and post-partnership: patent development, productivity growth, export competitiveness, human capital accumulation; correlation with partnership terms and enforcement records',
    'If genuine transfer: tangled rope classification (real coordination function) confirmed. If nominal or prevented: piton or snare classification (performance mask) confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_actualization, empirical, 'Whether technology transfer occurs or is rhetorical').

omega_variable(
    environmental_externalization_pricing,
    'What are the actual environmental costs (pollution remediation, health impacts, resource depletion) of partnership operations, and are they accurately priced into partnership agreements or externalized?',
    'Environmental accounting: full-cost assessment of partnership operations including external costs; comparison with partnership-stated environmental provisions; post-partnership remediation costs borne by third country governments vs corporate settlement amounts',
    'If externalized: environment is classic snare victim (extraction with no exit, no compensation). If priced in: environmental costs are coordination expenses, not pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(environmental_externalization_pricing, empirical, 'Whether environmental costs are externalized or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(third_country_partnership_extraction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp_tr_t0, third_country_partnership_extraction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(tcp_tr_t5, third_country_partnership_extraction, theater_ratio, 5, 0.6).
narrative_ontology:measurement(tcp_tr_t10, third_country_partnership_extraction, theater_ratio, 10, 0.68).
narrative_ontology:measurement(tcp_tr_t15, third_country_partnership_extraction, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(tcp_be_t0, third_country_partnership_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tcp_be_t5, third_country_partnership_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(tcp_be_t10, third_country_partnership_extraction, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(tcp_be_t15, third_country_partnership_extraction, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(third_country_partnership_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(third_country_partnership_extraction, 0.18).
narrative_ontology:affects_constraint(third_country_partnership_extraction, capital_flight_prevention).
narrative_ontology:affects_constraint(third_country_partnership_extraction, developing_country_debt_sustainability).
narrative_ontology:affects_constraint(third_country_partnership_extraction, labor_standard_harmonization).

% DUAL FORMULATION NOTE:
% Third country partnership extraction is a distinct constraint from its component mechanisms (labor extraction, environmental externalization, regulatory subordination). Each component has its own ε value and classification. This story models the integrated partnership framework; decomposition into labor_extraction (ε ≈ 0.72), environmental_externalization (ε ≈ 0.68), and regulatory_subordination (ε ≈ 0.55) would yield three separate stories linked by network.affects_constraints for deeper analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(third_country_partnership_extraction, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
